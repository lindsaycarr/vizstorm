#' from a sp (will be sf) object, optionally clip and optionally 
#' mutate data into svg element attributes
#' 
#' @details 
#' This function takes a single required dependency, which is `spatial_data`
#' Presently, `spatial_data` is an `sf` object.
#' if `attributes` are present, they are used to add to or modify the `data.frame`, and 
#' they are the only variables included in the return data (other than `geometry`).
#' in `spatial_data`. Supported operations are specifying a string to be repeated, or 
#' a string substitution using information from another column with the "{{variable}}" 
#' (`mustache`) syntax. This operation is skipped if `attributes` isn't present.
#' if `clip_box` is present as a dependency, it is used to clip the geometries in 
#' `spatial_data`
#' See `sub_attribute_text` function.
process.geom_dataframe <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, 'spatial_data') # spatial data is the only required depends
  
  geom_dataframe <- deps[['spatial_data']]
  
  if (!is.null(deps[['clip_box']])){
    # remove all polygons that are outside the box
    # trim all polgons that overlap the box
    geom_dataframe <- st_intersection(st_buffer(geom_dataframe, 0), deps[['clip_box']])
  }
  
  attributes <- viz[['attributes']]

  if (!is.null(attributes)){
    # replace existing attributes w/ these user-specified ones
    # evaluate "mustache" keys w/ geom_df
    attr_names <- names(attributes)
    
    # if mustache keys exist, evaluate with the context of the geom_dataframe variables:
    attrs_subbed <- sapply(attr_names, FUN = function(x) {
      sub_attribute_text(template = attributes[[x]], data = geom_dataframe)
    })
    
    geom_dataframe[attr_names] <- attrs_subbed
    # dropping data that was in geom_dataframe but not attributes:
    geom_dataframe <- select_(geom_dataframe, .dots = attr_names)
  }
  
  saveRDS(geom_dataframe, file = viz[['location']])
}

#' Sub mustache keys into text
#' 
#' Use mustache templates to sub data.frame values into 
#' strings. 
#' 
#' @param template a `template` per `whisker::whisker.render`: 
#' "character with template text"
#' @param data a data.frame with variables used for whisker rendering
#' @details this function evaluates the render function row-wise. 
#' Multiple keys can be combined in single template (see example). Per 
#' whisker pkg, keys that don't have variables in `data` evaluate to empty 
#' strings (no errors).
#' @example 
#' sub_attribute_text("hovertext('{{ID}}-{{dog}}' evt)", 
#'    data.frame(ID = c('g','f'), dog = c('ralph','cindy')))
#' 
#' @return a character vector of length nrow(data)
sub_attribute_text <- function(template, data){
  sapply(seq_len(nrow(data)), FUN = function(j) {
    whisker::whisker.render(template, data = data[j,])
    }, USE.NAMES = FALSE)
}

sfc_to_d <- function(sfc_object, xlim, ylim, ...){
  stopifnot(packageVersion('svglite') == '1.2.0.9003')
  # do the svg thing here:
  width = 8
  height = 5
  pointsize = 12
  warning('hardcoding width and height values here because am lazy - TODO***')
  
  # hmmm...with sf, each polygon of a multi-poly plots as a separate `d`. 
  # Can get this w/ st_cast(sfc_object[j], "POLYGON")
  # could test for this `"sfc_MULTIPOLYGON" %in% class(sfc_object[j])`
  # use `feature_id` to track which feature each polygon came from
  feature_ids <- c()
  rendered <- svglite::xmlSVG(width = width, height = height, pointsize = pointsize, standalone = F, {
    
    set_svg_plot(xlim, ylim)
    for (j in seq_len(length(sfc_object))){
      for (poly in sf::st_cast(sfc_object[j], "POLYGON")){
        plot(poly, ..., add = TRUE)
        feature_ids <- c(feature_ids, j)
      }
    }
  })
  
  svg.g <- xml2::xml_child(rendered)
  if (tail(feature_ids, 1) != length(sfc_object)){
    message('something might be wrong. Length of svg elements is different than number of features',
            'but ignore this warning for lines.')
  }
  
  # collapse the multi-d into a single one: 
  data_out <- data.frame(d_raw = xml2::xml_attr(xml2::xml_children(svg.g), 'd'), 
                         feature_id = feature_ids, 
                         stringsAsFactors = FALSE) %>% 
    group_by(feature_id) %>% 
    summarize(d = paste(d_raw, collapse = ' ')) %>% .$d
  
  return(data_out) 
}

process.as_svg_path <- function(viz){
  svg <- as_svg_elements('path', viz)
  xlim <- svg$xlim
  ylim <- svg$ylim
  
  svg$elements %>% 
    mutate(d = sfc_to_d(geometry, xlim = xlim, ylim = ylim)) %>% 
    mutate(.value = 'path') %>% 
    data.frame() %>% 
    select(-geometry)
  
}

process.as_svg_use <- function(viz){
  
  stop("not implemented")
  #as_svg_elements('use', viz)
  
  #cbind(data_out, data)
}

plot_bounds <- function(sfc_object){
  if (is.null(sfc_object)){
    NULL
  } else {
    bounds <- sf::st_bbox(sfc_object)
    list(xlim = bounds[c('xmin','xmax')], ylim = bounds[c('ymin','ymax')])
  }
  
}
as_svg_elements <- function(element_name, viz){
  deps <- readDepends(viz)
  box <- deps[['clip_box']]
  elements <- clip_sf(deps[['data']], box) %>% 
    mutate(.value = element_name) %>% 
    select(.value, everything())
  
  append(list(elements = elements), plot_bounds(box))
}

clip_sf <- function(sf_object, sf_clip = NULL){
  if (!is.null(sf_clip)){
    st_intersection(st_buffer(sf_object, 0), sf_clip)
  } else {
    sf_object
  }
}

#' set up the basic plot par for a map
set_svg_plot <- function(xlim, ylim){
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
  plot(NA, 0, xlim = xlim, ylim = ylim, axes = FALSE, frame.plot = FALSE, ann = FALSE)
}
