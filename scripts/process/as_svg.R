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
  warning('hardcoding width and height values here because am lazy')
  browser()
  rendered <- svglite::xmlSVG(width = width, height = height, pointsize = pointsize, standalone = F, {
    set_svg_plot()
    for (j in seq_len(length(sfc_object))){
      plot(sfc_object[j], ..., xlim = xlim, ylim = ylim, add = ifelse(j == 1, F, T))#instead of sf::plot_sf?
    }
  })
  
  svg.g <- xml2::xml_child(rendered)
  if (xml2::xml_length(svg.g) == length(sfc_object) + 1){
    xml_remove(xml_child(svg.g)) # remove the <rect thing it puts in there>
  } else if (xml2::xml_length(svg.g) != length(sfc_object)){
    message('something might be wrong. Length of svg elements is different than number of features',
            'but ignore this warning for lines.')
    xml_remove(xml2::xml_child(svg.g))
  }
  
  return(svg.g) # removing the <rect/> element...
}

process.as_svg_path <- function(viz){
  svg_path <- as_svg_elements('path', viz)
  
  browser()
  d = sfc_to_d(svg_path$elements$geometry, xlim = svg_path$xlim, ylim = svg_path$ylim)
  data %>% 
    mutate(.value = 'path') %>% 
    mutate(d = sfc_to_d(geometry)) %>% 
    select(-geometry)
  
}

process.as_svg_use <- function(viz){
  
  
  as_svg_elements('use', viz)
  
  cbind(data_out, data)
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
set_svg_plot <- function(){
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
}
