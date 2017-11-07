

svg_sub_attributes <- function(dataframe, attributes = NULL){
  
  sub_attribute_text <- function(template, data){
    sapply(seq_len(nrow(data)), FUN = function(j) {
      whisker::whisker.render(template, data = data[j,])
    }, USE.NAMES = FALSE)
  }
  if (!is.null(attributes)){
    # replace existing attributes w/ these user-specified ones
    # evaluate "mustache" keys w/ geom_df
    attr_names <- names(attributes)
    
    # if mustache keys exist, evaluate with the context of the geom_dataframe variables:
    attrs_subbed <- sapply(attr_names, FUN = function(x) {
      sub_attribute_text(template = attributes[[x]], data = dataframe)
    })
    
    dataframe[attr_names] <- attrs_subbed
  }
  
  return(dataframe)
}

sfc_to_d <- function(sfc_object, xlim, ylim, ...){
  
  stopifnot(!is.null(sfc_object))
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
  
  checkRequired(viz$depends, c('data', 'clip_box'))
  svg <- as_svg_elements('path', viz)
  xlim <- svg$xlim
  ylim <- svg$ylim
  
  svg_path_out <- svg$elements %>% 
    mutate(d = sfc_to_d(geometry, xlim = xlim, ylim = ylim), .value = 'path') %>% 
    svg_sub_attributes(attributes = attributes) %>% 
    data.frame() %>% 
    select_(.dots = c('.value', names(attributes)))
  
  saveRDS(svg_path_out, viz[['location']])
}

process.as_svg_use <- function(viz){
  
  attributes <- viz[['attributes']]
  
  svg_use_out <- as_svg_elements('use', viz)$elements %>% 
    svg_sub_attributes(attributes = attributes) %>% 
    data.frame() %>% 
    select_(.dots = c('.value', names(attributes)))
  
  saveRDS(svg_use_out, file = viz[['location']])
}

plot_bounds <- function(sfc_object){
  if (is.null(sfc_object)){
    NULL
  } else {
    bounds <- sf::st_bbox(sfc_object)
    list(xlim = bounds[c('xmin','xmax')], ylim = bounds[c('ymin','ymax')])
  }
  
}

clip_sf <- function(sf_object, sf_clip = NULL){
  if (!is.null(sf_clip)){
    st_intersection(st_buffer(sf_object, 0), sf_clip)
  } else {
    sf_object
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

#' set up the basic plot par for a map
set_svg_plot <- function(xlim, ylim){
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
  plot(NA, 0, xlim = xlim, ylim = ylim, axes = FALSE, frame.plot = FALSE, ann = FALSE)
}
