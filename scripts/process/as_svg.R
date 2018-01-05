
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

#' sub text and/or define new svg attributes
#' 
#' uses `sub_attribute_text` to create attribute strings
#' 
#' @param dataframe data to use for evaluation
#' @param attributes key value pairs that may or may not contain 
#' mustache {{key}} syntax
#' @return a data.frame with new columns for each name in `attributes`
svg_sub_attributes <- function(dataframe, attributes = NULL){
  
  if (!is.null(attributes)){
    # replace existing attributes w/ these user-specified ones
    # evaluate "mustache" keys w/ geom_df
    attr_names <- names(attributes)
    
    # if mustache keys exist, evaluate with the context of the dataframe variables:
    attrs_subbed <- sapply(attr_names, FUN = function(x) {
      sub_attribute_text(template = attributes[[x]], data = dataframe)
    })
    
    dataframe[attr_names] <- attrs_subbed
  }
  
  return(dataframe)
}

#' convert a simple feature collection into an svg path element's `d` attribute
#' 
#' @param .object a simple feature collection, currently limited to polygons
#' @param xlim min and max x values in the plotting range in the coordinates of `sfc_object`
#' @param ylim min and max y values in the plotting range in the coordinates of `sfc_object`
#' @param \dots additional arguments sent to `plot`
#' @param width the width of the output figure in inches
#' @param height the height of the output figure in inches
#' @param pointsize used in svgstring...fonts?
#' 
#' @return a data.frame with the same data as `.object`, 
#' but with the new attribute(s) added, and the geometry removed
sf_to_path <- function(.object, xlim, ylim, ..., width, height, pointsize = 12){
  
  stopifnot(!is.null(.object))
  stopifnot(packageVersion('svglite') == '1.2.0.9003')
  
  sfc_object <- .object[["geometry"]]
  data_in <- data.frame(.object) %>% select(-geometry)
  data_in$feature_id <- seq_len(nrow(data_in))
  
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
  
  paths <- xml2::xml_child(rendered) %>% xml2::xml_children()
  
  # collapse the multi-d into a single one: 
  data_out <- data.frame(d_raw = xml2::xml_attr(paths, 'd'), 
                         feature_id = feature_ids, 
                         stringsAsFactors = FALSE) %>% 
    group_by(feature_id) %>% 
    summarize(d = paste(d_raw, collapse = ' ')) %>% 
    left_join(data_in) %>% select(-feature_id)
  return(data_out) 
}

#' This function would let us take a point collection w/ text data and 
#' allow us to (for example) place state names on the map programmatically
sf_to_text <- function(.object, xlim, ylim, ..., width, height, pointsize = 12){
  stop('not implemented')
}

sf_to_circle <- function(.object, xlim, ylim, ..., width, height, pointsize = 12){
  stop('not implemented')
}

#' turn spatial data into svg elements w/ attributes
#' 
#' @param viz a viz object, containing both `data` and `clip_box` as depends
#' 
#' @return a data.frame for use in an svg, containing the element name (`.value`), 
#' `d`, and any other attributes added. See details
#' @details 
#' `<path>` assumes spatial data as inputs, which are then converted into `d` 
#' attributes which describe the draw operations for svg paths
#' the `clip_box` is used to figure out the x and y limits for plotting, which 
#' set up the svg coordinate space that is needed to create the `d` attributes
#' 
#' Additional attributes can be added by using the optional `attributes` field 
#' in the viz block. These must be character key value pairs, but can use mustache 
#' text substitution for simple modifications (see `svg_sub_attributes`).
#' 
#' 
#' in the future, maybe this function could also operate on non-spatial data
process.as_svg_path <- function(viz){
  
  checkRequired(viz$depends, c('data', 'clip_box'))
  
  attributes <- viz[['attributes']]
  svg <- as_svg_elements('path', viz)
  
  svg_path_out <- svg[['elements']] %>% 
    svg_sub_attributes(attributes = attributes) %>% 
    select_(.dots = names(attributes)) %>% 
    do(sf_to_path(., xlim = svg$xlim, ylim = svg$ylim, width = svg$width, height = svg$height)) %>% 
    mutate(.value = 'path') %>% select(.value, everything())
  
  
  saveRDS(svg_path_out, viz[['location']])
}

#' turn data into svg elements w/ attributes
#' 
#' @param viz a viz object, containing `data` and optionally `clip_box` as depends
#' 
#' @return a data.frame for use in an svg, containing the element name (`.value`), 
#' and any other attributes added. See details

#' @details 
#' `<use>` assumes the there are attributes which point to re-use of elements 
#' elsewhere in the svg
#' if included, and if the `data` are spatial the `clip_box` removes features outside
#' of the box. 
#' 
#' Additional attributes can be added by using the optional `attributes` field 
#' in the viz block. These must be character key value pairs, but can use mustache 
#' text substitution for simple modifications (see `svg_sub_attributes`).
#' 
process.as_svg_use <- function(viz){
  
  attributes <- viz[['attributes']]
  
  svg_use_out <- as_svg_elements('use', viz)$elements %>% 
    svg_sub_attributes(attributes = attributes) %>% 
    data.frame() %>% 
    select_(.dots = c('.value', names(attributes)))
  
  saveRDS(svg_use_out, file = viz[['location']])
}

#' shared operations that occur for each process.as_svg_{element}
#' 
as_svg_elements <- function(element_name, viz){
  deps <- readDepends(viz)
  box <- deps[['clip_box']]
  
  elements <- clip_sf(deps[['data']], box) %>% 
    mutate(.value = element_name) 
  
  append(list(elements = elements), plot_bounds(box))
}

#' takes output from one or many process.as_svg_{element} calls 
#' and creates a `defs` svg container for it
#' 
#' @return writes an .rds file that contains the xml from `<g>` in a list
#' format that can be converted to xml with xml2::as_xml_document()
#' @details 
#' if `depends` is empty, this still creates an element, it just 
#' doesn't have xml children
#' named `depends` are converted into `g` (group) names within `defs`
process.as_svg_defs <- function(viz){
  deps <- readDepends(viz)
  defs <- xml2::xml_new_root('.x') 
  group <- xml2::xml_add_child(defs, 'defs')
  
  for (g_name in names(deps)){
    g <- xml2::xml_add_child(group, 'g', id = g_name)
    svg_data <- deps[[g_name]]
    for (j in seq_len(nrow(svg_data))){
      do.call(xml2::xml_add_child, append(list(.x = g), svg_data[j, ]))
    }
  }
  saveRDS(xml2::as_list(defs), file = viz[['location']])
}

#' create an xml/svg "group" or `<g>` element for each named dependency
#' 
#' @return writes an .rds file that contains the xml from `<g>` in a list
#' format that can be converted to xml with xml2::as_xml_document()
#' @details doesn't name the id in the returned group 
process.as_svg_g <- function(viz){
  deps <- readDepends(viz)
  g_main <- xml2::xml_new_root('.x') 
  group <- do.call(xml2::xml_add_child, append(list(.x = g_main, .value = 'g'), viz[['attributes']]))
  
  for (g_name in names(deps)){
    g <- xml2::xml_add_child(group, 'g', id = g_name)
    svg_data <- deps[[g_name]]
    for (j in 1:nrow(svg_data)){
      do.call(xml2::xml_add_child, append(list(.x = g), svg_data[j, ]))
    }
  }
  
  saveRDS(xml2::as_list(g_main), file = viz[['location']])
}

#' set up the basic plot par and empty plot
set_svg_plot <- function(xlim, ylim){
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
  plot(NA, 0, xlim = xlim, ylim = ylim, axes = FALSE, frame.plot = FALSE, ann = FALSE)
}


#' get the xlim and ylim of a spatial object, if present. 
#' Otherwise, return NULL
plot_bounds <- function(sfc_object){
  if (is.null(sfc_object)){
    NULL
  } else {
    ppi <- 72
    bounds <- sf::st_bbox(sfc_object)
    svg_viewbox <- attr(sfc_object, 'svg_viewbox')
    margins <- strsplit(svg_viewbox, '[ ]')[[1]] %>% as.numeric()
    if (margins[1] != 0 | margins[2] != 0){
      stop(call. = FALSE, 'the svg viewBox attribute must have xmin and ymin of zero')
    }
    list(xlim = bounds[c('xmin','xmax')], 
         ylim = bounds[c('ymin','ymax')],
         width = margins[3]/ppi,
         height = margins[4]/ppi)
  }
}

#' perform an `st_intersection` clip operation on sf object

clip_sf <- function(sf_object, sf_clip = NULL){
  if (!is.null(sf_clip)){
    sf::st_intersection(sf::st_buffer(sf_object, 0), sf_clip)
  } else {
    sf_object
  }
}
