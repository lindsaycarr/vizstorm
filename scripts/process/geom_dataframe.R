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
#' See `sub_attribute_texte` function.
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
    # dropping data that was in geom_dataframe but not data_out:
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