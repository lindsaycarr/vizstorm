#' from a sp (will be sf) object, optionally clip and optionally 
#' mutate data into svg element attributes
#' 
#' @details 
#' This function takes a single required dependency, which is `spatial_data`
#' Presently, `spatial_data` is an `sf` object.
#' if `attributes` are present, they are used to add to or modify the `data.frame` 
#' in `spatial_data`. Supported operations are specifying a string to be repeated, or 
#' a string substitution using information from another column with the "{variable}" 
#' syntaxt. This operation is skipped if `attributes` isn't present.
#' if `clip_box` is present as a dependency, it is used to clip the geometries in 
#' `spatial_data`
#' See `eval_data.frame` function 
process.geom_dataframe <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, 'spatial_data') # spatial data is the only required depends
  
  geom_dataframe <- deps[['spatial_data']]
  
  if (!is.null(deps[['clip_box']])){
    # remove all polygons that are outside the box
    # trim all polgons that overlap the box
    message('process.geom_dataframe clipping is not implemented yet')
  }
  
  attributes <- viz[['attributes']]

  if (!is.null(attributes)){
    geom_dataframe[names(attributes)] <- attributes
    sub_required <- sapply(attributes, grepl, pattern = '\\{(.*?)\\}')
    complex_attrs <- names(sub_required)[sub_required]
    
    subbed_attrs <- sapply(complex_attrs, FUN = function(x) {
      sub_attribute_text(attribute = x, data = geom_dataframe, attributes[[x]])
    })
    
    geom_dataframe[complex_attrs] <- subbed_attrs
    # should we drop data that was in geom_dataframe but not data_out?
  }
  saveRDS(geom_dataframe, file = viz[['location']])
}

# does this work for "hovertext('{ID}-{dog}' evt)"
sub_attribute_text <- function(attribute, data, template){
  sprint_fmt <- gsub(x = template, pattern = '\\{(.*?)\\}', replacement = '%s')
  data_col <- gsub("[\\{\\}]", "", regmatches(template, gregexpr("\\{.*?\\}", template))[[1]])
  stopifnot(length(data_col) <= 1)
  sapply(data[[data_col]], FUN = function(x) sprintf(sprint_fmt, x), USE.NAMES = FALSE)
}