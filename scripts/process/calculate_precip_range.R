fetchTimestamp.calculate_precip_range <- vizlab::alwaysCurrent

#' Takes precip data and calculates the min and max
#' @description Saves min and max precip values as a json file.
#' 
#' @param viz a vizlab object including \code{fetch_args}
#' @details 
#' Depends on: \code{precip_data}: a data.frame with at least a column 
#' of numerical precipitation values named "precip". 
#'   
process.calculate_precip_range <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, "precip_data")
  precip_data <- deps[["precip_data"]]
  
  range_list <- list(
    minPrecip = min(precip_data[["precip"]]),
    maxPrecip =  max(precip_data[["precip"]]),
    breaks = seq(0, ceiling(max(precip_data[["precip"]])), length.out = 9)
  )
  
  jsonlite::write_json(range_list, viz[['location']])
  
}
