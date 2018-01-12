fetchTimestamp.precip_to_json <- vizlab::alwaysCurrent

#' Saves precip data frame json.
#' @description Converts time series data frame to jsonfile.
#' 
#' @param viz a vizlab object including \code{fetch_args}
#' @details 
#' Depends on: \code{precip_data}: a data.frame with at least a DateTime column. 
#'   
process.precip_to_json <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, "precip_data")
  precip_data <- deps[["precip_data"]]
  
  # convert to list where each list is a dataframe for a single timestep
  dates <- unique(precip_data$DateTime)
  data_list <- lapply(dates, function(d) {
    precip_data %>% filter(DateTime == d)
  })
  names(data_list) <- dates
  
  jsonlite::write_json(data_list, viz[['location']])
  
}
