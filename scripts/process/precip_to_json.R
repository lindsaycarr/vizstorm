fetchTimestamp.precip_to_json <- vizlab::alwaysCurrent

#' Saves precip data frame json.
#' @description Converts time series data frame to jsonfile.
#' 
#' @param viz a vizlab object including \code{fetch_args}
#' @details 
#' Depends on: \code{precip_data}: a data.frame with _____. 
#'   
process.precip_to_json <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, "precip_data")
  precip_data <- deps[["precip_data"]]
  
  tidy_data <- tidyr::gather(precip_data, key = "cell", value = "precip", 
                             -c(DateTime, variable, statistic, units))
  
  #force into one timestep for now...animation of multiple will come later
  # tidy_data_t <- dplyr::filter(tidy_data, DateTime == "2014-01-02 12:00:00")
  dates <- unique(df_tidy$DateTime)
  data_list <- lapply(dates, function(d) {
    df_tidy %>% filter(DateTime == d)
  })
  names(data_list) <- dates
  jsonlite::write_json(data_list, viz[['location']])
  
  # saveRDS(tidy_data, file = viz[['location']])
  
}
