

fetch.stageiv_precip_gdp <- function(viz){
  # NOAA stage IV precip:
  url <- "https://cida.usgs.gov/thredds/dodsC/stageiv_combined"
  variables <- "Total_precipitation_surface_1_Hour_Accumulation"
  
  deps <- readDepends(viz)
  checkRequired(deps, c("sf_stencil", "time_info"))
  checkRequired(deps[["time_info"]], c("start", "stop"))
  times <- c(deps[["time_info"]]$start, deps[["time_info"]]$stop)
  sf_stencil <- deps[['sf_stencil']]
  sp_stencil <- as(sf_stencil, "Spatial")
  sp_stencil_WGS84 <- sp::spTransform(sp_stencil, "+proj=longlat +datum=WGS84")
  
  stencil <- geoknife::simplegeom(sp_stencil_WGS84)
  
  fabric <- geoknife::webdata(
    url = url,
    times = times, 
    variables = variables
  )
  
  job <- geoknife::geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
  
  data <- geoknife::result(job, with.units = TRUE)
  
  data <- data %>% 
    dplyr::select(-variable, -statistic, -units) %>% 
    tidyr::gather(key = cell, value = precipVal, -DateTime) %>% 
    dplyr::mutate(precip = precipVal/25.4) #convert mm to inches
  
  if(!is.null(viz[["cumulative"]]) && viz[["cumulative"]]) {
    data <- data %>% 
      dplyr::group_by(cell) %>% 
      dplyr::mutate(precip = cumsum(precip)) 
  }
  
  saveRDS(data, file = viz[['location']])
}

fetchTimestamp.stageiv_precip_gdp <- vizlab::alwaysCurrent