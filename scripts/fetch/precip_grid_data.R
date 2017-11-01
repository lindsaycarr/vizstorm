

fetch.precip_grid_data <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, c("grid"))
  grid <- deps[['grid']]
  sp_grid <- as(grid, "Spatial")
  sp_grid_WGS84 <- sp::spTransform(sp_grid, "+proj=longlat +datum=WGS84")
  
  stencil <- geoknife::simplegeom(sp_grid_WGS84)
  fabric_args <- viz[["webdata_args"]]
  
  fabric <- geoknife::webdata(
    url = fabric_args$url, 
    times = as.POSIXct(fabric_args$times), 
    variables = fabric_args$variables
  )
  
  job <- geoknife::geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
  
  data <- geoknife::result(job, with.units = TRUE)
  saveRDS(data, file = viz[['location']])
}