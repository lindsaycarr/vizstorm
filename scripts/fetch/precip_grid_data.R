

fetch.precip_grid_data <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, c("grid"))
  grid <- deps[['grid']]
  sp_grid <- as(grid, "Spatial")
  sp_grid_WGS84 <- sp::spTransform(sp_grid, "+proj=longlat +datum=WGS84")
  
  stencil <- geoknife::simplegeom(sp_grid_WGS84)
  warning('the rest is not implemented')
}