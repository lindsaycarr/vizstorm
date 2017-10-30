

fetch.precip_grid_data <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, c("grid"))
  as(deps[['grid']], "Spatial")
}