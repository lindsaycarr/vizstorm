#' @title Fetch viz map data states
#' @param viz a vizlab object including a \code{spatial_metadata} parameter input
#' @details 
#' Depends on: \code{spatial_metadata} which must include:
#' /describe{
#'   \item{bbox}{numeric in xmin, ymin, xmax, ymax order}
#'}
#' and optionally: 
#' /describe{
#'   \item{crs}{valid crs for \pkg{sf}}
#' }
fetch.map_data_states <- function(viz = as.viz("map_data_states")){
  
  sm <- spatial_meta(viz)
  
  saveRDS(get_map_data_sf(database = 'state', 
                          crs = sm$crs, 
                          within = sm$bbox_polygon), 
          viz[['location']])
}

fetchTimestamp.map_data_states <- vizlab::alwaysCurrent

spatial_meta <- function(viz) {
  deps <- readDepends(viz)
  checkRequired(deps, "spatial_metadata")
  spatial_meta <- deps[["spatial_metadata"]]
  spatial_meta$bbox_polygon <- bbox_to_polygon(spatial_meta$bbox,
                                               return_crs = spatial_meta$crs)
  
}