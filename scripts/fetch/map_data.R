fetchTimestamp.map_data_states <- fetchTimestamp.map_data_counties <- vizlab::alwaysCurrent

#' Gets data for state polygons.
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
                          within = sm["bbox_polygon"]), 
          viz[['location']])
  }

#' Gets data for county polygons.
fetch.map_data_counties <- function(viz = as.viz("map_data_counties")){
  
  sm <- spatial_meta(viz)
  
  saveRDS(get_map_data_sf(database = 'county', 
                          crs = sm$crs, 
                          within = sm["bbox_polygon"]), 
          viz[['location']])
}

spatial_meta <- function(viz) {
  deps <- readDepends(viz)
  checkRequired(deps, "spatial_metadata")
  spatial_meta <- deps[["spatial_metadata"]]
  spatial_meta$bbox_polygon <- bbox_to_polygon(spatial_meta$bbox,
                                               return_crs = spatial_meta$crs)
  return(spatial_meta)
}