fetchTimestamp.map_data <- vizlab::alwaysCurrent

#' Gets data for state polygons.
#' @param viz a vizlab object including a \code{spatial_metadata} parameter input
#' @details 
#' Depends on: \code{viewbox_limits}
#' and arguments to maps::map in \code{fetch_args}
fetch.map_data <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, "viewbox_limits")
  viewbox <- deps[["viewbox_limits"]]
  
  viewbox_args <- list(crs=sf::st_crs(viewbox), within = viewbox)
  map_args <- append(viz$fetch_args, viewbox_args)
  map_data <- do.call(get_map_data, args = map_args)
  
  saveRDS(map_data, viz[['location']])
}

