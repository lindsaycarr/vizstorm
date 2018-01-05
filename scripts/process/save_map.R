fetchTimestamp.save_map <- vizlab::alwaysCurrent

#' Saves sf data as geojson.
#' @description Converts sf data to sp, then saves as geojson for use in d3.
#' 
#' @param viz a vizlab object including \code{viewbox_limits} and \code{fetch_args}
#' @details 
#' Depends on: \code{map_data}: an sf representation of the x and y 
#' (geographic coordinates) values of map_data shapes (counties, states, countries, etc).
#'   
process.save_map <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, "map_data")
  map_data <- deps[["map_data"]]

  # spatial data needs to be sp to use writeOGR
  # saves empty file if there is not any map features
  if(nrow(map_data) > 0){
    map_data_sp <- as(map_data, "Spatial") 
    rgdal::writeOGR(map_data_sp, viz[['savelocation']], 
                    layer="map_data_sp", driver="GeoJSON")
  } else {
    write.table(data.frame(), viz[["savelocation"]])
  }
  
}
