#' @title get Map data as sf
#' 
#' @description Gets data from \code{maps::map()} and returns a transformed sf object
#' 
#' @param crs sf package crs string. If included, data will be transformed to the given crs
#' @param within sf polygon. polygons completely outside the polygon will not be returned.
#' Can be retrieved from the \code{bbox_to_polygon()} function.
#' @param ... inputs to maps::map. For example: \code{database = "state"} or 
#' \code{database = "world", region = "Puerto Rico"}.
#' 
#' @return \code{data.frame} of class \code{sf} with geometry and ID columns.
#' 
#' @example 
#' states <- get_map_data(crs="+init=epsg:5070", database="state")
#' puerto_rico <- get_map_data(crs="+init=epsg:5070", database = "world", region = "Puerto Rico")
#' counties <- get_map_data(crs="+init=epsg:3857", database = "county")
#' maps inputs xlim and ylim can limit the response to be within a lat/lon bounding rectangle
#' islands <- get_map_data(crs = "+init=epsg:3857", database = 'mapdata::world2Hires', 
#'                            regions = "(?!USA)", xlim = c(275, 300), ylim = c(16, 30))
#'
#'# A polygon in a particular projection can also be used.
#' bbox <- bbox_to_polygon(c(-87, 21, -70, 34), return_crs = "+init=epsg:3857")
#' storm_counties <- get_map_data(database = "counties", crs = "+init=epsg:3857", within = bbox)
get_map_data <- function(..., crs=NULL, within = NULL){
  
  map_data <- sf::st_as_sf(maps::map(..., fill=TRUE, plot = FALSE))
  if(!is.null(crs)) {
    map_data <- sf::st_transform(map_data, crs)
  }
  if(!is.null(within)) {
    map_data <- tryCatch({
      map_data <- sf::st_intersection(map_data, within)
    }, error = function(e) {
      warning(paste("An error occured when subsetting the source polygons, it was:", e,
                    "trying to clean the geometry and try again. Check result!"))
      return(sf::st_intersection(sf::st_buffer(map_data, 0), within))
    })
  }
  return(map_data)
}

#' @title Construct sf polygon box from two corner numeric bbox.
#' 
#' @param bbox numeric xmin, ymin, xmax, ymax 
#' @param bbox_crs the crs string of the bbox (epsg:4326; in WGS84 lon,lat by default)
#' @param return_crs if the returned bbox should be in a projection other than bbox_crs, 
#' pass in a crs string compatible with sf::st_transform()
#' 
#' @details 
#' No rearranging is done is currently done for lat/lon values (e.g., if min/max are out of order)
#' This function currently doesn't check that the values are indeed within valid ranges 
#' for lat/long
#' 
#' @return an object of class 'bbox' from sf::
#' 
#' @example 
#' bbox <- bbox_to_polygon(c(-87, 21, -70, 34), return_crs = "+init=epsg:5070")
bbox_to_polygon <- function(bbox, bbox_crs = "+init=epsg:4326", return_crs = NULL) {
  bbox_poly <- sf::st_sfc(sf::st_polygon(list(
    matrix(
      bbox[c(1,2, 1,4, 3,4, 3,2, 1,2)], 
      ncol = 2, byrow = TRUE)), 
    dim = "XY"), 
    crs = bbox_crs)
  if(!is.null(return_crs)) {
    sf::st_transform(bbox_poly, return_crs)
  } else {
    bbox_poly
  }
}

#' extract the plotting limits from a spatial object, given a sized svg viewbox
#' 
#' @param geo an sf:: sfc object
#' @param ... additional arguments passed to svglite::svgstring, e.g., width, height
#' @param width the width (in inches) of the svg view
#' @param height the height (in inches) of the svg view
#' @param pointsize number of pixels per inch
#' 
#' @return an sfc polygon of the viewbox limits

plot_viewbox_limits <- function(geo, ...){
  
  if("sfc" %in% class(geo)) {
    .fun <- svglite::svgstring(..., standalone = F)
    plot(geo)
  } else {
    stop("only tested with sfc objects.")
  }
  usr <- par('usr')
  dev.off()
  
  viewbox <- bbox_to_polygon(usr[c(1, 3, 2, 4)], bbox_crs = sf::st_crs(geo))
  
  return(viewbox)
}
