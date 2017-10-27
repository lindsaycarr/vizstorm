#' @title Fetch viz view limits
#' @param viz a vizlab object including a \code{parameter_spatial} parameter input.
#' @details \code{parameter_spatial} must include:
#' /describe{
#'   \item{id}{parameter_spatial}
#'   \item{crs}{valid crs for \pkg{sf}}
#'   \item{bbox}{numeric in xmin, ymin, xmax, ymax order}
#'   \item{height}{figure height inches}
#'   \item{width}{figure width inches}
#'   \item{pointsize}{number of pixels per inch}
#' }
fetch.viewbox_limits <- function(viz = as.viz('viewbox_limits')){

  deps <- readDepends(viz)
  
  checkRequired(deps, "spatial_metadata")
  
  spatial_meta <- deps[["spatial_metadata"]]
    
  bbox_polygon <- bbox_to_polygon(spatial_meta$bbox, 
                            return_crs = spatial_meta$crs)
  
  # if `plot_metadata` isn't used, defaults from plot_viewbox_limits are used: 
  viewbox_args <- append(list(geo = bbox_polygon), deps[["plot_metadata"]])
  viewbox_limits <- do.call(plot_viewbox_limits, args = viewbox_args)
                                
  
  saveRDS(viewbox_limits, viz[['location']])
}

fetchTimestamp.viewbox_limits <- vizlab::alwaysCurrent

#' @title Construct sf polygon box from two corner numeric bbox.
#' @param bbox numeric xmin, ymin, xmax, ymax 
#' @param bbox_crs the crs string of the bbox (epsg:4326; in WGS84 lon,lat by default)
#' @param return_crs if the returned bbox should be in a projection other than bbox_crs, 
#' pass in a crs string compatible with sf::st_transform()
#' 
#' @details 
#' No rearranging is done is currently done for lat/lon values (e.g., if min/max are out of order)
#' This function currently doesn't check that the values are indeed within valid ranges 
#' for lat/long
#' @return an object of class 'bbox' from sf::
#' @example 
#' bbox <- construct_sf_bbox(c(-87, 21, -70, 34), return_crs = "+init=epsg:5070")
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
#' @return a list of limits, named w/ `xlim` and `ylim`

plot_viewbox_limits <- function(geo, ...){
  
  if("sfc" %in% class(geo)) {
    .fun <- svglite::svgstring(..., standalone = F)
    plot(geo)
  } else {
    stop("only tested with sfc objects.")
  }
  usr <- par('usr')
  dev.off()
  xlim = usr[c(1,2)]
  ylim = usr[c(3,4)]
  
  return(list(xlim = xlim, ylim = ylim))
}
