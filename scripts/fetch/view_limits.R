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
fetch.view_limits <- function(viz = as.viz('fetch_view_limits')){
  
  deps <- readDepends(viz)
  required <- "parameter_spatial"
  checkRequired(deps, required)
  p_spatial <- deps[["parameter_spatial"]]
  checkRequired(p_spatial, c("bbox", "width", "height", "pointsize"))
    
  bbox_polygon <- bbox_to_polygon(p_spatial$bbox, 
                            return_crs = p_spatial$crs)
  
  view_limits <- plot_view_limits(bbox_polygon,
                               width = p_spatial$width, 
                               height = p_spatial$height, 
                               pointsize = p_spatial$pointsize)
  
  saveRDS(view_limits, viz[['location']])
}

fetchTimestamp.view_limits <- neverCurrent

#' @title Construct sf polygon box from two corner numeric bbox.
#' @param bbox numeric xmin, ymin, xmax, ymax in WGS84 (EPSG:4326) lon,lat. 
#' No rearranging is done!
#' @param return_crs if the returned bbox should be in a projection other than EPSG:4326, 
#' pass in a crs string compatible with sf::st_transform()
#' 
#' @return an object of class 'bbox' from sf::
#' @example 
#' bbox <- construct_sf_bbox(c(-87, 21, -70, 34), return_crs = "+init=epsg:5070")
bbox_to_polygon <- function(bbox, return_crs = NULL) {
  bbox_poly <- sf::st_sfc(sf::st_polygon(list(matrix(c(bbox[c(1,2)], 
                                  bbox[c(1,4)], 
                                  bbox[c(3,4)], 
                                  bbox[c(3,2)], 
                                  bbox[c(1,2)]), 
                                ncol = 2, byrow = T)), 
                    dim = "XY"), 
         crs = "+init=epsg:4326")
  if(!is.null(return_crs)) {
    sf::st_transform(bbox_poly, return_crs)
  } else {
    bbox_poly
  }
}


#' extract the plotting limits from a spatial object, given a sized svg view
#' 
#' @param geo an sf:: sfc object
#' @param ... additional arguments passed to the plotting methods of `sf` (e.g., `expandBB`)
#' @param width the width (in inches) of the svg view
#' @param height the height (in inches) of the svg view
#' @param pointsize number of pixels per inch
#' @param return what limits to return
#' 
#' @return a list or numeric vector, depending on what is specified for `return`

plot_view_limits <- function(geo, ..., width = 10, height = 8, pointsize = 12, return = c('xlim','ylim')){
  
  if("sfc" %in% class(geo)) {
    .fun <- svglite::svgstring(width = width, height = height, pointsize = pointsize, standalone = F)
    suppressWarnings(plot(geo, ...))
  } else {
    stop("only tested with sfc objects.")
  }
  usr <- par('usr')
  dev.off()
  xlim = usr[c(1,2)]
  ylim = usr[c(3,4)]
  
  list.out <- list(xlim = xlim, ylim = ylim)
    
  if (length(return) > 1){
    return(list.out)
  } else {
    return(list.out[[return]])
  }
}
