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
    
  bbox <- construct_sf_bbox(p_spatial$bbox, 
                            return_crs = p_spatial$crs)
  
  view_limits <- get_view_lims(bbox,
                               width = p_spatial$width, 
                               height = p_spatial$height, 
                               pointsize = p_spatial$pointsize)
  
  view_limits <- append(view_limits, 
                        list(width = p_spatial$width, 
                             height = p_spatial$height, 
                             pointsize = p_spatial$pointsize))
  
  saveRDS(view_limits, viz[['location']])
}

fetchTimestamp.fetch_view_limits <- neverCurrent

#' @title Construct sf bbox from two corner numeric form.
#' @param bbox numeric xmin, ymin, xmax, ymax in WGS84 (EPSG:4326) lon,lat. 
#' No rearranging is done!
#' @param return_crs if the returned bbox should be in a projection other than EPSG:4326, 
#' pass in a crs string compatible with sf::st_transform()
#' 
#' @return an object of class 'bbox' from sf::
#' @example 
#' bbox <- construct_sf_bbox(c(-87, 21, -70, 34), return_crs = "+init=epsg:5070")
construct_sf_bbox <- function(bbox, return_crs = NULL) {
  bbox <- sf::st_sfc(sf::st_polygon(list(matrix(c(bbox[c(1,2)], 
                                  bbox[c(1,4)], 
                                  bbox[c(3,4)], 
                                  bbox[c(3,2)], 
                                  bbox[c(1,2)]), 
                                ncol = 2, byrow = T)), 
                    dim = "XY"), 
         crs = "+init=epsg:4326")
  if(!is.null(return_crs)) {
    sf::st_transform(bbox, return_crs)
  } else {
    bbox
  }
}

#' calculate the bounding box of the sp object and return as `SpatialPolygons` object
#' @param sp a spatial object
#' 
#' @return a `SpatialPolygons` object that represents the bounding box of the input `sp`
get_sp_bbox <- function(sp){
  bb <- sp::bbox(sp)
  xs <- bb[c(1, 3)]
  ys <- bb[c(2, 4)]
  proj.string = sp::CRS(sp::proj4string(sp))
  return(as.sp_box(xs, ys, proj.string))
}

#' create a spatial polygon from x and y coordinates
#' 
#' @param xs a numeric vector of length two, containing the min and max of x values
#' @param ys a numeric vector of length two, containing the min and max of y values
#' 
#' @return a `SpatialPolygons` object
as.sp_box <- function(xs, ys, proj.string){
  Sr1 <- sp::Polygon(cbind(c(xs[c(1, 2, 2, 1, 1)]), c(ys[c(1, 1, 2, 2, 1)])))
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  SpP <- sp::SpatialPolygons(list(Srs1), proj4string = proj.string)
  return(SpP)
}

#' extract the plotting limits from a spatial object, given a sized svg view
#' 
#' @param geo an sp:: SpatialPolygons or an sf:: sfc object
#' @param ... additional arguments passed to the plotting methods of `sp` (e.g., `expandBB`)
#' @param width the width (in inches) of the svg view
#' @param height the height (in inches) of the svg view
#' @param pointsize number of pixels per inch
#' @param return what limits to return
#' 
#' @return a list or numeric vector, depending on what is specified for `return`

get_view_lims <- function(geo, ..., width = 10, height = 8, pointsize = 12, return = c('xlim','ylim')){
  # could dispatch on supported classes? meh... probably retire the sp version soon.
  if("SpatialPolygons" %in% class(geo)) {
    bb <- get_sp_bbox(geo)
    # now default plot
    # extract usr, return lims from usr
    .fun <- svglite::svgstring(width = width, height = height, pointsize = pointsize, standalone = F)
    suppressWarnings(sp::plot(bb, ...)) # warning is for expandBB param, if used
  } else if("sfc" %in% class(geo)) {
    .fun <- svglite::svgstring(width = width, height = height, pointsize = pointsize, standalone = F)
    suppressWarnings(plot(geo, ...))
  } else {
    stop("only tested with SpatialPolygons and sfc objects.")
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
