#' @title Fetch viz view limits
#' @param viz a vizlab object including a \code{spatial_metadata} parameter input, and
#' optionally, a \code{"plot_metadata"} parameter input
#' @details \code{spatial_metadata} must include:
#' /describe{
#'   \item{bbox}{numeric in xmin, ymin, xmax, ymax order}
#'}
#' and optionally: 
#' /describe{
#'   \item{crs}{valid crs for \pkg{sf}}
#' }
#' \code{plot_metadata} is optional, and can include any of:
#'   \item{height}{figure height inches}
#'   \item{width}{figure width inches}
#'   \item{pointsize}{number of pixels per inch}
#' }
fetch.viewbox_limits <- function(viz = as.viz('viewbox_limits')){

  deps <- readDepends(viz)
  
  checkRequired(deps, "spatial_metadata")
  
  spatial_meta <- deps[["spatial_metadata"]]
    
  # if missing, `crs` will be NULL and used the `bbox_to_polygon` 
  # return_crs default of bbox_crs, which will be WGS84
  bbox_polygon <- bbox_to_polygon(spatial_meta$bbox, 
                            return_crs = spatial_meta$crs)
  
  # if `plot_metadata` isn't used, defaults from plot_viewbox_limits are used: 
  viewbox_args <- append(list(geo = bbox_polygon), deps[["plot_metadata"]])
  viewbox_limits <- do.call(plot_viewbox_limits, args = viewbox_args)
                                
  
  saveRDS(viewbox_limits, viz[['location']])
}

fetchTimestamp.viewbox_limits <- vizlab::alwaysCurrent

