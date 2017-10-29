#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 
#' STOP: BROUGHT THIS IN TO IMPLEMENT THE PROCESS.GEOM; REPLACE W/ REAL FUNCTIONS
library(maps)
library(sp)
to_sp <- function(..., proj.string){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  
  return(map.sp.t)
}


fetch.state_geoms <- function(viz = as.viz('state_geoms')){
  
  view <- readDepends(viz)[['spatial_metadata']]
  states <- to_sp('state', proj.string = view[['crs']])
  states.out <- SpatialPolygonsDataFrame(states, data=data.frame(name = names(states), row.names=row.names(states)))
  saveRDS(states.out, viz[['location']])
}
