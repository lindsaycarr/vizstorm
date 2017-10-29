#' from a sp (will be sf) object, optionally clip and optionally 
#' mutate data into svg element attributes
#' 
#' @details 
#' This function takes a single required dependency, which is `spatial_data`
#' Presently, `spatial_data` is Spatial{X}DataFrame, but this will be updated to `sf`
#' if `eval_args` are present, they are used to convert the `data` slot by evaluating
#' simple functions w/ variables in `data` (skipped if `eval_args` isn't present)
#' if `clip_box` is present as a dependency, it is used to clip the geometries in 
#' `spatial_data`
#' See `eval_data.frame` function 
process.geom_dataframe <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, 'spatial_data') # spatial data is the only required depends
  
  geom_dataframe <- deps[['spatial_data']]
  
  if (!is.null(deps[['clip_box']])){
    # remove all polygons that are outside the box
    # trim all polgons that overlap the box
    message('process.geom_dataframe clipping is not implemented yet')
  }
  
  eval_args <- viz[['eval_args']]
  if (!is.null(eval_args)){
    args <- append(
      list(data_in = geom_dataframe@data), 
      eval_args)
    data_out <- do.call(eval_data.frame, args = args)
    
    # then return the geom w/ the new data.frame w/ it
    geom_dataframe@data <- data_out
  }
  
  saveRDS(geom_dataframe, file = viz[['location']])
}

#' takes data_in and creates a data.frame that has attributes ready for svg
#' 
#' @param data_in a data.frame of information to be used by functions. 
#' nrow of data_in will be the length of data_out
#' @param \dots named arguments that can be evaluated with variables in \code{data_in}
#' 
#' @example{
#' data_in <- data.frame(name = c('california','colorado'))
#' eval_data.frame(data_in, class=I('state'), mouseover = "sprintf('hovertext(\"%s\", evt)', name)")
#' }
eval_data.frame <- function(data_in, ...){
  
  dots <- lazyeval::lazy_dots(...)
  element_args <- sapply(dots, FUN = function(dot){
    lazyeval::lazy_eval(dot$expr, data = data_in)
  })
  return(as.data.frame(element_args))
  
  
}