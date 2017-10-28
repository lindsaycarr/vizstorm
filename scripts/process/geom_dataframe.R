process.geom_dataframe <- function(viz){
  #deps <- readDepends(viz)
  #checkRequired(deps, 'spatial_data')
  
  # if (!is.null(deps[['clip_box']])){
  #   # then clip it
  # }
  
  #geom_dataframe <- deps[['spatial_data']]
  #data_in <- geom_dataframe@data
  data_in <- data.frame(name = c('california','colorado'))
  data_out <- do.call(info_to_data.frame, 
                      args = append(list(data_in = data_in), viz[['eval_args']]))
  
  # then return the geom w/ the new data.frame w/ it
}

#' takes data_in and creates a data.from that has attributes ready for svg
#' 
#' @param data_in a data.frame of information to be used by functions. 
#' nrow of data_in will be the length of data_out
#' @param \dots named arguments that will be added to 
#' 
#' @example{
#' data_in <- data.frame(name = c('california','colorado'))
#' info_to_data.frame(data_in, class=I('states'), mouseover = "sprintf('hovertext(\"%s\", evt)', name)")
#' }
info_to_data.frame <- function(data_in, ...){
  
  dots <- lazyeval::lazy_dots(...)
  element_args <- sapply(dots, FUN = function(dot){
    lazyeval::lazy_eval(dot$expr, data = data_in)
  })
  return(as.data.frame(element_args))
  
  
}