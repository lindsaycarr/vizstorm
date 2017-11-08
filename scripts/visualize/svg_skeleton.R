#' incomplete
visualize.svg_skeleton <- function(viz){
  deps <- readDepends(viz)
  #initialize svg:
  svg_root <- xml_new_root('svg') # and other details...TODO***
  for (group_name in names(deps)){
    group_list <- list(deps[[group_name]]) %>% setNames(group_name)
    # this is not naming correctly...
    svg_xml <- xml2::as_xml_document(group_list)
    # now add to parent document:
    
  }
  browser()
  stop('nothing here...')
}