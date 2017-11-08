#' incomplete
visualize.svg_skeleton <- function(viz){
  deps <- readDepends(viz)
  #initialize svg:
  svg_root <- init.svg()
  
  for (svg_list in deps){
    # convert list to xml
    svg_xml <- xml2::as_xml_document(svg_list)
    # now add to parent document:
    xml_add_child(svg_root, svg_xml)
  }
  
  xml2::write_xml(svg_root, file = viz[['location']])
}

init.svg <- function(..., width = 8, height = 5){
  # fragile, this is coded into svglite:
  ppi <- 72
  vb <- sprintf("%s %s %s %s", 0, 0, width*ppi, height*ppi)
  xml2::xml_new_root('svg', viewBox = vb, preserveAspectRatio="xMidYMid meet", 
                     xmlns="http://www.w3.org/2000/svg", `xmlns:xlink`="http://www.w3.org/1999/xlink", version="1.1" )
}