#' Draw a leaflet map
#' 
#' @export
#' @param x sf table
#' @param what character one or more of 'killed', 'wounded' or 'shot' (default)
#' @return leaflet object
leaflet_mst <- function(x = read_mst(), what = c('killed', 'wounded', 'shot')[3]){
  
  COLS <- RColorBrewer::brewer.pal(3, "Dark2")
  x <- dplyr::mutate(x, shot = .data$killed + .data$wounded)
  
  leaf <- leaflet::leaflet() |>
    leaflet::addTiles()
  
  if ("shot" %in% what){
    leaf <- leaflet::addCircleMarkers(leaf, 
                                      #radius = ~shot,
                                      data = x,
                                      color = COLS[3],
                                      clusterOptions = leaflet::markerClusterOptions())
  } else {
    
    if ("wounded" %in% what){
      leaf <- leaflet::addCircleMarkers(leaf, 
                                        radius = ~wounded,
                                        color = COLS[2],
                                        data = dplyr::filter(x, .data$wounded > 0),
                                        clusterOptions = leaflet::markerClusterOptions())
      
    }
    
    if ("killed" %in% what){
      leaf <- leaflet::addCircleMarkers(leaf, 
                                        radius = ~killed,
                                        color = COLS[1],
                                        data = dplyr::filter(x, .data$killed > 0),
                                        clusterOptions = leaflet::markerClusterOptions())
      
    }
    
  }
  leaf
}