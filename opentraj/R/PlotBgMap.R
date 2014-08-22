PlotBgMap <-
function( traj, ... ) {
  lapply(c("rgdal"), require, character.only = TRUE)
  
  data(canada.map)
  
  # extract the projection from the sp object
  hySplitProj <- CRS(proj4string(traj))
  
  # apply the projection to the background map
  canada <- spTransform(canada.map, hySplitProj)
  
  # plot the map
  plot(canada.map, border="white", col="lightgrey", ... )
}
