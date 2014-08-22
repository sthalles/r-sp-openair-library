# if( !require("openair") ) { 
#   install.packages("openair") 
#   library("openair")
# }
# 
# if( !require("rgdal") ) { 
#   install.packages("rgdal")
#   library("rgdal")
# }
# 
# if( !require("sp") ) { 
#   install.packages("sp")
#   library("sp")
# }
# 
# if( !require("raster") ) { 
#   install.packages("raster")
#   library("raster")
# }
# 
# if( !require("plyr") ) { 
#   install.packages("plyr")
#   library("plyr")
# }

# Df2SpPoints <- function(traj, crs = NA)
# {
#   # This function converts an object of class data frame, calculated by the 
#   # function ProcTraj, into an object of class SpatialPoints.
#   #
#   # Args:
#   #   df: Data Frame Object created by the function ProcTraj.
#   #   crs: Object of class."CRS"; holding a valid proj4 string.
#   #
#   # Results:
#   #   The SpatialPointsDataFrame object  
#   lapply(c("sp"), require, character.only = TRUE)
#   
#   if( !is.na(crs) ){
#     crs <- CRS(crs)
#   }
#   
#   # get the coordinates out of the data.frame
#   cc <- traj[7:8]
#   
#   # reverse the order of the columns from [Lat Long] to [Long Lat]
#   cc <- cc[,c(2,1)]
#   
#   # get all the data (except) the coordinates
#   df <- traj[-7:-8]
#   
#   # create a SpatialPointsDataFrame object
#   sp.lines.df <- SpatialPointsDataFrame(coords = cc, data = df, proj4string = crs)
#   
#   sp.lines.df
# }


Df2SpLinesDf <- function( spLines, df, add.distance=F, add.azimuth=F )
{
  # This function converts an object of class SpatialLines, calculated by the 
  # function Df2SpLines, into an Object of class SpatialLinesDataFrame.
  #
  # Args:
  #  spLines: Object of class SpatialLines calculated by the function Df2SpLines.
  #  df: Data Frame Object created by the function ProcTraj.
  #  add.distance: Logical: If True, it will calculate and include the distance in meters between the first and last point for every line.
  #  add.azimuth: Logical: If True it will calculate and include the azimuth for every line.
  
  # Results:
  #   Returns an object of class SpatialLinesDataFrame.
  
  # get the trajectory lenghth
  # all trajectories have the same length
  
  # load required packages
  lapply(c("plyr", "maptools", "sp"), require, character.only = TRUE)
  
  max.traj.length <- max(abs(df$hour.inc)) + 1
  
  # create a traj ID column to identify each trajctory uniquely 
  df$ID <- rep(1:(nrow(df) / max.traj.length), each=max.traj.length )
  
  # apply the function to each subgroup of the data frame
  # the dataframe is divided in subgroups of equal IDs 
  #(each trajectory has an unique ID)
  # the function just get the fist line of each trajectory and returns 
  # a data.frame with that information
  data.list <- ddply(df, .(ID), function(df){ df[1,] }, .inform=TRUE)
  
  if(add.distance == T){
    CalcDistance <- function( line ){ 
      # get the coordinates of the point
      cc <- as.data.frame(coordinates(line))
      
      # get the first and last pair of coordinates
      cc <- cc[-c(2,3),]
      
      # calculate the distance between those two points
      dist <- spDists(as.matrix(cc), longlat=TRUE)[1,2]
    }
    
    data.list$distance <- sapply(slot(spLines, "lines"), FUN=CalcDistance)
    data.list$distance <- data.list$distance * 1000
  }
  
  if(add.azimuth==T){
    CalcAzimuth <- function( line ){ 
      # get the coordinates of the point
      cc <- as.data.frame(coordinates(line))
      
      # get the first and last pair of coordinates
      first.p <- as.matrix(cc[1,])
      second.p <-  as.matrix(cc[nrow(cc),])
      
      # calculate the distance between those two points
      gzAzimuth(first.p, second.p)
    }
    
    data.list$azimuth <- sapply(slot(spLines, "lines"), FUN=CalcAzimuth)
  }
  
  spLinesDataFrame <- SpatialLinesDataFrame(spLines, data = data.list)
  
  spLinesDataFrame
}


Df2SpLines <-
  function( df, crs=NA )
  {   
    # This function converts an object of type data frame, calculated by the function
    # ProcTraj, into an object of type Spatial Lines.
    #
    # Args:
    #   df: Data Frame Object created by the function ProcTraj.
    #   crs: String: Valid projection string. An example would be crs= "+proj=longlat +datum=NAD27"
    #
    # Results:
    #  Returns an object of class SpatialLines.
    
    # load required packages
    lapply(c("plyr", "sp"), require, character.only = TRUE)
    
    if( !is.na(crs) ){
      crs <- CRS(crs)
    }
    
    max.traj.length <- max(abs(df$hour.inc)) + 1
    
    if(nrow(df) %% max.traj.length != 0) {
      stop("The number of rows in the 'df' argument is not a multiple of the length of an individual trajectory" )
    }
    
    # create a traj ID column to identify each trajctory uniquely 
    df$ID <- rep(1:(nrow(df) / max.traj.length), each=max.traj.length )
    
    CreateLines <- function(df) {
      # get the coordinates out of the data.frame
      cc <- df[7:8]
      
      # reverse the order of the columns from [Lat Long] to [Long Lat]
      cc <- cc[, c(2, 1)]
      
      # create a individual line
      line <- Line(cc)
      
      # transfor the line [line] into a Lines object and assign a unique ID
      Lines(line, ID=as.character(df$ID[1]))  
    }
    
    lines.list <- dlply( df, .(ID), CreateLines)
    
    sp.lines <- SpatialLines(lines.list, proj4string = crs)
    
    sp.lines
  }



PlotBgMap <- function( traj, ... ) {
  lapply(c("rgdal"), require, character.only = TRUE)
  
  data(canada.map)
  
  # extract the projection from the sp object
  hySplitProj <- CRS(proj4string(traj))
  
  # apply the projection to the background map
  canada <- spTransform(canada.map, hySplitProj)
  
  # plot the map
  plot(canada.map, border="white", col="lightgrey", ... )
}

PlotTraj <- function( traj, ... ) { 
  # This function is designed to plot hySplit Forward and Backward trajectories calculated 
  # by the function ProcTraj
  # 
  # Args:
  #   traj: SpatialLines or SpatialLinesDataFrame or SpatialPoints data frame
  #   ...: Arguments passed to or from methods
  #
  # Results:
  #    A plot with a map in the background background
  
  #get the old par configuration
  oldpar <- par(no.readonly=TRUE)
  
  # it reduces the margin's size
  par(mar = c(0,0,0,0) + 2.0)
  
  # gets the bounding box of the object
  # this information will help to focus on the trajectory
  bb <- bbox(traj)
  
  PlotBgMap(traj, xlim=bb[1,], ylim=bb[2,], axes=TRUE)
  
  # print the grid
  grid(col="white")
  
  # plot the trajectory lines
  plot(traj, add=TRUE, ...)
  
  # get the coordinates from the trajectory
  cc <- coordinates(traj)

  box()
  
  # restore the par configuration
  par(oldpar)
}


PlotTrajFreq <-
  function( spGridDf, background = T, 
            overlay = NA, overlay.color = "white", 
            pdf = F, ... )
{
  # This function is designed to display trajectory frequency map output by
  # the function RasterizeTraj.
  # Since the function RasterizeTraj outputs a RasterLayer object, this Object
  # must be converted to SpatialGridDataDataFrame Object using the 
  # as( rasterObject, "SpatialGridDataFrame" ) for example.
  #
  # Args:
  #   spGridDf: SpatialGridDataFrame Object obtened by the convertion of the 
  #             raster Object output by the RasterizeTraj function.
  #   background: Boolean: Indicates whether or not the Canada background map 
  #               should be displayed.
  #   overlay: SpatialPolygonsDataFrame
  #   overlay.color: String. sets the Poligons' color defined by the overlay
  #                  argument e.g. "blue"
  #   pdf: Boolean. Defined whether or not the output map should be saved 
  #                 in a pdf file
  # Results:
  #   Chart   
  
  if (pdf == T){
    pdf(title.name, paper="USr", height=0, width=0)
  }
  
  #get the old par configuration
  oldpar <- par(no.readonly=TRUE)
  
  # it reduces the margin's size
  par(mar=c(0,0,0,0) + 2.0)
  
  plot.add <- F
  
  # get all extra arguments
  extra.args <- list(...)
  
  # if the argument main was not defined
  if (!"main" %in% names(extra.args)) {
    extra.args$main <- NULL
  }
  
  if(background == T){
    
    bb <- bbox(spGridDf)
    
    PlotBgMap(spGridDf, xlim=bb[1,], ylim=bb[2,], axes=TRUE)
    
    # print the grid
    grid(col="white")
    
    plot.add <- T
  }
  
  #spplot(r1, add=T)
  
  grays <- colorRampPalette(c( "light green", "green", "greenyellow", "yellow", "orange", "orangered", "red"))(10)
  image( spGridDf, col=grays, breaks=(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)), add = plot.add)
  
  legend("topleft", legend=c("0.0 - 0.1", 
                             "0.1 - 0.2",
                             "0.2 - 0.3",
                             "0.3 - 0.4",
                             "0.4 - 0.5",
                             "0.5 - 0.6",
                             "0.6 - 0.7",
                             "0.7 - 0.8",
                             "0.8 - 0.9",
                             "0.9 - 1.0")
         , fill = grays)
  
  do.call(title, extra.args)
  
  if(!missing(overlay)){
    plot(overlay, add = T, col="black", border="black")
  }
  
  # restore the par configuration
  par(oldpar)
  
  if(pdf == T){
    dev.off()
  }
}

SplitSpLines <-
function( sp.lines, into ) {
  # This function divides the sp.lines object into [into] sub sets of Spatial Lines
  # Objects
  #
  # Args:
  #   sp.lines: Object of class SpatialLines calculated by the function Df2SpLines.
  #   into: Number of times that the sp.lines object must be divided
  #   
  # Returns:
  #   A list of size [into] containing Spatial Lines Objects
  lapply(c("sp"), require, character.only = TRUE)
  
  size <- length(sp.lines)
  
  if (size <= 1)
    stop("The length of the Spatial Lines object must be greater than 1")
  
  if (into >= size)
    stop("Error!")
  
  sp.list <- list()
  
  interval <- length(sp.lines) %/% into
  
  # Serial Execution
  count <- 1
  
  for (i in seq(from=1, to=length(sp.lines), by=interval)) {
    if (count != into){
      sp.list <- c(sp.list, (sp.lines[i:(i + interval - 1)]))
    } else {
      sp.list <- c(sp.list, (sp.lines[i:length(sp.lines)]))
      break
    }
    count <- count + 1
  }
  
  sp.list
}

RasterizeTraj <-
  function(spLines, resolution=10000, reduce = T) {
    # This function produces a grid over an specified area and then computes the 
    # frequency of lines that cross the grid cells. 
    # 
    # Args:
    #   spLines: An object of class Spatial Lines created by the function Df2SpLines
    #   reduce: Boolean: If TRUE the result will be reduced to one raster object
    #           if FALSE, this function will return a list of raster Layers. The size of 
    #           the list is equal to the number of available cores in the system
    #   
    # Returns:
    #   An obejct of class RasterLayer
    
    lapply(c("raster"), require, character.only = TRUE)
    
    # get the bounding box of the spLines object
    ext <- extent(spLines)
    
    # split the sp lines object into N set of lines
    # where N is the number of cores available
    cores <- detectCores()
    
    list.splines <- SplitSpLines( spLines, cores )
    
    getRasterGrid <- function(sp.lines, xmn, xmx, 
                              ymn, ymx, ncols=40,
                              nrows=40, resolution=10000, ext=ext)
    {
      # create raster object
      rast <- raster(xmn = xmn, 
                     xmx = xmx, 
                     ymn = ymn, 
                     ymx = ymx, 
                     ncols = ncols, 
                     nrows = nrows)
      
      
      # set all the grids to NA
      rast <- setValues(rast, NA)
      
      crs1 <- "+proj=aea +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      rast <- projectRaster(rast, crs = crs1, res = resolution)
      
      # get the projection from the sp object
      crs2 <- proj4string(sp.lines)
      
      # Reproject
      rast <- projectRaster(rast, crs = crs2)
      
      rast
    }
    
    # get the number of phisical cores availables
    cores<-detectCores()
    
    cl <- makeCluster(cores)
    
    registerDoParallel(cl)
    
    rast2 <- foreach(i=list.splines, .combine='c', .packages="raster") %dopar%
    {
      # And then ... rasterize it! This creates a grid version 
      # of your points using the cells of rast, values from the IP field:
      rast <- getRasterGrid(i, 
                            xmn=xmin(ext),
                            xmx = xmax(ext),
                            ymn = ymin(ext),
                            ymx = ymax(ext),
                            resolution=resolution)
      
      rasterize(i, rast, fun='count', background=0) 
    }
    
    stopCluster(cl)
    
    if(reduce == T){
      rast2 <- Reduce("+", rast2)
      
      # replace all 0 values per NA
      rast2[rast2==0] <- NA
      
    }
    rast2

}
