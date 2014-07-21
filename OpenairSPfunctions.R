if( !require("openair") ) { 
  install.packages("openair") 
  library("openair")
}

if( !require("rgdal") ) { 
  install.packages("rgdal")
  library("rgdal")
}

if( !require("sp") ) { 
  install.packages("sp")
  library("sp")
}

if( !require("raster") ) { 
  install.packages("raster")
  library("raster")
}

if( !require("plyr") ) { 
  install.packages("plyr")
  library("plyr")
}

if( !require("doMC") ) { 
  install.packages("doMC")
  library("doMC")
}

# load the Canada's map

# canada <- readOGR("/home/thalles/Desktop","province")

# source('/home/thalles/OpenAirWD/openAirHySplit.R')
# 
# ########################
# output = "pheno2012"
# 
# rimouski_N<-"48d27'N"
# rimouski_W<-"68d32'W"
# 
# rimouski_north <- char2dms(rimouski_N)
# rimouski_west <- char2dms(rimouski_W)
# 
# rimouski_decimal_coords <- c(as(rimouski_north, "numeric"), as(rimouski_west, "numeric"))
# 
# procTraj(lat = rimouski_decimal_coords[1], lon = rimouski_decimal_coords[2], year = 2013, name = output,
#          start.month=07, start.day=15, end.month=07, end.day=17, hour.interval="1 hour",
#          met = "/home/thalles/Desktop/hysplit/trunk/working/met2013/", out = "/home/thalles/OpenAirWD/", 
#          hours = 3, height = 100, hy.path = "/home/thalles/Desktop/hysplit/trunk/") 
# 
# 
# traj2013 <- importTraj(site = output, year = 2013, local="~/OpenAirWD/")
# 
# 
# crs<-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
# t<-hySplit2SPDF(traj2013, crs)
# 
# 
# spLines<-DF2SpLines(traj2013, crs) # create a SpatialLines object
# spLinesDF<-DF2SLDF(spLines, traj2013, crs) # create a SpatialLinesDataFrame object
# 
# PlotTraj(spLines, crs, axes=TRUE)
# PlotTrajFreq(spLinesDF, 20, 20, crs)
# 
# spplot(sp.lines, spLines)


Df2SpPoints <- function(traj, crs = NA)
{
  # This function converts an object of class data frame, calculated by the 
  # function ProcTraj, into an object of class SpatialPoints.
  #
  # Args:
  #   df: Data Frame Object created by the function ProcTraj.
  #   crs: Object of class."CRS"; holding a valid proj4 string.
  #
  # Results:
  #   The SpatialPointsDataFrame object  
  
  # get the coordinates out of the data.frame
  cc <- traj[7:8]
  
  # reverse the order of the columns from [Lat Long] to [Long Lat]
  cc <- cc[,c(2,1)]
  
  # get all the data (except) the coordinates
  df <- traj[-7:-8]
  
  # create a SpatialPointsDataFrame object
  sp.lines.df <- SpatialPointsDataFrame(coords = cc, data = df, proj4string = crs)

  sp.lines.df
}


Df2SpLinesDF <- function( spLines, df, crs=NA )
{
  # This function converts an object of type data frame, calculated by the 
  # function ProcTraj, into an Object of class SpatialLinesDataFrame.
  #
  # Args:
  #  spLines: Object of class SpatialLines calculated by the function Df2SpLines.
  #  df: Data Frame Object created by the function ProcTraj.
  #  crs: String: Valid projection string. An example would be crs="+proj=longlat +datum=NAD27".
  #  
  # Results:
  #   Returns an object of class SpatialLinesDataFrame.
  
  # get the trajectory lenghth
  # all trajectories have the same length
  max.traj.length <- max(abs(df$hour.inc)) + 1
  
  # create a traj ID column to identify each trajctory uniquely 
  df$ID <- rep(1:(nrow(df) / max.traj.length), each=max.traj.length )
  
  # apply the function to each subgroup of the data frame
  # the dataframe is divided in subgroups of equal IDs 
  #(each trajectory has an unique ID)
  # the function just get the fist line of each trajectory and returns 
  # a data.frame with that information
  data.list <- ddply(df, .(ID), function(df){ df[1,] }, .inform=TRUE)
  
  spLinesDataFrame <- SpatialLinesDataFrame(spLines, data = data.list, proj4string = crs)

  spLinesDataFrame
}



Df2SpLines<-function( df, crs )
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
  #
  #     # list to hold elements of type Lines
  #     lines.list<-list()
  #     
  #     # make a group variable
  #     # get the length of the trajectories
  #     # all trajectories has the same length
  #     max.traj.length<-max(abs(df$hour.inc)) + 1
  #     
  #     # create a traj ID column to identify each trajctory uniquely 
  #     df$ID<-rep(1:(nrow(df)/max.traj.length), each=max.traj.length )
  # 
  #     # split the dataframe's elements based of its ID
  #     # each trajectory has its own ID
  #     list.df<-split(df, df$ID)  
  #     
  #     start.time<-Sys.time()
  #     
  #     lines.list<-foreach( l=list.df, .combine='c', .packages="sp" ) %dopar% 
  #     {
  #         # get the coordinates out of the data.frame
  #         cc <- l[7:8]
  #         
  #         # reverse the order of the columns from [Lat Long] to [Long Lat]
  #         cc<-cc[,c(2,1)]
  #         
  #         # create a individual line
  #         line<-Line(cc)
  #         
  #         # transfor the line [line] into a Lines object and assign an unique ID
  #         Lines(line, ID=as.character(l$ID[1]))  
  #     }
  #     
  #     end.time<-Sys.time()
  #     time.taken<-end.time - start.time 
  #     time.taken
  
  start.time <- Sys.time()
  
  max.traj.length <- max(abs(df$hour.inc)) + 1
  
  if(nrow(df) %% max.traj.length != 0) {
    stop("The number of rows in the 'df' argument is not a multiple of the length of a individual trajectory" )
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
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time 
  time.taken
  
  sp.lines
}


PlotBgMap <- function( traj, ... ) {
  # extract the projection from the sp object
  hySplitProj <- CRS(proj4string(traj))
  
  # apply the projection to the background map
  canada <- spTransform(canada,hySplitProj)
  
  # plot the map
  plot(canada, border="white", col="lightgrey", add=TRUE, ... )
}

PlotTraj <- function( traj, ... ) { 
  # The function PlotTraj is designed to plot hySplit trajectories calculated 
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
  
  # plot an empty plot with the proper axes
  plot(traj, axes=TRUE, type="n")
  
  # gets the bounding box of the object
  # this information will help to focus on the trajectory
  bb <- bbox(traj)
  
  PlotBgMap(traj, xlim=bb[1,], ylim=bb[2,])
  
  # print the grid
  grid(col="white")
  
  # plot the trajectory lines
  plot(traj, add=TRUE, ...)
  
  # get the coordinates from the trajectory
  cc <- coordinates(traj)
  
  # get the first coordinate point of the trajectory
  origin <- cc[[1]][[1]][1,]
  
  # plot the trajectory initial point over the trajectory line
  points(origin[1], origin[2], ...)
  
  box()
  
  # restore the par configuration
  par(oldpar)
}




PlotTrajFreq <- function( traj, resolution=10000, gridX=1, gridY=1, ... )
{
  # Function responsable for ploting trajectory frequencies from SpatialObjects
  #
  # Args:
  #   traj: Data frame calculated by the ProcTraj function
  #   resolution:
  # 
  #
  # Results:
  #   It plots a map of trajectory frequencies
  #
  pdf("test.pdf")
  
  #get the old par configuration
  oldpar <- par(no.readonly=TRUE)
  
  # it reduces the margin's size
  par(mar=c(0,0,0,0) + 2.0)
  
  # gets the bounding box of the object
  # this information will help to focus on the trajectory
  bb <- bbox(traj)
  
  # create raster object
  rast <- raster(ncols=gridX, nrows=gridY)
  
  extent(rast) <- extent(traj) # assigns the min and max latitude and longitude
  
  # set all the grids to NA
  rast <- setValues(rast, NA)
  
  crs1 <- "+proj=aea +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  rast <- projectRaster(rast,crs=crs1,res=resolution)
  
  # get the projection from the sp object
  crs2 <- proj4string(traj)
  
  # Reproject
  rast <- projectRaster(rast,crs=crs2)
  
  # And then ... rasterize it! This creates a grid version 
  # of your points using the cells of rast, values from the IP field:
  rast2 <- rasterize(spLines, rast,  fun='count') 
  
  # plots only the box 
  plot(rast2, legend=FALSE, alpha=0, 
       col=colorRampPalette(c("light green", "yellow", "orange", "red"))(100) )
  
  # plot the map on the background
  PlotBgMap(traj, xlim=bb[1,], ylim=bb[2,])
  
  #bb<-bbox(traj)
  #PlotBgMap(traj, xlim=bb[1,], ylim=bb[2,], CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  plot(rast2, legend=TRUE, col=colorRampPalette(c("light green", "yellow", "orange", "red"))(100), alpha=0.8, axes=TRUE, add=TRUE) ## (n))
  
  # restore the par configuration
  par(oldpar)
  
  dev.off()
}

SplitSpLines <- function( sp.lines, into ) {
  # One Sentence description of the function
  #
  # Args:
  #   sp.lines: An SpLines Object with more than one line.
  #   into: An integer value. into must be less than or equal to  
  #         the length of the sp.lines object.
  #   
  # Returns:
  #   A list containing into Spatial Lines Objects
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




