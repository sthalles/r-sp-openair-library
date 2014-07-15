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
# plotTraj(spLines, crs, axes=TRUE)
# plotTrajFreq(spLinesDF, 20, 20, crs)
# 
# spplot(sp.lines, spLines)

# Input: data frame
# Output: SpatialPointsDataFrame
hySplit2SPDF<-function(traj, crs)
{
    # get the coordinates out of the data.frame
    cc <- traj[7:8]
    
    # reverse the order of the columns from [Lat Long] to [Long Lat]
    cc<-cc[,c(2,1)]
    
    # get all the data (except) the coordinates
    df <- traj[-7:-8]
    
    # create a SpatialPointsDataFrame object
    SPDF <- SpatialPointsDataFrame(coords=cc, data=df)
    
    proj4string(SPDF)<-crs
    SPDF
}


# DF2SLDF = DataFrame to SpatialLinesDataFrame
# Input: SpatialLines, data frame, Projection
# Output: SpatialLinesDataFrame
DF2SLDF<-function( spLines, df, crs )
{
    # get the trajectory lenghth
    # all trajectories have the same length
    max.traj.length<-max(abs(df$hour.inc)) + 1
    
    # create a traj ID column to identify each trajctory uniquely 
    df$ID<-rep(1:(nrow(df)/max.traj.length), each=max.traj.length )
    
    # apply the function to each subgroup of the data frame
    # the dataframe is divided in subgroups of equal IDs (each trajectory has an unique ID)
    # the function just get the fist line of each trajectory and returns a data.frame with that information
    data.list<-ddply( df, .(ID), function(df){ df[1,] }, .inform=TRUE)

    spLinesDataFrame<-SpatialLinesDataFrame(spLines, data=data.list)
    proj4string(spLinesDataFrame)<-crs
    return(spLinesDataFrame)
}


# DF2SL = Data Frame to SpatialLines
# Input: DataFrame
# Output: SpatialLines
DF2SpLines<-function( df, crs, ... )
{   
#     # list to hold elements of type Lines
#     linesList<-list()
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
#     dataFrames<-split(df, df$ID)  
#     
#     start.time<-Sys.time()
#     
#     linesList<-foreach( l=dataFrames, .combine='c', .packages="sp" ) %dopar% 
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

    start.time<-Sys.time()
    
    max.traj.length<-max(abs(df$hour.inc)) + 1
    
    if( nrow(df) %% max.traj.length != 0 )
    {
        stop("The number of rows in the 'df' argument is not a multiple of the length of a individual trajectory" )
    }
    
    # create a traj ID column to identify each trajctory uniquely 
    df$ID<-rep(1:(nrow(df)/max.traj.length), each=max.traj.length )
    
    createLines<-function(df)
    {
        #print(df)
        # get the coordinates out of the data.frame
        cc <- df[7:8]
        
        # reverse the order of the columns from [Lat Long] to [Long Lat]
        cc<-cc[,c(2,1)]
        
        # create a individual line
        line<-Line(cc)
        
        # transfor the line [line] into a Lines object and assign a unique ID
        Lines(line, ID=as.character(df$ID[1]))  
    }
    
    linesList<-dlply( df, .(ID), createLines)
    
    spLines<-SpatialLines(linesList)
    proj4string(spLines)<-crs
    
    end.time<-Sys.time()
    time.taken<-end.time - start.time 
    time.taken
    
    return(spLines)
}


plotBGmap<-function( traj, ... )
{
    # create the projection
    hySplitProj<-CRS(proj4string(traj))
    
    # apply the projection
    canada<-spTransform(canada,hySplitProj)
    
    # plot the canada map
    plot(canada, border="white", col="lightgrey", add=TRUE, ... )
}

# plotTraj: The function plotTraj is designed to plot hySplit trajectories
# with the initial location represented
plotTraj<-function( traj, ... )
{
    #get the old par configuration
    oldpar<-par(no.readonly=TRUE)
    
    # it reduces the margin's size
    par(mar=c(0,0,0,0) + 2.0)
    
    # plot an empty plot with the proper axes
    plot(spLines, axes=TRUE, type="n")
    
    # gets the bounding box of the object
    # this information will help to focus on the trajectory
    bb<-bbox(traj)
    
    plotBGmap(traj, xlim=bb[1,], ylim=bb[2,])
    
    # print the grid
    grid(col="white")
    
    # plot the trajectory lines
    plot(traj, add=TRUE, ...)
    
    # get the coordinates from the trajectory
    cc<-coordinates(traj)
    
    # get the first coordinate point of the trajectory
    origin <- cc[[1]][[1]][1,]
    
    # plot the trajectory initial point over the trajectory line
    points(origin[1], origin[2], ...)
    
    box()
    
    # restore the par configuration
    par(oldpar)
}



# Function responsable for ploting trajectory frequencies from SpatialObjects
# Input: SpatialObject obs. Tested only with SpatialLines 
#         gridX: X Dimenssion of the Grid
#         gridY: Y Dimenssion of the Grid
# output: plot a map of trajectory frequencies
plotTrajFreq<-function( traj, resolution=10000, gridX=1, gridY=1, ... )
{
    pdf("test.pdf")
    
    #get the old par configuration
    oldpar<-par(no.readonly=TRUE)
    
    # it reduces the margin's size
    par(mar=c(0,0,0,0) + 2.0)
    
    # gets the bounding box of the object
    # this information will help to focus on the trajectory
    bb<-bbox(traj)
    
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
    plotBGmap(traj, xlim=bb[1,], ylim=bb[2,])
    
    #bb<-bbox(traj)
    #plotBGmap(traj, xlim=bb[1,], ylim=bb[2,], CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    plot(rast2, legend=TRUE, col=colorRampPalette(c("light green", "yellow", "orange", "red"))(100), alpha=0.8, axes=TRUE, add=TRUE) ## (n))
    
    # restore the par configuration
    par(oldpar)
    
    dev.off()
}

splitSP<-function( sp, by )
{
    size<-length(sp)
    
    if ( by >= size )
        stop("Error!")
    
    spList<-list()
    
    interval <- length(sp) %/% by
    
    # Serial Execution
    count<-1
    
    for(i in seq(from=1, to = length(sp), by=interval)) 
    {
        if( count != by )
        {
            spList<-c(spList, (sp[i:(i+interval-1)]))
            
        }
        else
        {
            spList<-c(spList, (sp[i:length(sp)]))
            break
        }
        count<-count+1
    }
    
    
    spList
}




