
library("opentraj")

library("doParallel")

setwd("/home/thalles/Music")

# source("OpenairSPfunctions.R")

########################
# SETUP VARIABLES
kYear <- 2007
KHeight <- 100
KMetFiles <- "/home/thalles/Desktop/hysplit/trunk/metfiles/met2007/"
KOutFiles <- paste("/home/thalles/OpenAirWD/pheno2007/", KHeight, "M/", sep="")
# KPheno <- "/home/thalles/Documents/pheno2007.csv"
KHySplitPath <- "/home/thalles/Desktop/hysplit/trunk/"

# load the defoliation point file
data(pheno2007)

# convert the dates to objects of class Date
pheno2007$Year.Month.Day <-as.Date(pheno2007$Year.Month.Day)

# subset the data, in order to get only the points with ID = 1
pointsDf<-split(pheno2007, pheno2007$ID)

# get the number of phisical cores availables
cores <- detectCores()
# 
cl <- makeCluster(cores)

registerDoParallel(cl)

start.time<-Sys.time()

hy.traj2007 <- foreach(i=1:length(pointsDf), .packages="opentraj", .combine = rbind) %dopar% 
{
  points <- pointsDf[[i]]
  
  # get the point's latitude and longitude
  lat<-points[[2]][1]
  long<-points[[3]][1]
  
  dates <- points$Year.Month.Day
  
  ########################
  output.file.name<-""
  output.file.name<-paste("pheno", "_", as.character(i), "_", sep="")
  
  ProcTraj(lat = lat, lon = long, year = Year, name = output.file.name,
           hour.interval = 1,
           met = KMetFiles, out = KOutFiles, 
           hours = 3, height = KHeight, hy.path = KHySplitPath, ID = i, dates=dates,
           start.hour = "19:00", end.hour="23:00",
           tz="EST", clean.files=T) 
}

end.time<-Sys.time()
time.taken<-end.time - start.time 
time.taken

stopCluster(cl)

crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

hytraj07.lines <- Df2SpLines(hy.traj2007, crs)

hytraj07.linesDf <- Df2SpLinesDf(hytraj07.lines, hy.traj2007)


###################################


###################################
# READ THE TRAJECTORIES
###################################
print("Reading HySplit pre-calculated Trajectories")

# get the number of phisical cores availables
cores <- detectCores()

cl <- makeCluster(cores)

registerDoParallel(cl)

path <- KOutFiles

# list all files
files <- list.files(path=path)

# remove the file extensions
files <- sub("\\.[[:alnum:]]+$", "", files)

start.time <- Sys.time()

# iterate through all files and open it
# merge all files in one data frame
data.frame <- foreach( file=files, .combine=rbind, .packages='openair' ) %dopar%
{
  # get the file
  importTraj(site = file, year="", local=path)
  
  # get only the desired dates
  #traj2013<-selectByDate(traj2013, start = "27/07/2013", end = "27/07/2013", hour=19:21)
}

end.time <- Sys.time()
time.taken <- end.time - start.time 
time.taken

stopCluster(cl)

# if((nrow(pheno2007) * 24 * 4) == nrow(data.frame)){
#   print("OK")
# }else{
#   print("Error!")
# }


###################
# CREATE SP LINES OBJECTS IN PARALLEL
###################

crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

hytraj07.lines <- Df2SpLines(hy.traj2007, crs)

PlotTraj(spLines)

rast <- RasterizeTraj(spLines, reduce=T)

spLinesDf <- Df2SpLinesDf(spLines, data.frame)

# remove(data.frame)

cores<-detectCores()

list.splines <- SplitSpLines( spLines, cores )

# remove(spLines)

# pdf("title.name", paper="USr", height=0, width=0)
# 
# bb <- bbox(spLines)
# 
# PlotBgMap(spLines, xlim=bb[1,], ylim=bb[2,], axes=TRUE)
# 
# plot(spLines, cex=0.3, add=T)
# 
# dev.off()

# load the doMPI package
#library(doMPI)

# # get the number of phisical cores availables
# cores<-detectCores()
# 
# #cl <- startMPIcluster(count=cores)
# 
# #registerDoMPI(cl)
# 
# # add a PID column represening the Porcess ID
# # basically it divides the dataframe's data equally among the cores
# if( (nrow(data.frame) %% cores) == 0 ) 
# {
#     # create a process ID column to identify each process portion
#     data.frame$PID<-rep(1:cores, each=nrow(data.frame)/cores ) 
# } else
#     stop("The number of cores is not a multiple of the data.frame's row number")
# 
# # split the data.frame data based on its PID
# # basically it creates a separate data.frame for each process work on it separetaly
# list.frame<-split(data.frame, data.frame$PID)
# 
# cl <- makeCluster(cores)
# 
# registerDoParallel(cl)
# 
# start.time<-Sys.time()
# 
# list.splines<- foreach(i=1:length(list.frame), .combine='c', .packages="plyr") %dopar% {
#     DF2SpLines(as.data.frame(list.frame[[i]]), crs)
# }
# 
# end.time<-Sys.time()
# time.taken<-end.time - start.time 
# time.taken
# 
# closeCluster(cl)

#stopCluster(cl)



####################
# RASTERIZE
####################
print("Ready to RASTERIZE")
getRasterGrid <- function(sp.lines, xmn=-80, xmx=-61, 
                          ymn=44, ymx=52, ncols=40,
                          nrows=40, resolution=10000)
{
  # create raster object
  rast <- raster(xmn = xmn, 
                 xmx = xmx, 
                 ymn = ymn, 
                 ymx = ymx, 
                 ncols = ncols, 
                 nrows = nrows)
  
  #extent(rast) <- extent(sp.lines) # assigns the min and max latitude and longitude
  
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
# 
# RasterizeTraj <- function(reduce = T)
# {
#   # get the number of phisical cores availables
#   cores<-detectCores()
#   
#   cl <- makeCluster(cores)
#   
#   registerDoParallel(cl)
#   
#   # start.time<-Sys.time()
# 
#   getRasterGrid <- function(sp.lines, xmn=-80, xmx=-61, 
#                             ymn=44, ymx=52, ncols=40,
#                             nrows=40, resolution=10000)
#   {
#     # create raster object
#     rast <- raster(xmn = xmn, 
#                    xmx = xmx, 
#                    ymn = ymn, 
#                    ymx = ymx, 
#                    ncols = ncols, 
#                    nrows = nrows)
#     
#     #extent(rast) <- extent(sp.lines) # assigns the min and max latitude and longitude
#     
#     # set all the grids to NA
#     rast <- setValues(rast, NA)
#     
#     crs1 <- "+proj=aea +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#     rast <- projectRaster(rast, crs = crs1, res = resolution)
#     
#     # get the projection from the sp object
#     crs2 <- proj4string(sp.lines)
#     
#     # Reproject
#     rast <- projectRaster(rast, crs = crs2)
#     
#     rast
#   }
# 
#   rast2 <- foreach(i=list.splines, .combine='c', .packages="raster") %dopar%
#   {
#     # And then ... rasterize it! This creates a grid version 
#     # of your points using the cells of rast, values from the IP field:
#     rast <- getRasterGrid(i)
#     
#     rasterize(i, rast, fun='count', background=0) 
#   }
# 
# #   end.time<-Sys.time()
# #   time.taken<-end.time - start.time 
# #   time.taken
#   
#   stopCluster(cl)
# 
#   if(reduce == T){
#     rast2 <- Reduce("+", rast2)
#     
#     # replace all 0 values per NA
#     rast2[rast2==0] <- NA
#     
#   } else {
#     rast2
#   }
# }

# # create raster object
# rast <- raster(ncols=1, nrows=1)
# 
# extent(rast) <- extent(spLines) # assigns the min and max latitude and longitude
# 
# # set all the grids to NA
# rast <- setValues(rast, NA)
# 
# crs1 <- "+proj=aea +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# rast <- projectRaster(rast,crs=crs1,res=10000)
# 
# # get the projection from the sp object
# crs2 <- proj4string(spLines)
# 
# # Reproject
# rast <- projectRaster(rast,crs=crs2)

# list.rast<-foreach(i=1:8, .combine="c") %do%
# {
#     getRasterGrid(list.splines[[i]])
# }


rast200 <- RasterizeTraj(spLines, reduce=T)

tiff.file.name<-paste("phenoggg00112", kYear, "_", KHeight, "M", sep="")

writeRaster(rast200, tiff.file.name, format="GTiff", overwrite=T)

# clean working space
rm(list = ls(all = TRUE))

# cleanWD("/home/thalles/Desktop/hysplit/trunk/")


###################################
# READ THE TIFF FILE
####################################

# def <- readOGR("/media/thalles/ADATA UFD1/Defoliation_Polygons/TBE_2008/TBE_2008", "iggmf_me_inv_aer_tbeg_s_08")

diff <- readOGR("/home/thalles/Documents/diff1009", "diff-1009")

#####################################

r <- raster("/home/thalles/Desktop/r-sp-openair-library/pheno2009_100M.tif")

#crs <- proj4string(r)

#def <- spTransform(def,CRS(crs))

data(hytraj07.lines)

r <- RasterizeTraj(hytraj07.lines, reduce=T, resolution=10000)

r.max.value <- maxValue(r)

v <- getValues(r)

v <- v / r.max.value

r <- setValues(r, v)

#convert raster object to SparialGridDataFrame Object
r1 <- as(r, "SpatialGridDataFrame")

PlotTrajFreq(r1, background = TRUE, overlay.color = "white", main="Title", pdf=F)

#####################################


data(hytraj07.linesDf)

data <- slot(hytraj07.linesDf, 'data')

subset(data, day==23 & hour==19)

SelectByDate <- function( sp.lines.df, year, month, day, hour)

