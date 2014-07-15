library(doParallel)
library(doMPI)

source('/home/thalles/Desktop/OpenairSPfunctions.R')

source('/home/thalles/OpenAirWD/openAirHySplit.R')

########################
# SETUP VARIABLES

YEAR<-2008
HEIGHT<-200
MET.FILES<-"/home/thalles/Desktop/hysplit/trunk/working/met2008/"
OUT.FILES<-paste("/home/thalles/OpenAirWD/pheno2008/", HEIGHT, "M/", sep="")
PHENO<-"/home/thalles/Documents/Pheno2008.csv"
HYSPLIT.PATH = "/home/thalles/Desktop/hysplit/trunk/"

########################


# # read the data
# pheno2013<-read.csv(PHENO)
# 
# # subset the data, in order to get only the points with ID = 1
# pointsDf<-split(pheno2013, pheno2013$ID)
# 
# # get the number of phisical cores availables
# cores<-detectCores()-2
# 
# cl <- makeCluster(cores)
# 
# registerDoParallel(cl)
# 
# #cleanWD("/home/thalles/Desktop/hysplit/trunk/")
# 
# start.time<-Sys.time()
# 
# #length(pointsDf)
# 
# foreach(i=1:length(pointsDf)) %dopar% 
# {
#     points<-as.data.frame(pointsDf[i])
#     
#     # get the point's latitude and longitude
#     lat<-points[[2]][1]
#     long<-points[[3]][1]
#     
#     # get the max and min date
#     start.date<-as.Date(points[[4]][1]) # transform to object of type date
#     end.date<-as.Date(points[[4]][nrow(points)]) # transform to object of type date
#     
#     # extract the start Year, Month, and Day
#     Year <- format(start.date, "%Y") # long format (4 digit year)
#     start.month<-format(start.date, "%m")
#     start.day<-format(start.date, "%d")
#     
#     # extract the end Month, and Day
#     end.month<-format(end.date, "%m")
#     end.day<-format(end.date, "%d")
#     
#     
#     ########################
#     output.file.name<-""
#     output.file.name<-paste("pheno", "_", as.character(i), "_", sep="")
# 
#     #print(output.file.name)
#     
#     procTraj(lat = lat, lon = long, year = Year, name = output.file.name,
#              start.month=start.month, start.day=start.day, end.month=end.month, end.day=end.day, hour.interval="1 hour",
#              met = MET.FILES, out = OUT.FILES, 
#              hours = 3, height = HEIGHT, hy.path = HYSPLIT.PATH, ID=i ) 
#     
# }
# 
# end.time<-Sys.time()
# time.taken<-end.time - start.time 
# time.taken
# 
# stopCluster(cl)

###################################
# READ THE TRAJECTORIES
###################################
print("READING HYSPLIT TRAJECTORIES")

# get the number of phisical cores availables
cores<-detectCores()

cl <- makeCluster(cores)

registerDoParallel(cl)

path<-OUT.FILES

# list all files
files<-list.files(path=path)

# remove the file extensions
files<-sub("\\.[[:alnum:]]+$", "", files)

start.time<-Sys.time()

# iterate through all files and open it
# merge all files in one data frame
data.frame <- foreach( file=files, .combine=rbind, .packages='openair' ) %dopar%
{
    # get the file
    importTraj(site = file, year="", local=path)
    
    # get only the desired dates
    #traj2013<-selectByDate(traj2013, start = "27/07/2013", end = "27/07/2013", hour=19:21)
}

end.time<-Sys.time()
time.taken<-end.time - start.time 
time.taken

stopCluster(cl)

crs<-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
# spLines<-DF2SpLines(data.frame, crs) # create a SpatialLines object
# spLinesDF<-DF2SLDF( spLines, data.frame, crs )
# plotTraj(spLines, col="blue")

# create raster object
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
# 
# # And then ... rasterize it! This creates a grid version 
# # of your points using the cells of rast, values from the IP field:
# rast2 <- rasterize(spLines, rast,  fun='count') 
# 
# tiff.file.name<-paste("pheno", YEAR, "_", HEIGHT, "M", sep="")
# 
# writeRaster(rast2, tiff.file.name, format="GTiff", overwrite=T)

#plotTrajFreq(spLines, 10000)

#library(plotKML)
#plotKML(spLinesDF)



###################
# CREATE SP LINES OBJECTS IN PARALLEL
###################

print("CREATINT SPLINES OBJECTS")

spLines<-DF2SpLines(data.frame, crs)

list.splines<-splitSP( spLines, 8 )

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
getRasterGrid<-function( spLines )
{
    # create raster object
    rast <- raster(xmn=-80, xmx=-61, ymn=44, ymx=52, ncols=40, nrows=40)
    
    #extent(rast) <- extent(spLines) # assigns the min and max latitude and longitude
    
    # set all the grids to NA
    rast <- setValues(rast, NA)
    
    crs1 <- "+proj=aea +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    rast <- projectRaster(rast,crs=crs1,res=10000)
    
    # get the projection from the sp object
    crs2 <- proj4string(spLines)
    
    # Reproject
    rast <- projectRaster(rast,crs=crs2)
    
    rast
}

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

remove(data.frame)
remove(list.frame)

# get the number of phisical cores availables
cores<-detectCores()

#cl <- makeCluster(cores)

#registerDoParallel(cl)

cl <- startMPIcluster(count=cores)

registerDoMPI(cl)

start.time<-Sys.time()

# .combine=function(x, ...) sum(x, na.rm=T)

rast2<-foreach(i=list.splines, .combine='c', .packages="raster") %dopar%
{
    # And then ... rasterize it! This creates a grid version 
    # of your points using the cells of rast, values from the IP field:
    rast<-getRasterGrid(i)
    
    rasterize(i, rast, fun='count', background=0) 

}

end.time<-Sys.time()
time.taken<-end.time - start.time 
time.taken

closeCluster(cl)

#stopCluster(cl)

rast2<-Reduce("+", rast2)

# replace all 0 values per NA
rast2[rast2==0]<-NA

tiff.file.name<-paste("pheno", YEAR, "_", HEIGHT, "M", sep="")

writeRaster(rast2, tiff.file.name, format="GTiff", overwrite=T)

# clean working space
rm(list = ls(all = TRUE))

# cleanWD("/home/thalles/Desktop/hysplit/trunk/")


####################################
# READ THE TIFF FILE
#####################################

#r<-raster("/home/thalles/pheno2007_400M.tif")