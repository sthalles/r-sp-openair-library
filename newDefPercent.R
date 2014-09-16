## This code identifies new spots of defoliations in year N+1 related to year N.
## Once this new spots are identified, HYSPLIT is ran for the year N and then,
## we identify which of the new areas of defoliation in Year N+1 is coved by
## air trajectories from Year N. Then, we estimate which percentage of the new
## areas of defoliations is coved by these trajectories from Year N

library(rgdal)
library(rgeos)
library(raster)

# Function to convert a polygon coverage (sbw) to points data frame 
# suited for Hysplit.
# The function "rasterize" with the option getCover=TRUE does not seem 
# to pick all the polygons if the raster cell size is too big.
# That's because "For polygons, values are transferred if the polygon 
# covers the center of a raster cell"
# Consequently we have to convert the raster to polygons, use over to 
# find the intersections and save into points
Poly2Hysplit <- function(sbw, resolution=10000, as.points=TRUE) {
   r <- raster(xmn=-80, xmx=-61, ymn=44, ymx=52, ncols=40, nrows=40)
   r <- setValues(r, 1)
   crs1 <- "+proj=aea +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
   grid <- projectRaster(r,crs=crs1,res=resolution)
   grid.poly <- rasterToPolygons(grid)
   
   test_over <- over(grid.poly,sbw)
   grid.poly.over <- grid.poly[!is.na(test_over),]
   
   crs2 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
   
   if(as.points == TRUE){
     grid.poly.over <- gCentroid(grid.poly.over, byid=T)
   }
   
   spTransform(grid.poly.over,CRS(crs2))
}

crs <- "+proj=aea +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# load and change the projection of the defoliation files
def01 <- readOGR("/media/thalles/ADATA UFD1/Defoliation_Polygons/TBE_2012/TBE_2012", "iggmf_me_inv_aer_tbeg_s_12")
def01 <- spTransform(def01, CRS(crs))

def02 <- readOGR("/media/thalles/ADATA UFD1/Defoliation_Polygons/TBE_2013/TBE_2013", "iggmf_me_inv_aer_tbeg_s_13")
def02 <- spTransform(def02, CRS(crs))

# convert the defoliation areas to polygons
def01.poly <- Poly2Hysplit(def01, as.points = FALSE)

# convert the defoliation areas to points
def02.poly <- Poly2Hysplit(def02, as.points = TRUE)

# identify the new spots of defoliation in YEAR N regarding YEAR N-1
# diff <- def02.poly[is.na(over(def02.poly, def01.poly)), ]
diff <- gDifference(def02.poly, def01.poly)

# convert the defoliation areas to points
def02.poly <- Poly2Hysplit(def02, as.points = FALSE)

# Identify which areas have points over it
diff.poly <- def02.poly[ !is.na(over(def02.poly, diff)),]
# plot(diff.poly)

# plot(def02.poly, col="blue")
# plot(def01.poly, add=T, col="red")
# plot(diff, add=T, col="pink")

###########################
# RUN HYSPLIT

library(opentraj)
library(doParallel)

# SETUP VARIABLES
kYear <- 2012
KHeight <- 200
kHours <- 3

# path to meteorological files
# you have to make sure this path is consistent 
# for information on how to get HySplit Meteorological data,
# http://www.arl.noaa.gov/documents/workshop/Spring2011/HYSPLIT_Tutorial.pdf
KMetFiles <- paste0("/home/thalles/Desktop/hysplit/trunk/metfiles/met", kYear, "/", sep="")

KOutFiles <- paste("/home/thalles/OpenAirWD/pheno", kYear, "/", KHeight, "M", kHours, "H/", sep="")

# if this directory does not exist, then, creates it
if(!file.exists(KOutFiles)){
  print("Creating folder")
  dir.create(KOutFiles)
}

# HySplit instalation path
KHySplitPath <- "/home/thalles/Desktop/hysplit/trunk/"

# load the defoliation point file
pheno <- read.csv("/home/thalles/Documents/pheno_input/Pheno2007.csv")

# convert the dates to objects of class Date
pheno$Year.Month.Day <-as.Date(pheno$Year.Month.Day)

# subset the data, in order to get only the points with ID = 1
pointsDf<-split(pheno, pheno$ID)

# get the number of phisical cores availables
cores <- detectCores()
# 
cl <- makeCluster(cores)

registerDoParallel(cl)

start.time<-Sys.time()

traj <- 
  foreach(i=1:length(pointsDf), .packages="opentraj", .combine = rbind) %dopar%
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
           hours = kHours, height = KHeight, hy.path = KHySplitPath, ID = i, dates=dates,
           start.hour = "19:00", end.hour="23:00",
           tz = "EST", clean.files = TRUE) 
}

end.time<-Sys.time()
time.taken<-end.time - start.time 
time.taken

stopCluster(cl)


###################################
# READ THE TRAJECTORIES
###################################
print("Reading HySplit pre-calculated Trajectories")

path <- KOutFiles

# list all files
files <- list.files(path=path)

# remove the file extensions
files <- sub("\\.[[:alnum:]]+$", "", files)

start.time <- Sys.time()

# iterate through all files and open it
# merge all files in one data frame
data.frame <- foreach( file=files, .combine=rbind, .packages='openair' ) %do%
{
  # get the file
  importTraj(site = file, year="", local=path)
  
  # get only the desired dates
  #traj2013<-selectByDate(traj2013, start = "27/07/2013", end = "27/07/2013", hour=19:21)
}

end.time <- Sys.time()
time.taken <- end.time - start.time 
time.taken


############################
# Creating SpatialObjects
############################

crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
spLines <- Df2SpLines(data.frame, crs)

plot(spLines)
plot(diff.poly, add=T, col="white")

# identify which areas have lines crossing it
intersect.poly <- diff.poly [!is.na(over(diff.poly, spLines)), ]

# find the percentage of areas of new defoliation that are being covered 
# by air trajectories 
length(intersect.poly) / length(diff.poly) * 100







