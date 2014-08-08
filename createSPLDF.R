
setwd("/home/thalles/Desktop/r-sp-openair")

########################
# If the required package is not available, download it
if( !require("doParallel", character.only = TRUE)){
  install.packages("doParallel", dependencies = T)
}

# If the required package is not available, download it
if( !require("doMPI", character.only = TRUE)){
  install.packages("doMPI", dependencies = T)
  library("doMPI")
}


source('OpenairSPfunctions.R')

source('openAirHySplit.R')

########################
# SETUP VARIABLES

kYear <- 2008
KHeight <- 100
KMetFiles <- "/home/thalles/Desktop/hysplit/trunk/metfiles/met2013/"
KOutFiles <- paste("/home/thalles/Desktop/r-sp-openair/output/", KHeight, "M/", sep="")
KPheno <- "/home/thalles/Desktop/r-sp-openair/NewDefoliation.csv"
KHySplitPath <- "/home/thalles/Desktop/hysplit/trunk/"
hy.split.exec.dir <- paste(KHySplitPath, "exec/hyts_std", sep="")

# read the data
pheno2013 <- read.csv(KPheno)

# convert the dates to objects of class Date
years <-as.Date(pheno2013$ID, format="%Y")

# make a column with just the years
# these years will be the base to split these points
pheno2013$year <- format(years, "%Y") 

# subset the data by year
# pointsDf <- split(pheno2013, pheno2013$year)

# get the number of phisical cores availables
# cores <- detectCores()
#  
# cl <- makeCluster(cores)
# 
# registerDoParallel(cl)

start.time<-Sys.time()

# length(pointsDf)

lines.df <- data.frame()

foreach(i = 1:nrow(pheno2013)) %do% 
{
  # get the point
  point <- pheno2013[i, ]
  
  # get the point's latitude and longitude
  lat <- point$Latitude
  long <- point$Longitude
  
  # get the point's year
  p.year <- point$year
  
  # set the metereological file based on the year
  KMetFiles <- paste("/home/thalles/Desktop/hysplit/trunk/metfiles/met", p.year, "/", sep="")
  
  start.date <- paste(p.year, "06", "15", sep="/")
  end.date <- paste(p.year, "07", "31", sep="/")
  dates <- seq(as.POSIXct(start.date, "EST"), as.POSIXct(end.date, "EST"), by = "1 day")
  
  # creating data frame line
  # this information will be placed in the SpatialLinesDataFrame object
  # each line will have the following information
  # Point ID, Starting Year, Starting Day, Starting Hour, Altitude, Line object
  
  
  ########################
  output.file.name<-""
  output.file.name<-paste("pheno", "_", as.character(i), "_", sep="")
  
  #print(output.file.name)
  
  ProcTraj(lat = lat, lon = long, year = Year, name = output.file.name,
           hour.interval = 1,
           met = KMetFiles, out = KOutFiles, 
           hours = -3, height = KHeight, hy.path = KHySplitPath, ID = i, dates=dates,
           hy.split.exec.dir = hy.split.exec.dir,
           add.new.column=T, new.column.name="LineID", new.column.value=point$ID ) 
  
}

end.time<-Sys.time()
time.taken<-end.time - start.time 
time.taken

#stopCluster(cl)

###################################

###################################
# READ THE TRAJECTORIES
###################################
print("Reading HySplit pre-calculated Trajectories")

# get the number of phisical cores availables
# cores <- detectCores()
# 
# cl <- makeCluster(cores)
# 
# registerDoParallel(cl)

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

# stopCluster(cl)

crs <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

###################
# CREATE SP LINES OBJECTS IN PARALLEL
###################

print("Creating SpatialLines Object")

spLines <- Df2SpLines(data.frame, crs)

sp.lines.df <- Df2SpLinesDF(spLines, df=data.frame)

# remove undesired columns from the data.list
sp.lines.df@data <- sp.lines.df@data[c("lineid", "year", "day", "hour", "height")]

# save to a shape file
writeOGR(sp.lines.df, dsn="/home/thalles/Desktop/r-sp-openair/", layer="SpatialLinesDf", driver="ESRI Shapefile", overwrite=T)

sp.lines.df2 <- readOGR(dsn="/home/thalles/Desktop/r-sp-openair/", layer="SpatialLinesDf")
##########################################################


cores<-detectCores()-2

list.splines <- SplitSpLines( spLines, cores )


####################
# RASTERIZE
####################

# get the number of phisical cores availables
cores<-detectCores()-2

cl <- makeCluster(cores)

registerDoParallel(cl)

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

stopCluster(cl)

#stopCluster(cl)

rast2<-Reduce("+", rast2)

# replace all 0 values per NA
rast2[rast2==0]<-NA

tiff.file.name<-paste("pheno", kYear, "_", KHeight, "M", sep="")

writeRaster(rast2, tiff.file.name, format="GTiff", overwrite=T)

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

r.max.value <- maxValue(r)

v <- getValues(r)

v <- v / r.max.value

r <- setValues(r, v)

#convert raster object to SparialGridDataFrame Object
r1 <- as(r, "SpatialGridDataFrame")

title.name <- "Diff 2010-2009_100m"

PlotTrajFreq(r1, background = TRUE, overlay = diff, overlay.color = "white", main = title.name, pdf=T)

#####################################

PlotTrajFreq <- function( spGridDf, background = T, 
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
  
  print(extra.args)
  
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










