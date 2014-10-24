pkgname <- "opentraj"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "opentraj-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('opentraj')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Df2SpLines")
### * Df2SpLines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Df2SpLines
### Title: Data Frame to Spatial Lines
### Aliases: Df2SpLines
### Keywords: Lines Trajectories

### ** Examples

  ## load data frame of HYSPLIT trajectory calculations calculated by function ProcTraj
  crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  air.traj.lines <- Df2SpLines(air.traj, crs)
  PlotTraj(air.traj.lines)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Df2SpLines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Df2SpLinesDf")
### * Df2SpLinesDf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Df2SpLinesDf
### Title: Data Frame to Spatial Lines Data Frame
### Aliases: Df2SpLinesDf
### Keywords: Lines Trajectories

### ** Examples

  crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  air.traj.lines <- Df2SpLines(air.traj, crs)
  air.traj.linesDf <- Df2SpLinesDf(air.traj.lines, air.traj)
  PlotTraj(air.traj.linesDf)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Df2SpLinesDf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PlotTraj")
### * PlotTraj

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlotTraj
### Title: Plot Trajectory
### Aliases: PlotTraj
### Keywords: ~kwd1 ~kwd2

### ** Examples

  crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  air.traj.lines <- Df2SpLines(air.traj, crs)
  PlotTraj(air.traj.lines)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlotTraj", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PlotTrajFreq")
### * PlotTrajFreq

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlotTrajFreq
### Title: Plot Trajectory Frequency
### Aliases: PlotTrajFreq
### Keywords: Air Trajectory Trajectory Frequency

### ** Examples

  library(raster)
    
  crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  air.traj.lines <- Df2SpLines(air.traj, crs)
  raster.lines <- RasterizeTraj(air.traj.lines, reduce=TRUE, resolution=15000, parallel=FALSE)
    
  r.max.value <- maxValue(raster.lines)
  v <- getValues(raster.lines)
  v <- v / r.max.value
  r <- setValues(raster.lines, v)
    
  ## convert raster object to SparialGridDataFrame Object
  r1 <- as(r, "SpatialGridDataFrame")
    
  PlotTrajFreq(r1, background = TRUE, main="Title", pdf=FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlotTrajFreq", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ProcTraj")
### * ProcTraj

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ProcTraj
### Title: Process Trajectory
### Aliases: ProcTraj
### Keywords: Air Trajectory ~kwd2

### ** Examples

##---- For Windows system 


##---- For Unix alike systems 

# library("opentraj")
# library("doParallel")
# 
# ########################
# # SETUP VARIABLES
# kYear <- 2007
# KHeight <- 100
# 
# # path to meteorological files
# # you have to make sure this path is consistent 
# # for information on how to get HySplit Meteorological data,
# # http://www.arl.noaa.gov/documents/workshop/Spring2011/HYSPLIT_Tutorial.pdf
# KMetFiles <- "/path/to/the/meteorological/files/"
# 
# KOutFiles <- "/path/output/files/"
# 
# # HySplit instalation path
# KHySplitPath <- "/path/to/hysplit/"
# 
# # load the defoliation point file
# data(pheno2007)
# 
# # convert the dates to objects of class Date
# pheno2007$Year.Month.Day <-as.Date(pheno2007$Year.Month.Day)
# 
# # subset the data, in order to get only the points with ID = 1
# pointsDf<-split(pheno2007, pheno2007$ID)
# 
# # get the number of phisical cores availables
# cores <- detectCores()
# # 
# cl <- makeCluster(cores)
# 
# registerDoParallel(cl)
# 
# start.time<-Sys.time()
# 
# hy.traj2007 <- 
#   foreach(i = 1:length(pointsDf), .packages="opentraj", .combine = rbind) %dopar%
# {
#   points <- pointsDf[[i]]
#   
#   # get the point's latitude and longitude
#   lat<-points[[2]][1]
#   long<-points[[3]][1]
#   
#   dates <- points$Year.Month.Day
#   
#   ########################
#   output.file.name<-""
#   output.file.name<-paste("pheno", "_", as.character(i), "_", sep="")
#   
#   ProcTraj(lat = lat, lon = long, year = Year, name = output.file.name,
#            hour.interval = 1,
#            met = KMetFiles, out = KOutFiles, 
#            hours = 3, height = KHeight, hy.path = KHySplitPath, ID = i, dates=dates,
#            start.hour = "19:00", end.hour="23:00",
#            tz="EST", clean.files=F) 
# }
# 
# end.time<-Sys.time()
# time.taken<-end.time - start.time 
# time.taken
# 
# stopCluster(cl)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ProcTraj", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RasterizeTraj")
### * RasterizeTraj

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RasterizeTraj
### Title: Rasterize Trajectory
### Aliases: RasterizeTraj
### Keywords: Raster Frequency

### ** Examples
  
 crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
 air.traj.lines <- Df2SpLines(air.traj, crs)
 raster.lines <- RasterizeTraj(air.traj.lines, reduce=TRUE, resolution=10000, parallel=FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RasterizeTraj", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SplitSpLines")
### * SplitSpLines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SplitSpLines
### Title: Split Spatial Lines
### Aliases: SplitSpLines
### Keywords: Split SpatialLines

### ** Examples

## split the SpatialLines object in a list with 8 SpatialLines objects

crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
air.traj.lines <- Df2SpLines(air.traj, crs)

lines.list <- SplitSpLines(air.traj.lines, 8)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SplitSpLines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("air.traj")
### * air.traj

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: air.traj
### Title: HYSPLIT Forward air trajectories.
### Aliases: air.traj
### Keywords: datasets

### ** Examples

  data(air.traj)
  str(air.traj) 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("air.traj", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hy.traj2007")
### * hy.traj2007

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hy.traj2007
### Title: HySplit trajectory's calculations
### Aliases: hy.traj2007
### Keywords: datasets

### ** Examples

data(hy.traj2007)
str(hy.traj2007) 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hy.traj2007", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hytraj07.lines")
### * hytraj07.lines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hytraj07.lines
### Title: HYSPLIT Trajectory Lines
### Aliases: hytraj07.lines
### Keywords: datasets

### ** Examples

# data(hytraj07.lines)
# str(hytraj07.lines)

# library(sp)
# plot(hytraj07.lines)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hytraj07.lines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hytraj07.linesDf")
### * hytraj07.linesDf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hytraj07.linesDf
### Title: HYSPLIT Trajectory Lines Data Frame
### Aliases: hytraj07.linesDf
### Keywords: datasets

### ** Examples

## data(hytraj07.linesDf)
## str(hytraj07.linesDf)

## library(sp)
## plot(hytraj07.linesDf)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hytraj07.linesDf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pheno2007")
### * pheno2007

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pheno2007
### Title: Phenology of eastern spruce budworm adult emergence in Quebec in
###   2007.
### Aliases: pheno2007
### Keywords: datasets

### ** Examples

data(pheno2007)
str(pheno2007)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pheno2007", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("worldmap")
### * worldmap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: worldmap
### Title: World's Map
### Aliases: worldmap
### Keywords: World's Map

### ** Examples

  data(worldmap)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("worldmap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
