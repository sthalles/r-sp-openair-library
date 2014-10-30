library("opentraj")
library("sp")
library("rgdal")
library("rgeos")
library("raster")

for( year in seq(1981,1990,1)) {
  
  min.df <- data.frame()
  max.df <- data.frame()
  pcp.df <- data.frame()
  
  #year <- 1981
  Year <- year - 1900
  
  # read the defoliation area
  sbw <- readOGR("/home/thalles/Desktop/daily/defoliation/sbwON", paste0("Spruce_Budworm", Year))
  
  # projection in which the ascii.grid files were created 
  grid.crs <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  # change the projection of the defoliation Polygon to be the
  # same one from the weather grid files
  # they have to be in the same projection in order to crop and use the
  # over function
  # For some reason, when I tried to put the ascii grid files with the 
  # same projection from the defoliation it generates an error.
  # That is why I did the opposite, I put the defiliation with the
  # same projection of the ascii grid files
  sbw <- spTransform(sbw, grid.crs)
  
  for( jday in 1:370){
    
    # check if the file really exist
    # this is important because some files does not have 
    # the file with julian day 60
    if(file.exists(paste0("/home/thalles/Desktop/daily/years/", year, "/max", year, "_", jday, ".asc")) == FALSE){
      next # skip this iteration
    }
    
    # read the ascii.grid file containing the grids cells with weather information
    # for one day of an year. 
    # Also, assign the appropriate projection
    sp.grid.max.df <- read.asciigrid(
      fname = paste0("/home/thalles/Desktop/daily/years/", year, "/max", year, "_", jday, ".asc"), 
      proj4string=grid.crs) # max weather 
    
    sp.grid.min.df <- read.asciigrid(
      fname = paste0("/home/thalles/Desktop/daily/years/", year, "/min", year, "_", jday, ".asc"), 
      proj4string=grid.crs) # min weather 
    
    sp.grid.pcp.df <- read.asciigrid(
      fname = paste0("/home/thalles/Desktop/daily/years/", year, "/pcp", year, "_", jday, ".asc"), 
      proj4string=grid.crs) # pcp weather
    
    
    # merge the data frames 
    # df <- cbind(sp.grid.max.df@data, sp.grid.min.df@data, sp.grid.pcp.df@data)
    names(sp.grid.max.df@data)[1] <- "max"
    names(sp.grid.min.df@data)[1] <- "min"
    names(sp.grid.pcp.df@data)[1] <- "pcp"
    
    # convert each of the three SpatilGridDataFrame to a 
    # different RasterLayer Object
    r1 <- as(sp.grid.max.df, "RasterLayer")
    r2 <- as(sp.grid.min.df, "RasterLayer")
    r3 <- as(sp.grid.pcp.df, "RasterLayer")
    
    # clean these grid objects from memory
    rm(sp.grid.max.df, sp.grid.min.df, sp.grid.pcp.df)
    
    rast.stack <- stack(r1, r2, r3)
    
    # clean these raster objects from memory
    rm(r1, r2, r3)
    
    # crop the canada map to the extention 
    # of the defoliation area
    # This '+1' ensures that the crop area is slightly bigger 
    # than the area of the targeted object
    crop.area <- as(extent(sbw)+1, 'SpatialPolygons')
    crs(crop.area) <- crs(rast.stack)
    cropped.area <- crop(rast.stack, crop.area)
    
    # transforms the raster object previously cropped 
    # to a SpatialPolygonsDataFrame
    grid.poly <- rasterToPolygons(cropped.area)
    
    # verifies which cells of the Polygon is over 
    # the areas of defoliations
    test_over <- over(grid.poly, sbw)
    
    # get only the polygons that intersect with 
    # the defoliation area
    grid.poly.over <- grid.poly[!is.na(test_over),]
    
    # plot(grid.poly.over)
    # plot(sbw, add=T)
    
    # get the central points of each cell
    points <- gCentroid(grid.poly.over, byid=T)
    # plot(points)
    
    grid.df <- as.data.frame(grid.poly.over)
    points.df <- as.data.frame(points)
    
    
    ################################
    # building the final data frames
    
    if(nrow(min.df) == 0){
      min.df <- points.df
      min.df[paste0(as.character(jday), "-", year)] <- cbind(grid.df$min)
    }else{
      min.df[paste0(as.character(jday), "-", year)] <- cbind(grid.df$min)
    }
    
    if(nrow(max.df) == 0){
      max.df <- points.df
      max.df[paste0(as.character(jday), "-", year)] <- cbind(grid.df$max)
    }else{
      max.df[paste0(as.character(jday), "-", year)] <- cbind(grid.df$max)
    }
    
    if(nrow(pcp.df) == 0){
      pcp.df <- points.df
      pcp.df[paste0(as.character(jday), "-", year)] <- cbind(grid.df$pcp)
    }else{
      pcp.df[paste0(as.character(jday), "-", year)] <- cbind(grid.df$pcp)
    }
  }
  
  save(max.df, file = paste0("/home/thalles/Desktop/daily/output/max", "_", year, ".RData"))
  save(min.df, file = paste0("/home/thalles/Desktop/daily/output/min", "_", year, ".RData"))
  save(pcp.df, file = paste0("/home/thalles/Desktop/daily/output/pcp", "_", year, ".RData"))
}

# clean working space
rm(list = ls(all = TRUE))

###########################
###########################

load(file="max_1967.RData")
df1 <- max.df

load(file="max_1968.RData")
df2 <- max.df
df2 <- df2[-c(1,2)]

df <- cbind(df1, df2)
save(df, file = "max_df.RData")


load(file="min_1967.RData")
df1 <- min.df

load(file="min_1968.RData")
df2 <- min.df
df2 <- df2[-c(1,2)]

df <- cbind(df1, df2)
save(df, file = "min_df.RData")


load(file="pcp_1967.RData")
df1 <- pcp.df

load(file="pcp_1968.RData")
df2 <- pcp.df
df2 <- df2[-c(1,2)]

df <- cbind(df1, df2)
save(df, file = "pcp_df.RData")

############################
############################
############################

ConvertJulianDay(61, 2000)

ConvertJulianDay <- function(jday, year){
  months <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  # check if the year is a leap-year
  if( (year %% 4) == 0){
    if( (year %% 100) != 0 ) {
      months[2] <- 29
    } else if( year %% 400 == 0){
      months[2] <- 29
    }
  }
  
  i <- 1
  while(jday > months[i]){
    jday <- jday - months[i]
    i <- i + 1
  }
  jday
}


Poly2Raster <- function(sbw, resolution=10000) {
  # get the extent of  the original polygon
  ext <- extent(sbw) + 1
  
  # get the projection of from the input polygon
  proj <- proj4string(sbw)
  
  r <- raster(ext = ext, crs=CRS(proj), resolution=resolution)
  r <- setValues(r, 1)
  
  grid.poly <- rasterToPolygons(r)
  
  test_over <- over(grid.poly, sbw)
  grid.poly.over <- grid.poly[!is.na(test_over),]
  
  grid.poly.over
}



PolyToRaster <- function(sbw, resolution=10000, as.points=TRUE) {
  # get the extent of  the original polygon
  ext <- extent(sbw)
  
  r <- raster(ext = ext, ncols=40, nrows=40)
  r <- setValues(r, 1)
  crs1 <- "+proj=aea +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  sbw <- spTransform(sbw, CRS(crs1))
  
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
