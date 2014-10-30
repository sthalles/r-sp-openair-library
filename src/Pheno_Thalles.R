# This code was written by Jean-Noel Candau and it uses some algorithms to
# predict the various fases of the Spruce Budworm insect such as pulp and 
# adult. What most interests us in these fases is the Adult fase, when the
# insect can fly therefore it is when they can go from one place to another.
# At some points of this code, we will have the dates when insects start flying
# and dying. It is important to note that these predictions are made for one 
# specific point (coordinate) based on max and min temperature and precipitation
# at that coordinate point.



# convert julian days to regular days
# It outputs the date in a string format [DD-MM-YYYY]
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
  as.Date(paste0(year, "-", i, "-", jday), format="%Y-%m-%d")
}


# This function is in Regniere el al (2012), but it is not used here
TmaxP <- function(Tmax,Tmin,w) { Tmax + w*(Tmax - Tmin) }

r7 <- function(B1,B2,B3,B4,T,Tb,Tm) {
  if (T > Tb && Tm > T) {
    tau <- (T - Tb)/(Tm - Tb)
    B1*(1/(1+exp(B2-B3*tau)) - exp((tau-1)/B4))
  } else {
    0
  }
}

r8 <- function(B1,B2,B3,T,Tb) {
  if (Tb < T) {
    B1*exp(-0.5*((T-B2)/B3)**2)
  } else {
    0
  }
}

r9 <- function(B1,B2,B3,T,Tb,Tm) {
  tau <- min(max(T,Tb),Tm)
  1/(B1+B2*tau+B3*tau**2)
}

r10 <- function(k,q) { 1 - log((runif(1)**(-1*q)-1)/(0.5**(-1*q) - 1))/k}

r11 <- function(k,q) { k*(-1 * log(1-runif(1)))**(-1/q)}
############################################################################
##########################################################################

for( year in seq(1981,1990,1)) {
  ## Load Daily MinT, MaxT and Precip for one year in vectors
  data.frame <- data.frame()
  
  load(paste0("/home/thalles/Desktop/daily/output/max_", year, ".RData"))
  load(paste0("/home/thalles/Desktop/daily/output/min_", year, ".RData"))
  load(paste0("/home/thalles/Desktop/daily/output/pcp_", year, ".RData"))
  
  # since max.df, min.df, and pcp.df have the same number of lines,
  # it does not matter which one we choose to iterate in
  # nrow(min.df)
  for( row in 1:nrow(min.df)) {
  
    # get the points coordinates lat/lon
    point <- max.df[row, c(1,2)]
    
    Tmax <- unlist(max.df[row, -c(1,2)])
    Tmin <- unlist(min.df[row, -c(1,2)])
    PCP <- unlist(pcp.df[row, -c(1,2)])
  
    # Number of days in the year
    n <- length(Tmin)
    
    # Transform into 4-hours intervals
    # The computation start at the 2nd day since it needs previous day's T to apply the sinusoidal interpolation
    # For the first day, take the mean (Tmax+Tmin)/2
    Tm <- rep((Tmax[1]+Tmin[1])/2,6)
    for (i in 2:(n-1)) {
      for (h in seq(0,20,4)) {
        if (h <= 6)  {
          Th <- (Tmin[i] + Tmax[i-1])*0.5 + (Tmax[i-1]-Tmin[i])*0.5 * cospi((h+10)/16)
        } else {
          if (h <= 14) {
            Th <- (Tmax[i] + Tmin[i])*0.5 - (Tmax[i]-Tmin[i])*0.5 * cospi((h-6)/8)
          } else {
            Th <- (Tmax[i] + Tmin[i+1])*0.5 + (Tmax[i]-Tmin[i+1])*0.5 * cospi((h-14)/16)
          }
        }
        Tm <- c(Tm,Th) 
      }
    }
    
    # Set up the dataframe of parameters for the different growth functions
    Params.Rates <- data.frame(Egg=c(0.228,NA,3.12,5.94,0.0073,6,35,0.11),
                                L1=c(0.277,NA,32.14,11.63,NA,6.2,NA,0.11),
                                L2o=c(0.194,NA,3.0,5.84,0.034,2.5,35,0),
                                L2=c(0.362,0.919,2.91,5.32,0.061,4.4,38,0.11),
                                L3=c(0.461,0.438,3.06,6.85,0.061,4.4,38,0.11),
                                L4=c(0.708,1.211,3.80,7.55,0.148,4.4,38,0.11),
                                L5=c(0.332,0.269,3.02,8.57,0.005,4.4,38,0.11),
                                L6_male=c(0.351,0.288,2.67,5.03,0.151,4.4,38,0.11),
                                L6_female=c(0.386,0.317,3.06,4.66,0.136,4.4,38,0.11),
                                Pupa_male=c(0.259,NA,2.75,4.66,0.053,4.4,35,0.11),
                                Pupa_female=c(0.205,NA,2.85,6.28,0.044,4.4,35,0.11),
                                Adult=c(57.8,NA,-3.08,0.045,NA,8,35,0))
    rownames(Params.Rates) <- c("B1a","B1b","B2","B3","B4","Tb","Tm","w")
    
    
    # Next are the parameters and functions for the variability in growth rates
    Params.Var <- data.frame(Egg=c(-3.98,29.48),
                             L2o=c(-6.68,1.05),
                             L2=c(-1.68,8.12),
                             L3=c(-1.13,9.03),
                             L4=c(-1.31,10.02),
                             L5=c(-0.6,7.37),
                             L6=c(-1.8,11.6))
    rownames(Params.Var) <- c("q","k")
    
    # First, we calculate the rate for each larval stage
    L2o <- rep(NA,length(Tm))
    for (i in 1:length(L2o)) L2o[i] <- r7(Params.Rates["B1a","L2o"],Params.Rates["B2","L2o"],Params.Rates["B3","L2o"],Params.Rates["B4","L2o"],Tm[i],Params.Rates["Tb","L2o"],Params.Rates["Tm","L2o"])/6
    L2 <- rep(NA,length(Tm))
    for (i in 1:length(L2)) L2[i] <- r7(Params.Rates["B1b","L2"],Params.Rates["B2","L2"],Params.Rates["B3","L2"],Params.Rates["B4","L2"],Tm[i],Params.Rates["Tb","L2"],Params.Rates["Tm","L2"])/6
    L3 <- rep(NA,length(Tm))
    for (i in 1:length(L3)) L3[i] <- r7(Params.Rates["B1b","L3"],Params.Rates["B2","L3"],Params.Rates["B3","L3"],Params.Rates["B4","L3"],Tm[i],Params.Rates["Tb","L3"],Params.Rates["Tm","L3"])/6
    L4 <- rep(NA,length(Tm))
    for (i in 1:length(L4)) L4[i] <- r7(Params.Rates["B1b","L4"],Params.Rates["B2","L4"],Params.Rates["B3","L4"],Params.Rates["B4","L4"],Tm[i],Params.Rates["Tb","L4"],Params.Rates["Tm","L4"])/6
    L5 <- rep(NA,length(Tm))
    for (i in 1:length(L5)) L5[i] <- r7(Params.Rates["B1b","L5"],Params.Rates["B2","L5"],Params.Rates["B3","L5"],Params.Rates["B4","L5"],Tm[i],Params.Rates["Tb","L5"],Params.Rates["Tm","L5"])/6
    L6_female <- rep(NA,length(Tm))
    for (i in 1:length(L6_female)) L6_female[i] <- r7(Params.Rates["B1b","L6_female"],Params.Rates["B2","L6_female"],Params.Rates["B3","L6_female"],Params.Rates["B4","L6_female"],Tm[i],Params.Rates["Tb","L6_female"],Params.Rates["Tm","L6_female"])/6
    Pupa_female <- rep(NA,length(Tm))
    for (i in 1:length(Pupa_female)) Pupa_female[i] <- r7(Params.Rates["B1a","Pupa_female"],Params.Rates["B2","Pupa_female"],Params.Rates["B3","Pupa_female"],Params.Rates["B4","Pupa_female"],Tm[i],Params.Rates["Tb","Pupa_female"],Params.Rates["Tm","Pupa_female"])/6
    Adult <- rep(NA,length(Tm))
    for (i in 1:length(Adult)) Adult[i] <- r9(Params.Rates["B1a","Adult"],Params.Rates["B2","Adult"],Params.Rates["B3","Adult"],Tm[i],Params.Rates["Tb","Adult"],Params.Rates["Tm","Adult"])/6
    
    # Calculate the age of m individuals taking into account variability in development
    m <- 1000
    E.L2o <- rep(NA,m)
    E.L2 <- rep(NA,m)
    E.L3 <- rep(NA,m)
    E.L4 <- rep(NA,m)
    E.L5 <- rep(NA,m)
    E.L6_female <- rep(NA,m)
    E.Pupa_female <- rep(NA,m)
    E.Adult <- rep(NA,m)
    
    for (i in 1:m) {
      # End of L2o
      v <- r11(Params.Var["k","L2o"],Params.Var["q","L2o"])
      E.L2o[i] <- length(cumsum(v*L2o)[cumsum(v*L2o)<1])
      # End of L2
      L2.tmp <- L2
      L2.tmp[1:E.L2o[i]] <- 0
      v <- r10(Params.Var["k","L2"],Params.Var["q","L2"])
      E.L2[i] <- length(cumsum(v*L2.tmp)[cumsum(v*L2.tmp)<1])
      # End of L3
      L3.tmp <- L3
      L3.tmp[1:E.L2[i]] <- 0
      v <- r10(Params.Var["k","L3"],Params.Var["q","L3"])
      E.L3[i] <- length(cumsum(v*L3.tmp)[cumsum(v*L3.tmp)<1])
      # End of L4
      L4.tmp <- L4
      L4.tmp[1:E.L3[i]] <- 0
      v <- r10(Params.Var["k","L4"],Params.Var["q","L4"])
      E.L4[i] <- length(cumsum(v*L4.tmp)[cumsum(v*L4.tmp)<1])
      # End of L5
      L5.tmp <- L5
      L5.tmp[1:E.L4[i]] <- 0
      v <- r10(Params.Var["k","L5"],Params.Var["q","L5"])
      E.L5[i] <- length(cumsum(v*L5.tmp)[cumsum(v*L5.tmp)<1])
      # End of L6_female
      L6_female.tmp <- L6_female
      L6_female.tmp[1:E.L5[i]] <- 0
      v <- r10(Params.Var["k","L6"],Params.Var["q","L6"])
      E.L6_female[i] <- length(cumsum(v*L6_female.tmp)[cumsum(v*L6_female.tmp)<1])
      # End of Pupa_female
      Pupa_female.tmp <- Pupa_female
      Pupa_female.tmp[1:E.L6_female[i]] <- 0
      E.Pupa_female[i] <- length(cumsum(Pupa_female.tmp)[cumsum(Pupa_female.tmp)<1])
      # End of Adult
      Adult.tmp <- Adult
      Adult.tmp[1:E.Pupa_female[i]] <- 0
      E.Adult[i] <- length(cumsum(Adult.tmp)[cumsum(Adult.tmp)<1])
    }
    
    # Recover the dates for quartiles
    date1 <- quantile(E.L2o%/%6,c(0.25))
    date2 <- quantile(E.L2o%/%6,c(0.75))
    
    Date1 <- ConvertJulianDay(date1, year)
    Date2 <- ConvertJulianDay(date2, year)
    
  #   if( is.na(Date2) ){
  #     stop("Error, Date NA")
  #     print(row)
  #     print(Date2)
  #     print(year)
  #   }
    
    data.frame <- rbind(data.frame, cbind(point, Date1, Date2))
  }
  
  # for some reason, the long is in the first column and the lat is the second,
  # so, lets put the lat as the first and the long as the second
  data.frame <- data.frame[, names(data.frame)][c("y", "x", "Date1", "Date2")]
  names(data.frame) <- c("x", "y", "Date1", "Date2")
  
  # df <- data.frame
  # data.frame[6,][4] <- df[1,][4]
  # data.frame[2352,][4] <- df[2,][4]
  # data.frame[is.na(data.frame),]
  # 
  # nrow(data.frame[!complete.cases(data.frame),])
  
  # save this data frame
  # this date frame contains the point (lat/lon) and the start and end dates for 
  # adult insects
  save(data.frame, file = paste0("/home/thalles/Desktop/daily/output/hysplit/", "pre_hysplit_", year, ".RData"))

  # clean working space
  rm(list = ls(all = TRUE))
}



############################################################
############################################################
## Run HySplit
library("opentraj")
library("doParallel")
library("plyr")

load(paste0("/home/thalles/Desktop/daily/output/hysplit/", "pre_hysplit_", year, ".RData"))

kYear <- year
KHeight <- 100
kHours <- 3

KMetFiles <- paste0("/home/thalles/Desktop/daily/metfiles/", kYear, "/", sep="")

KOutFiles <- paste("/home/thalles/Desktop/daily/output/hysplit/pheno", kYear, "/", KHeight, "M", kHours, "H/", sep="")

# if this directory does not exist, then, creates it
if(!file.exists(KOutFiles)){
  print("Creating folder")
  dir.create(KOutFiles, recursive = TRUE)
}

# HySplit instalation path
KHySplitPath <- "/home/thalles/Desktop/hysplit/trunk/"

# get the number of phisical cores availables
cores <- detectCores()
 
cl <- makeCluster(cores)

registerDoParallel(cl)

start.time<-Sys.time()

# iterate over each line of the data frame
traj <- 
  foreach(i=1:nrow(data.frame), .packages="opentraj", .combine = rbind) %dopar%
{
  df.line <- data.frame[i, ]
  
  # get the point's latitude and longitude
  lat <- df.line$x
  long <- df.line$y
  
  dates <- seq(df.line$Date1, df.line$Date2, by = "1 day")
  date.interval <- as.character(c(df.line$Date1, df.line$Date2))
  
  ########################
  output.file.name <- ""
  output.file.name <- paste("pheno", "_", as.character(i), "_", sep="")
  
  ProcTraj(lat = lat, lon = long, name = output.file.name,
           hour.interval = 1,
           met = KMetFiles, out = KOutFiles, 
           hours = kHours, height = KHeight, hy.path = KHySplitPath, ID = i, dates=dates,
           start.hour = "19:00", end.hour="23:00",
           tz = "EST", clean.files = TRUE,
           add.new.column = T, new.column.name=c("start", "end"), new.column.value=date.interval) 
}

end.time<-Sys.time()
time.taken<-end.time - start.time 
time.taken

stopCluster(cl)

crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
spLines <- Df2SpLines(traj, crs)


###############

max.traj.length <- max(abs(traj$hour.inc)) + 1
traj["ID"] <- rep(1:(nrow(traj)/max.traj.length), each = max.traj.length)
data.list <- ddply(traj, "ID", function(df) {
  df[-c(1:3), ]
}, .inform = TRUE)

end.coord <- data.list[c(7,8)]
####################

spLinesDf <- Df2SpLinesDf(spLines, traj, add.distance = T, add.azimuth = T)
df <- spLinesDf@data
df <- cbind(df, end.coord)
df <- df[, c(2,5,7,8,9,13,14,16,17,18,19)]
names(df) <- c("year", "hour", "start.lat", "start.lon", "height", "start.date",
               "end.date", "distance", "azimuth", "end.lat", "end.lon" )
save(df, file =
       paste0(KOutFiles, "hysplit_", year, "_", KHeight, "H", ".RData" ))

PlotTraj(spLines)

