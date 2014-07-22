####################################################
####################################################
####################################################

script.name="test"

ReadFiles <- function(hours = 96, hy.path, ID) 
{
  # 
  #
  # Args:
  #
  #
  #
  # Results:
  #
  #
  combine.file.name <- paste("Rcombined_", ID, ".txt", sep="")
  dump.file.name <- paste("tdump_", ID, "_", "*", sep="")
  
  # find tdump files
  files <- Sys.glob(dump.file.name)
  output <- file(combine.file.name, 'w')
  
  #print(files)
  
  # read through them all, ignoring 1st 7 lines
  for (i in files){
    input <- readLines(i)
    input <- input[-c(1:7)] # delete header
    writeLines(input, output)
  }
  close(output)
  
  # read the combined txt file
  traj <- read.table(paste0(hy.path, "working/", combine.file.name, sep=""), 
                     header = FALSE)
  
  traj <- subset(traj, select = -c(V2, V7, V8))
  
  traj <- rename(traj, c(V1 = "receptor", V3 = "year", V4 = "month", V5 = "day",
                         V6 = "hour", V9 = "hour.inc", V10 = "lat", V11 = "lon",
                         V12 = "height", V13 = "pressure"))
  
  # hysplit uses 2-digit years ...
  year <- traj$year[1]
  
  if (year < 50) {
    traj$year <- traj$year + 2000 
  } else { 
    traj$year <- traj$year + 1900
  }
  
  # Setup the Canada, ON timezone
  traj$date2 <- with(traj, ISOdatetime(year, month, day, hour, min = 0, 
                                       sec = 0, tz = "EST")) 
  
  # arrival time
  traj$date <- traj$date2 - 3600 * traj$hour.inc
  traj
}

AddMetFiles <- function(month, Year, met, bat.file, control.file) 
{
  ## if month is one, need previous year and month = 12
  if (month == 0) {
    month <- 12
    Year <- as.numeric(Year) - 1
  }
  
  if (month < 10) {
    month <- paste("0", month, sep = "")
  }
  
  ## add first line
  write.table(paste("echo", met, ">>", control.file, sep=" "), 
              bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, 
              append = TRUE)
  
  x <- paste("echo RP", Year, month, ".gbl >> ", control.file, sep = "")
  
  write.table(x, bat.file, col.names = FALSE, row.names = FALSE, 
              quote = FALSE, append = TRUE)
}

# 
ProcTraj <- function(lat = 51.5, lon = -0.1, year = 2010, start.month = 05, 
                     start.day = 01, end.month = 05, end.day = 02, 
                     hour.interval = 1, name = "london",
                     met = "c:/users/david/TrajData/", 
                     out = "c:/users/david/TrajProc/", 
                     hours = 12, height = 100, 
                     hy.path = "/home/thalles/Desktop/hysplit/trunk/", ID) {
  # This function setsup and executes hysplit. The ProcTraj function is 
  # designed for parallel execution.
  #
  # Args:
  #   lat:
  #   lon:
  #   year:
  #   start.month:
  #   start.day:
  #   end.month:
  #   end.day:
  #   hour.interval:
  #   name:
  #   met:
  #   out:
  #   hours:
  #   height:
  #   hy.path:
  #
  # Returns:
  #   It generates an R file with all the trajectories that have been 
  #   calculated by HySplit
  
  # generate an unique CONTROL file name for each instance of HYSPLIT
  control.file.number <- 1 # this number is the extension of the CONTROL file
  
  # hours is the back trajectory time e.g. 96 = 4-day back trajectory
  # height is start height (m)
  lapply(c("openair", "plyr", "reshape2"), require, character.only = TRUE)
  
  # function to run 12 months of trajectories
  # assumes 96 hour back trajectories, 1 receptor
  setwd(paste0(hy.path, "working/"))
  
  # insure that each HySplit process will create an individual script file
  # which allows parallel processing 
  bat.file.name <- paste(script.name, "_", ID, ".sh", sep="")
  
  # name of BAT file to add to/run
  bat.file <- paste0(hy.path, "working/", bat.file.name) 
  
  start <- paste(year, start.month, start.day, sep = "-")
  start <- paste(start, "00:00", sep = " ")
  #print(start)
  
  end <- paste(year, end.month, end.day, sep = "-")
  end <- paste(end, "23:00", sep = " ")
  
  hour.interval <- paste( hour.interval, "hour", sep=" ")
  
  dates <- seq(as.POSIXct(start, "EST"), as.POSIXct(end, "EST"), by = hour.interval)
  
  for (i in 1:length(dates)) {
    control.file <- "CONTROL"
    
    # CONTROL FILE extension
    # format: [1-9]+_[1-9]+...
    # the first number represents the interation ID, it is necessary 
    # to insure that each thread will create a separate group of CONTROl.files
    # the second number represents the ID of the trajectory, which can be 
    # determined by the number of trajectories for each individual point
    control.file.extension <- paste(as.character(ID), "_", control.file.number, sep="")
    
    # create CONTROL file name
    control.file <- paste(control.file, control.file.extension, sep=".")
    
    year <- format(dates[i], "%y")
    Year <- format(dates[i], "%Y") # long format
    month <- format(dates[i], "%m")
    day <- format(dates[i], "%d")
    hour <- format(dates[i], "%H")
    
    #print(hour)
    
    shbang <- "#!/bin/sh"
    
    write.table(shbang, bat.file, col.names = FALSE, row.names = FALSE, 
                quote = FALSE)
    
    x <- paste("echo", year, month, day, hour, ">", control.file, sep=" ")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, 
                quote = FALSE)
    
    x <- paste("echo 1 >>", control.file, sep=" ")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, 
                quote = FALSE, append = TRUE)
    
    x <- paste("echo", lat, lon, height, ">>", control.file, sep=" ")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, 
                quote = FALSE, append = TRUE)
    
    x <- paste("echo", hours, ">>", control.file, sep=" ")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, 
                quote = FALSE, append = TRUE)
    
    x <- paste("echo 0", ">>", control.file, "\n",
              "echo 10000.0 >>", control.file, "\n",
              "echo 3 >>", control.file, "\n",
              sep=" ")
    
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, 
                quote = FALSE, append = TRUE)
    
    ## processing always assumes 3 months of met for consistent tdump files
    months <- as.numeric(unique(format(dates[i], "%m")))
    months <- c(months, months + 1:2)
    months <- months - 1 ## to make sure we get the start of the previous year
    months <- months[months <= 12]
    #print(months)
    if (length(months) == 2) {
      months <- c(min(months) - 1, months)
    }
    
    for (i in 1:3) {
      AddMetFiles(months[i], Year, met, bat.file, control.file)
    }
    
    x <- paste("echo ./ >>", control.file, sep=" ")
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, 
                quote = FALSE, append = TRUE)
    
    x <- paste("echo tdump", "_", ID, "_", year, month, day, hour, 
               " >> ", control.file, sep = "")
    
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, 
                quote = FALSE, append = TRUE)
    
    x <- paste("/home/thalles/Desktop/hysplit/trunk/exec/hyts_std", 
               control.file.extension, sep=" ")
    
    write.table(x, bat.file, col.names = FALSE, row.names = FALSE, 
                quote = FALSE, append = TRUE)
    
    # run the file
    system(paste0("sh ", hy.path, 'working/', bat.file.name))
    
    # create another control file
    control.file.number<-control.file.number+1
  }
  
  # combine files and make data frame
  traj <- ReadFiles(hours, hy.path, ID)
  
  ## write R object to file
  file.name <- paste(out, name, Year, ".RData", sep = "")
  save(traj, file = file.name)
  
  #print("Files generated: ")
  #print(length(dates))
}


# This function will clean up all the files created by the routine ProcTraj
# the files that will be deleted are: tdump_, CONTROL., and MESSAGE.
cleanWD <- function(hy.path = "/home/thalles/Desktop/hysplit/trunk/") {
  # remove existing "tdump" files
  path.files <- paste0(hy.path, "working/")
  
  files <- list.files(path = path.files, pattern = "tdump_")
  lapply(files, function(x) file.remove(x))
  
  # remove existing CONTROL. and MESSAGE files
  files <- list.files(path = path.files, pattern = "CONTROL.")
  lapply(files, function(x) file.remove(x))
  
  files <- list.files(path = path.files, pattern = "MESSAGE.")
  lapply(files, function(x) file.remove(x))
  
  files <- list.files(path = path.files, pattern = "Rcombined_")
  lapply(files, function(x) file.remove(x))
  
  # Delete all the script files
  files <- list.files(path = path.files, 
                      pattern = paste(script.name, "_", sep=""))
  
  lapply(files, function(x) file.remove(x))
  
  invisible(NA)
}
