####################################################
####################################################
####################################################

ReadFiles <- function(hours = 96, working_dir, ID, dates) 
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
  files <- list.files(path = working_dir, pattern = paste("tdump_", ID, sep=""))
  
  output <- file(combine.file.name, 'w')
  
  if(length(dates) != length(files)){
    print(length(dates))
    print(length(files))
    cleanWD(hy.path, ID)
    stop("Error! Finalmente te peguei!")
  }
  
  #print(files)
  
  # read through them all, ignoring 1st 7 lines
  for (i in files){
    input <- readLines(i)
    input <- input[-c(1:7)] # delete header
    writeLines(input, output)
  }
  close(output)
  
  # read the combined txt file
  traj <- read.table(paste0(working_dir, combine.file.name, sep=""), 
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

AddMetFiles <- function(month, Year, met, script.file, control.file) 
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
  line <- paste("echo", met, ">>", control.file, sep=" ")
  cat(line, file = script.file, sep = "\n")
  
  line <- paste("echo RP", Year, month, ".gbl >> ", control.file, sep = "")
  cat(line, file = script.file, sep = "\n")
}

# 
ProcTraj <- function(lat = 51.5, lon = -0.1, year = 2010, 
                     hour.interval = 1, name = "london",
                     met = "c:/users/david/TrajData/", 
                     out = "c:/users/david/TrajProc/", 
                     hours = 12, height = 100, 
                     hy.path = "/home/thalles/Desktop/hysplit/trunk/", ID,
                     dates, hy.split.exec.dir, script.name="test",
                     add.new.column = F, new.column.name, new.column.value ) {
  
  # This function setsup and executes hysplit. The ProcTraj function is 
  # designed for parallel execution.
  #
  # Args:
  #   lat:
  #   lon:
  #   year:
  #   hour.interval:
  #   name:
  #   met:
  #   out:
  #   hours:
  #   height:
  #   hy.path:
  #   dates: vector containg all the dates that will be calculated by hysplit
  
  # Returns:
  #   It generates an R file with all the trajectories that have been 
  #   calculated by HySplit
  
  # changes the R working directory to the hysplit working directory
  hy.split.wd <- paste0(hy.path, "working/")
  setwd(hy.split.wd)
  
  # process folder name
  folder.name = paste( "process_", ID, sep="")
  
  # each process creates its own folders to store necessary files
  dir.create(file.path(hy.split.wd, folder.name), showWarnings = FALSE)
  
  # process directory full path
  path.to.pwd <- paste(hy.split.wd, folder.name, "/", sep="")
  
  # each process changes the R working directory to its own folder
  setwd(path.to.pwd)
  
  # link all ASC files from to the process directory
  # this spep is requered in order to run hysplit
  system("ln -s /home/thalles/Desktop/hysplit/trunk/bdyfiles/* ./")
  
  # generate an unique CONTROL file name for each instance of HYSPLIT
  control.file.number <- 1 # this number is the extension of the CONTROL file
  
  # hours is the back trajectory time e.g. 96 = 4-day back trajectory
  # height is start height (m)
  lapply(c("openair", "plyr", "reshape2"), require, character.only = TRUE)
  
  # insure that each HySplit process will create an individual script file
  # which allows parallel processing 
  script.name <- paste(script.name, "_", ID, ".sh", sep="")

  # name of the script file to add to/run
  script.path.name <- paste0(path.to.pwd, script.name, sep="/")
   
  ###################
  # process the dates
  dates.and.times <- c()
  
  for( i in 1:length(dates) ){
    start.day <- paste(dates[i], "19:00", sep=" ")
    end.day <- paste(dates[i], "23:00", sep=" ")
    
    posix.date <- seq(as.POSIXct(start.day, "EST"), as.POSIXct(end.day, "EST"), by = "1 hour")
    char.date <- as.character(posix.date)
    
    dates.and.times <- c(dates.and.times, char.date)
  }
  
  if((length(dates) * 5) != length(dates.and.times)){
    stop("Error! Aqui!")
  }
  
  ###################
  hour.interval <- paste( hour.interval, "hour", sep=" ")
  
   
  for (i in 1:length(dates.and.times)) {
    control.file <- "CONTROL"
    
    date <- as.POSIXct(dates.and.times[i], tz="EST")
    
    # CONTROL FILE extension
    # format: [1-9]+_[1-9]+...
    # the first number represents the interation ID, it is necessary 
    # to insure that each thread will create a separate group of CONTROl.files
    # the second number represents the ID of the trajectory, which can be 
    # determined by the number of trajectories for each individual point
    control.file.extension <- paste(as.character(ID), "_", control.file.number, sep="")
    
    # create CONTROL file name
    control.file <- paste(control.file, control.file.extension, sep=".")
    
    year <- format(date, "%y")
    Year <- format(date, "%Y") # long format
    month <- format(date, "%m")
    day <- format(date, "%d")
    hour <- format(date, "%H")
    
    # create file connection
    script.file <- file(script.name, "w")  # open an output file connection
    
    cat("#!/bin/bash", file= script.file, sep="\n")
    
    line <- paste("echo", year, month, day, hour, ">", control.file, sep=" ")
    cat( line, file = script.file, sep = "\n")
    
    line <- paste("echo 1 >>", control.file, sep=" " )
    cat(line, file = script.file, sep="\n")
    
    line <- paste("echo", lat, lon, height, ">>", control.file, sep=" ")
    cat(line, file = script.file, sep="\n")
    
    line <- paste("echo", hours, ">>", control.file, sep=" ")
    cat(line, file = script.file, sep="\n")
    
    line <- paste("echo 0 >> ", control.file, "\n",
                  "echo 10000.0 >> ", control.file, "\n",
                  "echo 3 >> ", control.file, "\n",
                  sep="")
    
    cat(line, file = script.file, sep="")
    
    ## processing always assumes 3 months of met for consistent tdump files
    months <- as.numeric(unique(format(date, "%m")))
    months <- c(months, months + 1:2)
    months <- months - 1 ## to make sure we get the start of the previous year
    months <- months[months <= 12]
    #print(months)
    if (length(months) == 2) {
      months <- c(min(months) - 1, months)
    }
    
    for (i in 1:3) {
      AddMetFiles(months[i], Year, met, script.file, control.file)
    }
    
    line <- paste("echo ./ >>", control.file, sep=" ")
    cat(line, file = script.file, sep="\n")
    
    line <- paste("echo tdump", "_", ID, "_", year, month, day, hour, 
                  " >> ", control.file, sep = "")
    cat(line, file = script.file, sep="\n")
    
    line <- paste(hy.split.exec.dir, control.file.extension, sep=" ")
    cat(line, file = script.file, sep="\n")
    
    # close the file connection
    close(script.file)
    
    # run the file
    system(paste0("sh ", script.name))
    
    # create another control file
    control.file.number <- control.file.number + 1
  
  }
  Sys.sleep(2)
  
  # combine files and make data frame
  traj <- ReadFiles(hours, path.to.pwd, ID, dates.and.times)
  
  # check if add new column was required
  if (add.new.column == T){
    if( !missing(new.column.name) & !missing(new.column.value) ){
      traj[new.column.name] <- new.column.value
    } else {
      stop("Parameters 'new.column.name' and 'new.column.value' are not defined.")
    }
  }
  
  ## write R object to file
  file.name <- paste(out, name, Year, ".RData", sep = "")
  save(traj, file = file.name)
  
  # sets the working directory back in order to delete the process folders
  setwd("/home/thalles/Desktop/hysplit/trunk/working")

  # remove existing "tdump" files 
  unlink(folder.name, recursive = TRUE)
  
  invisible(NA)
}


# # This function will clean up all the files created by the routine ProcTraj
# # the files that will be deleted are: tdump_, CONTROL., and MESSAGE.
# cleanWD <- function(hy.path = "/home/thalles/Desktop/hysplit/trunk/", ID) {
#   # remove existing "tdump" files
#   path.files <- paste0(hy.path, "working/")
#   
#   
#   files <- list.files(path = path.files, pattern = paste("tdump_", ID, sep=""))
#   lapply(files, function(x) file.remove(x))
#   
#   # remove existing CONTROL. and MESSAGE files
#   files <- list.files(path = path.files, pattern = paste("CONTROL.", ID, sep=""))
#   lapply(files, function(x) file.remove(x))
#   
#   files <- list.files(path = path.files, pattern = paste("MESSAGE.", ID, sep=""))
#   lapply(files, function(x) file.remove(x))
#   
#   files <- list.files(path = path.files, pattern = paste("Rcombined_", ID, sep=""))
#   lapply(files, function(x) file.remove(x))
#   
#   # Delete all the script files
#   files <- list.files(path = path.files, 
#                       pattern = paste(script.name, "_", ID, sep=""))
#   
#   lapply(files, function(x) file.remove(x))
#   
#   invisible(NA)
# }
