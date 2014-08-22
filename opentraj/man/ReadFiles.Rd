\name{ReadFiles}
\alias{ReadFiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Read Files
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadFiles(working_dir, ID, dates, tz)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{working_dir}{
%%     ~~Describe \code{working_dir} here~~
}
  \item{ID}{
%%     ~~Describe \code{ID} here~~
}
  \item{dates}{
%%     ~~Describe \code{dates} here~~
}
  \item{tz}{
%%     ~~Describe \code{tz} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (working_dir, ID, dates, tz) 
{
    combine.file.name <- paste("Rcombined_", ID, ".txt", sep = "")
    dump.file.name <- paste("tdump_", ID, "_", "*", sep = "")
    files <- list.files(path = working_dir, pattern = paste("tdump_", 
        ID, sep = ""))
    output <- file(combine.file.name, "w")
    if (length(dates) != length(files)) {
        print(length(dates))
        print(length(files))
        cleanWD(hy.path, ID)
        stop("Error! Finalmente te peguei!")
    }
    for (i in files) {
        input <- readLines(i)
        input <- input[-c(1:7)]
        writeLines(input, output)
    }
    close(output)
    traj <- read.table(paste0(working_dir, combine.file.name, 
        sep = ""), header = FALSE)
    traj <- subset(traj, select = -c(V2, V7, V8))
    traj <- rename(traj, c(V1 = "receptor", V3 = "year", V4 = "month", 
        V5 = "day", V6 = "hour", V9 = "hour.inc", V10 = "lat", 
        V11 = "lon", V12 = "height", V13 = "pressure"))
    year <- traj$year[1]
    if (year < 50) {
        traj$year <- traj$year + 2000
    }
    else {
        traj$year <- traj$year + 1900
    }
    traj$date2 <- with(traj, ISOdatetime(year, month, day, hour, 
        min = 0, sec = 0, tz = tz))
    traj$date <- traj$date2 - 3600 * traj$hour.inc
    traj
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
