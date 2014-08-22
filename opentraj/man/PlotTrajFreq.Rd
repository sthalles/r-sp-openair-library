\name{PlotTrajFreq}
\alias{PlotTrajFreq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot Trajectory Frequency
}
\description{
This function is designed to display a trajectory frequency map that was output by
the function \code{\link{RasterizeTraj}}.
Since the function RasterizeTraj outputs a RasterLayer object, this Object
must be converted to a SpatialGridDataFrame Object using the 
as(rasterObject, "SpatialGridDataFrame") for example.
}
\usage{
PlotTrajFreq(spGridDf, background = T, overlay = NA,
  overlay.color = "white", pdf = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spGridDf}{
    SpatialGridDataFrame Object obtained by the convertion of the 
    raster Object output by the \code{\link{RasterizeTraj}} function.
}
  \item{background}{
    Boolean: Indicates whether or not the Canadian's background map should be displayed.
}
  \item{overlay}{
    [Optional] If defined, it takes a SpatialPolygonsDataFrame as input and plots 
    it over the spGridDf. 
}
  \item{overlay.color}{
    String. sets the Polygons' color defined by the overlay argument e.g. "blue"
}
  \item{pdf}{
    Defines whether or not the output map should be saved in a pdf file.
}
  \item{\dots}{
    Further arguments to be passed to the generic function plot.
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
Thalles Santos Silva
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- I have to to that
# spLines caulculated by the function Df2SpLines
r <- RasterizeTraj(spLines, reduce=T)

r.max.value <- maxValue(r)

v <- getValues(r)

v <- v / r.max.value

r <- setValues(r, v)

#convert raster object to SparialGridDataFrame Object
r1 <- as(r, "SpatialGridDataFrame")

PlotTrajFreq(r1, background = TRUE, main="Title", pdf=F)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
