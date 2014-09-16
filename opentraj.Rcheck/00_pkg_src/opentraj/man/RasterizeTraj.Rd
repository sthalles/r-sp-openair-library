\name{RasterizeTraj}
\alias{RasterizeTraj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Rasterize Trajectory
}
\description{
This function produces a grid over an specified area and then computes the 
frequency of lines that cross each cells' grid. 
}
\usage{
  RasterizeTraj(spLines, resolution=10000, reduce=TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spLines}{
    An object of class SpatialLines created by the function \code{\link{Df2SpLines}}.
  }
  
  \item{resolution}{
    numeric vector of length 1 or 2 to set the resolution. If this argument is used, 
    arguments ncols and nrows are ignored.
  }
  
  \item{reduce}{
  Boolean: If TRUE the result will be reduced to one raster object; if FALSE, 
  this function will return a list of RasterLayer. The size of 
  the list is equal to the number of available cores in the system.
  }
}
\details{
  Because this function do all the process in parallel, it calls the function
  \code{\link{SplitSpLines}} and divides the spLines object into N sub sets of
  Spatial Lines objects, where N is the number of cores availables in the System.
   
}
\value{
  A Object of class RasterLayer or a list of objects RasterLayer.
}

\author{
  Thalles Santos Silva
}

\seealso{
  \code{\link{SplitSpLines}}, \code{\link{raster}}.
}
\examples{  
  ## Copy, paste, and RUN
  ## r <- RasterizeTraj(hytraj07.lines, reduce=TRUE, resolution=10000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Raster }
\keyword{ Frequency }% __ONLY ONE__ keyword per line
