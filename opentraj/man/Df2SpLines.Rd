\name{Df2SpLines}
\alias{Df2SpLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Frame to Spatial Lines
}
\description{
This function converts an object of type \code{\link{data.frame}}, calculated by 
the function \code{\link{ProcTraj}}, into an object of type \code{\link{SpatialLines-class}}.
}
\usage{
Df2SpLines(df, crs=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
    \code{\link{data.frame}} Object created by the function \code{\link{ProcTraj}}.
  }
  \item{crs}{
    String: Valid projection string. An example would be crs="+proj=longlat +datum=NAD27"
  }
}
\details{
An individual line consists of a set of lines in the data frame that contains the same ID. This function identifies individual trajectories based on their length. It is assumed that all trajectories calculated by HySplit using the \code{\link{ProcTraj}} function have the same length. Thus, once known the length of the trajectories, this function splits the data frame in X different data frames where each data frame contains R rows, R being the trajectory’s length and X being the number of rows in the initial data frame divided by the trajectory’s length. Each of the X different data frames will be transformed into a different line.
}
\value{
Returns an object of class \code{\link{SpatialLines-class}}.
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
\code{\link{data.frame}}, \code{\link{ProcTraj}}, \code{\link{SpatialLines-class}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- I have to do it
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Lines }
\keyword{ Trajectories }% __ONLY ONE__ keyword per line
