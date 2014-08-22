\name{PlotTraj}
\alias{PlotTraj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot Trajectory
}
\description{
  The function PlotTraj is designed to plot HySplit trajectories calculated  by
  the function \code{\link{ProcTraj}}.
}
\usage{
  PlotTraj(traj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{
    SpatialLines or SpatialLinesDataFrame calculated by the functions \code{\link{Df2SpLines}}
    and \code{\link{Df2SpLinesDF}} respectively. 
  }
  \item{\dots}{
    Further arguments to be passed to the generic function plot.
  }
}
\details{
  This function calls the function \code{\link{PlotBgMap}} to plot the 
  background map behind the trajectories.
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
  \code{\link{Df2SpLines}}, \code{\link{Df2SpLinesDF}}, \code{\link{ProcTraj}}.
}

\examples{
##---- I gotta do that
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
