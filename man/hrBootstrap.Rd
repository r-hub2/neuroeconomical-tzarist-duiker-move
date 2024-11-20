\name{hrBootstrap}

\alias{hrBootstrap}
\alias{hrBootstrap,SpatialPoints-method}
\alias{hrBootstrap,.MoveTrackStack-method}

\docType{methods}

\title{Calculates and plots the area of the Minimum Convex Polygon of a track}

\description{The hrBootstrap function calculates the 0, 25, 50, 75, 100\%  percentile of the Minimum Convex Polygon (MCP) area by a logarithmic step wise increase of the number of samples per calculation. For every step this calculation is repeated \code{rep} times with random coordinates from the track.}

\usage{
  \S4method{hrBootstrap}{SpatialPoints}(x, rep=100, plot=TRUE, level=95,
            levelMax=100, unin='km', unout='m2', ...)
  
  \S4method{hrBootstrap}{.MoveTrackStack}(x, rep=100, plot=TRUE, level=95, 
            levelMax=100, unin="km", unout="m2", ...)

}

\arguments{
  \item{x}{a \code{move}, \code{moveStack}, \code{moveBurst} or \code{SpatialPoints} object}
  \item{rep}{numeric value for the number of repetitions per sample size, default is 100}
  \item{plot}{logical value that indicates whether the graph is plotted or not, default is TRUE}
  \item{level}{the percentage of coordinates taken into account for the MCP area size calculation in each step, default is 95 (95\% of all coordinates per step are taken into account)}
  \item{levelMax}{the percentage of coordinates taken into account for the maximum MCP area size calculation (horizontal line in the plot)}
  \item{unin}{units from the input values (can be 'm' or 'km')}
  \item{unout}{units for the output values (can be 'm2', 'km2', or 'ha')}
  \item{...}{Currently not implemented}
}

\details{
The function calculates the 0, 25, 50, 75, 100\%  percentile of the Minimum Convex Polygon (MCP) area with a logarithmic step wise increase of the number of samples per calculation. For every step this calculation is repeated \code{rep} times with random coordinates from the track. For example it calculates 100 times the MCP area from 3 random locations and stores the area. In the next step it calculates it from 5 random locations and so on. The returned graph shows the 5 percentiles of the area sizes (see 'Values'). The dot-dashed line indicates the real MCP area size of all locations.

  The \code{hrBootstrap} function passes values (samples of the track) on to the function \code{mcp} that is part of the adehabitatHR package. See the help of \code{\link[adehabitatHR]{mcp}} for more information about input and output units.
}

\note{
  Plots for MoveStacks are plotted one after another, and not side by side. 
}

\value{
  The values are returned in a data.frame with the units indicated by \code{unout}.
  
  Plot legend: \cr
  - 0\% percentile of mcp area: blue bottom line \cr
  - 25\% percentile of mcp area: red bottom line\cr
  - 50\% percentile of mcp area: black middle line\cr
  - 75\% percentile of mcp area: red top line\cr
  - 100\% percentile of mcp area: blue top line\cr
  - Real mcp area size of all locations: horizontal dot-dashed black line

The number of locations used in each step are printed in the console.
}
  
\author{Marco Smolla & Anne Scharf}
  
\examples{
if(requireNamespace("adehabitatHR")){
## for a Move object
  m <- move(x=rnorm(55), y=rnorm(55), time=as.POSIXct(1:55, origin="1970-1-1"), 
	          proj=CRS("+proj=aeqd +ellps=WGS84"), animal='a')    
  hrBootstrap(m,rep=5, level=99, unout="m2", plot=TRUE)

## for a MoveStack object
   m2 <- move(x=rnorm(30), y=rnorm(30), time=as.POSIXct(1:30, origin="1970-1-1"), 
	           proj=CRS("+proj=aeqd +ellps=WGS84"), animal='b') 
  mstack <- moveStack(list(m[30:50,],m2))
  hrBootstrap(mstack,rep=5, unout="m2", plot=FALSE) 

## for a SpatialPoints object
  hrBootstrap(as(m,"SpatialPoints"),rep=5, unout="m2", plot=TRUE) 
}
}

