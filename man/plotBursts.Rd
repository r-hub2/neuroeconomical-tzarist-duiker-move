\name{plotBursts}

\alias{plotBursts}
\alias{plotBursts,list-method}
\alias{plotBursts,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Plotting the centroids of a bursted track}

\description{
  The \code{plotBursts} function plots bursted Move objects (see \link{burst} for how to create a bursted Move object). The function plots a circle at the midpoint of each burst segment (consecutive coordinates that belong to a single burst).
  }

\usage{
  \S4method{plotBursts}{list}(object, add=TRUE, 
            sizeFUN=function(x){as.numeric(diff(range(timestamps(x))),units ="mins")},
            col = NA, breaks = 3, ...)
            
  \S4method{plotBursts}{.MoveTrackSingleBurst}(object, add=TRUE,
            sizeFUN=function(x){as.numeric(diff(range(timestamps(x))),units ="mins")}, 
            col = NA, breaks = 3, ...)
}

\arguments{
  \item{object}{a \code{moveBurst} object or a list of \code{moveBurst} objects}
  \item{add}{logical, if FALSE a new plot is generated, default is TRUE}
  \item{sizeFUN}{a function to calculate the size of the plotted circles (see 'Details')}
  \item{breaks}{how many size classes should the circles have, default is 3}
  \item{col}{a vector of color codes with the same length as the burstID. By default the standard colors from 1:8 are used (see \code{palette()} to obtain vector of default colors). If there are more than 8 burstIDs the colors are recycled}
  \item{...}{additional plot attributes}
}

\details{
\emph{\code{sizeFUN}}\cr The color of the circles correspond to the burstIDs. The size of the cycles can have different meanings, depending on what function is defined. By default the size refers to the relative time of the burst segment compared to the whole track. This argument accepts any personalized function.
}

\note{
If a list of moveBurst objects is provided, the plots are plotted one after another, and not side by side.
}

\author{Marco Smolla & Anne Scharf}

\examples{
data(leroy)
behav <- c(rep(1:4,each=200), rep(5, 118))
testb <- burst(leroy, f=behav)
plot(coordinates(leroy),type="l")
plotBursts(testb, breaks=3, add=TRUE, pch=19)
plotBursts(testb, breaks=5, add=FALSE, pch=19, col=rainbow(5))

## plotting circle size of a moveBurst track by realtive segment length
plotBursts(object=testb, breaks=3, sizeFUN=function(x){sum(distance(x))}, pch=19, add=FALSE)

}
