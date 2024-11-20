\name{seglength}

\alias{seglength}
\alias{seglength,SpatialPointsDataFrame-method}

\docType{methods}

\title{Segment lengths of a track}

\description{Calculates the length of each segment of a track}

\usage{
  \S4method{seglength}{SpatialPointsDataFrame}(x)
}

\arguments{
  \item{x}{a Move, MoveStack or MoveBurst object}
}

\details{
  The \code{seglength} function calculates the distances between point 1 and point 2, point 2 and point 3, and so on. \cr
  Distances are calculates with the \link[raster]{pointDistance} function from the package raster. \cr
}

\value{
 A numeric vector one element shorter than the number of locations is obtained. Note that in moveStacks distances are not split between animals (see 'Examples' on how to add the values to a moveStack).
 
Length in map units. \cr
If the projection of the coordinates is longitude/latitude all values are returned in meters, otherwise it is the Euclidean distance in the map units of the projection of the move object. Check and set the projection of your Move, MoveStack or MoveBurst object using the \code{proj4string()} function.


}

\author{Marco Smolla}

\examples{

\dontrun{
## Move object in longlat projection
data(leroy)
head(seglength(leroy)) 
# to add this information to the move object, a "NA" has to be assigened
# e.g. to the last location (it also could be assigend to the first location).
leroy$segLength <- c(seglength(leroy), NA)

## MoveStack object in longlat projection
data(fishers)
head(seglength(fishers))
# to add this information to the moveStack object, a "NA" has to be assigened
# e.g. to the last location of each individual (it also could be assigend to the first location).
fishers$segLength <-  unlist(lapply(lapply(split(fishers),seglength),c, NA))
}
}
