\name{distance}

\alias{distance}
\alias{distance,.MoveTrackSingle,missing-method}
\alias{distance,.MoveTrackStack,missing-method}

\docType{methods}

\title{Distance between the locations of a movement track}

\description{Calculates the distance between the consecutive locations of a Move or MoveStack object.}

\usage{
  \S4method{distance}{.MoveTrackSingle,missing}(x)
  \S4method{distance}{.MoveTrackStack,missing}(x)
}

\arguments{
  \item{x}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}} or \code{\link[=MoveBurst-class]{moveBurst}} object}
}

\details{
    \code{\link[raster]{pointDistance}} is used to calculate the distances.
}

\value{
Distance in map units. \cr
If the projection of the coordinates is long/lat all values are returned in meters, otherwise in the map units of the projection of the move object. For long/lat distance is calculated on a sphere using the ellipsoid, for other projections the calculation is done on a plane using Pythagoras. Check and set the projection of your Move, MoveStack or MoveBurst object using the \code{proj4string()} function.

If a \code{move} or \code{moveBurst} object is provided, a numeric vector one element shorter than the number of locations is obtained. \cr
If a \code{moveStack} object is provided, a list with one element per individual containing a numeric vector one element shorter than the number of locations is obtained
}

\author{Marco Smolla & Anne Scharf}

\examples{
## distance from a Move object 
data(leroy)
head(distance(leroy))
# to add this information to the move object, a "NA" has to be assigened
# e.g. distance is assigned to the first location of a segment
leroy$distance <- c(distance(leroy), NA)

## distance from a MoveStack object
data(fishers)
str(distance(fishers))
# to add this information to the moveStack object, a "NA" has to be assigened
# e.g. distance is assigned to the first location of a segment
fishers$distance <- unlist(lapply(distance(fishers), c, NA))
}
