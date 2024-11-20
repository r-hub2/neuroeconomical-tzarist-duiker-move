\name{speed}

\alias{speed}
\alias{speed,.MoveTrackSingle-method}
\alias{speed,.MoveTrackStack-method}

\docType{methods}

\title{Speed between the locations of a movement track}

\description{This function returns the speed between consecutive locations of Move or MoveStack object.}

\usage{
  \S4method{speed}{.MoveTrackSingle}(x)
  \S4method{speed}{.MoveTrackStack}(x)
}

\arguments{
  \item{x}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}} or \code{\link[=MoveBurst-class]{moveBurst}} object}
}

\author{Marco Smolla & Anne Scharf}

\value{
Speed in map units/second. \cr
If the projection of the coordinates is long/lat all values are returned in m/s, otherwise in the map units/second of the projection of the move object. Check and set the projection of your Move, MoveStack or MoveBurst object using the \code{proj4string()} function.

If a \code{move} or \code{moveBurst} object is provided, a numeric vector one element shorter than the number of locations is obtained. \cr
If a \code{moveStack} object is provided, a list with one element per individual containing a numeric vector one element shorter than the number of locations is obtained.
}

\examples{
# speeds from a Move object 
data(leroy)
head(speed(leroy))
# to add this information to the move object, a "NA" has to be assigened
#  e.g. to the last location (it also could be assigend to the first location).
leroy$speed <- c(speed(leroy), NA)

## speeds from a MoveStack object
data(fishers)
str(speed(fishers))
# to add this information to the moveStack object, a "NA" has to be assigened
# e.g. to the last location of each individual (the speed belongs to the following segment).
fishers$speed <- unlist(lapply(speed(fishers),c, NA ))
}
