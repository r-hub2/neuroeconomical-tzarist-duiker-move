\name{timeLag}

\alias{timeLag}
\alias{timeLag,.MoveTrackSingle-method}
\alias{timeLag,.MoveTrackStack-method}

\docType{methods}

\title{Time lags between the locations of a movement track}

\description{Calculates the time lags between consecutive locations of a track.}

\usage{
  \S4method{timeLag}{.MoveTrackSingle}(x,...)
  \S4method{timeLag}{.MoveTrackStack}(x,units, ...)
}

\arguments{
  \item{x}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}} or \code{\link[=MoveBurst-class]{moveBurst}} object}
  \item{units}{The units used for the conversion (e.g. "secs", "mins", "hours", "days" or "weeks"). They should be specified for a moveStack to ensure the same units between individuals. Optional (but recommended).
  }
  \item{...}{Currently not implemented.}
}

\details{Optionally the argument \code{units} can be passed on to ensure the time lag is in a certain unit, this is especially useful in case of a moveStack. For more information on the units argument see the help of \code{difftime}.}

\value{
Time lags in the specified units.\cr

If a \code{move} or \code{moveBurst} object is provided, a numeric vector one element shorter than the number of locations is obtained. \cr
If a \code{moveStack} object is provided, a list with one element per individual containing a numeric vector one element shorter than the number of locations is obtained.
}

\author{Bart Kranstauber & Anne Scharf}

\examples{
## time lags from a Move object 
data(leroy)
head(timeLag(leroy, units="hours"))
# to add this information to the move object, a "NA" has to be assigened
# e.g. to the first location (it also could be assigend to the first location).
leroy$timeLag <- c(timeLag(leroy, units="hours"), NA)

## time lags from a MoveStack object
data(fishers)
str(timeLag(fishers, units="mins"))
# to add this information to the moveStack object, a "NA" has to be assigened
# e.g. to the duration is assigned to the first location of each segment
fishers$timeLag <- unlist(lapply(timeLag(fishers, units="mins"),  c, NA))
}
