\name{turnAngleGc}

\alias{turnAngleGc}
\alias{turnAngleGc,.MoveTrackSingle-method}
\alias{turnAngleGc,.MoveTrackStack-method}


\docType{methods}

\title{Turning angles on great circle tracks}

\description{This function returns the turning angles of a great circle track. This angle represents the relative angle between the consecutive segments.}

\usage{
  \S4method{turnAngleGc}{.MoveTrackSingle}(x)
}

\arguments{
  \item{x}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}} or \code{\link[=MoveBurst-class]{moveBurst}} object, in long/lat projection}
}

\details{
On great circle tracks the bearing of arrival on a point is not the same as witch the previous point was left. This function returns the difference between these bearings between -180 and 180. The bearings are calculated using the functions \code{\link[geosphere]{bearing}} and \code{\link[geosphere:finalDirection]{finalBearing}} of the geosphere package.
}

\author{Bart Kranstauber & Anne Scharf}

\value{
	 Angles in degrees (between -180 and 180)

If a \code{move} or \code{moveBurst} object is provided, a numeric vector two elements shorter than the number of locations is obtained. \cr
If a \code{moveStack} object is provided, a list with one element per individual containing a numeric vector two elements shorter than the number of locations is obtained.
}

\seealso{\link{angle}}

\examples{
## turnAngleGc from a Move object 
data(leroy)
head(turnAngleGc(leroy))
# to add this information to the move object, a "NA" has to be assigened
# to the first and last location.
leroy$turnAngleGc <- c(NA, turnAngleGc(leroy), NA)

## turnAngleGc from a MoveStack object
data(fishers)
str(turnAngleGc(fishers))
# to add this information to the moveStack object, a "NA" has to be assigened
# to the first and last location of each individual
fishers$turnAngleGc <-unlist(lapply(turnAngleGc(fishers), function(x) c(NA, x, NA)))
}
