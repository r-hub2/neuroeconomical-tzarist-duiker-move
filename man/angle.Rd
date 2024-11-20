\name{angle}

\alias{angle}
\alias{angle,.MoveTrackSingle-method}
\alias{angle,.MoveTrackStack-method}

\docType{methods}

\title{Headings of the segments of a movement track}

\description{This function calculates the heading/azimuth/direction of movement of each segment between consecutive locations of a track.}

\usage{
  \S4method{angle}{.MoveTrackSingle}(x)
  \S4method{angle}{.MoveTrackStack}(x)
}

\arguments{
  \item{x}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}} or \code{\link[=MoveBurst-class]{moveBurst}} object}
}

\details{Other terms for this measurment are azimuth or direction of travel/movement. The angles are relative to the North pole. The headings are calculated using the functions \code{\link[geosphere]{bearing}} of the geosphere package.}

\author{Marco Smolla & Anne Scharf}

\value{
Angles in degrees (between -180 and 180). North is represented by 0.

If a \code{move} object is provided, a numeric vector one element shorter than the number of locations is obtained. \cr
If a \code{moveStack} object is provided, a list with one element per individual containing a numeric vector one element shorter than the number of locations is obtained.
}

\seealso{\link{turnAngleGc}}

\examples{
## angles from a Move object 
data(leroy)
head(angle(leroy))
# to add this information to the move object, a "NA" has to be assigened
# e.g. to the last location (it also could be assigend to the first location).
leroy$angles <- c(angle(leroy), NA)

## angles from a MoveStack object
data(fishers)
str(angle(fishers))
# to add this information to the moveStack object, a "NA" has to be assigened
# e.g. to the last location of each individual
fishers$angles <- unlist(lapply(angle(fishers), c, NA))
}
