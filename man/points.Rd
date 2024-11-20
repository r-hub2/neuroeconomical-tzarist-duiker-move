\name{points}

\alias{points}
\alias{points,.MoveTrackSingle-method}
\alias{points,.MoveTrackStack-method}
\alias{points,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Plotting the points of a track}

\description{Function for plotting a track from a Move object as points.}

\usage{
  \S4method{points}{.MoveTrackSingle}(x,...)
  \S4method{points}{.MoveTrackStack}(x,col=NA,...)
  \S4method{points}{.MoveTrackSingleBurst}(x,...)
}

\arguments{
	\item{x}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}}, \code{\link[=MoveBurst-class]{moveBurst}}, \code{\link[=dBMvariance-class]{dBMvariance}}, \code{\link[=dBMvariance-class]{dBMvarianceStack}}, \code{\link[=dBMvariance-class]{dBMvarianceBurst}} or \code{\link[=dBGBvariance-class]{dBGBvariance}} object.}
	\item{col}{a vector of colors of the same length as the number of individual for a moveStack, or number of burst levels for a moveBurst object. If left empty the default 8 colors from R are used, which will be recycled if the object contains more individuals or burst levels (run \code{palette()} to obtain vector of default colors)}
	\item{...}{arguments to be passed on, e.g. \code{col} for color, or \code{add} to add the points to a plot. See \code{?par} for options.}
}

\author{Marco Smolla & Anne Scharf}

\seealso{
	\code{\link{plot}},
	\code{\link{lines}}
}

\examples{
## add a track from a Move object to a plot
data(leroydbbmm)
data(leroy)
plot(leroydbbmm)
points(spTransform(leroy, center=TRUE), col=3) # add a track from a Move object to a plot

## plot a moveStack object
data(fishers)
plot(fishers, type="l")
points(fishers, col=3:4, pch=4) 

}
