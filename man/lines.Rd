\name{lines}

\alias{lines}
\alias{lines,.MoveTrackSingle-method}
\alias{lines,.MoveTrackStack-method}
\alias{lines,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Plotting the lines of a track}

\description{Function for plotting a track from a Move object as lines}

\usage{
  \S4method{lines}{.MoveTrackSingle}(x,...)
  \S4method{lines}{.MoveTrackStack}(x,col=NA,...)
  \S4method{lines}{.MoveTrackSingleBurst}(x,col=NA,...)
}

\arguments{
  \item{x}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}}, \code{\link[=MoveBurst-class]{moveBurst}}, \code{\link[=dBMvariance-class]{dBMvariance}}, \code{\link[=dBMvariance-class]{dBMvarianceStack}}, \code{\link[=dBMvariance-class]{dBMvarianceBurst}} or \code{\link[=dBGBvariance-class]{dBGBvariance}} object.}
  \item{col}{a vector of colors of the same length as the number of individual for a moveStack, or number of burst levels or of segments for a moveBurst object. If left empty the default 8 colors from R are used, which will be recycled if the object contains more individuals or burst levels (run \code{palette()} to obtain vector of default colors)}
  \item{...}{arguments to be passed on, e.g. \code{lty} or \code{lwd}.}
}


\author{Marco Smolla & Anne Scharf}

\seealso{
	\link{points}, \link{plot}
}

\examples{
## add a track from a Move object to a plot
data(leroy)
data(leroydbbmm)
plot(leroydbbmm)
lines(spTransform(leroy, center=TRUE), col=3)

## plot the points and lines of a moveStack
data(fishers)
plot(fishers, type='p',pch=4)
lines(fishers, col=3:4)
}
