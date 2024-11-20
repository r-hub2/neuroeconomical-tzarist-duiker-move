\name{plot}

\alias{plot}
\alias{plot,.MoveTrackSingle,missing-method}
\alias{plot,.MoveTrackStack,missing-method}
\alias{plot,.MoveTrackSingleBurst,missing-method}

\docType{methods}

\title{Plotting track or raster}

\description{Function for plotting a recorded track from a Move object or the probability values from a DBBMM object}

\usage{
  \S4method{plot}{.MoveTrackSingle,missing}(x, y,asp=1, ...)
  \S4method{plot}{.MoveTrackStack,missing}(x, y, type="p",asp=1,  ...)
  \S4method{plot}{.MoveTrackSingleBurst,missing}(x, y, type="p",asp=1,  ...)
}

\arguments{
	\item{x}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}}, \code{\link[=MoveBurst-class]{moveBurst}}, \code{\link[=DBBMM-class]{DBBMM}}, \code{\link[=DBBMMStack-class]{DBBMMStack}}, \code{\link[=DBBMMBurstStack-class]{DBBMMBurstStack}}, \code{\link[=dynBGB-class]{dynBGB}}, \code{\link[=dBMvariance-class]{dBMvariance}}, \code{\link[=dBMvariance-class]{dBMvarianceStack}}, \code{\link[=dBMvariance-class]{dBMvarianceBurst}}, \code{\link[=dBGBvariance-class]{dBGBvariance}}, \code{\link[=.UD-class]{.UD}}, \code{\link[=.UDStack-class]{.UDStack}} or \code{\link[=.UDBurstStack-class]{.UDBurstStack}} object}
	\item{y}{unused variable (listed for compatibility reasons)}
	\item{type}{defines the type of the plot (e.g. 'l', 'p', 'b', 'o')}
	\item{asp}{defines the aspect ratio of the plot generally 1 makes most sense since the x and y dimensions are the same}
	\item{...}{arguments to be passed to methods, such as graphical parameters, and the logical \code{add} argument (see \code{\link[graphics]{par}}). See 'Details' for \code{col} (color) options.}
}

\details{
	If \code{x} is a MoveBurst object colored lines (according to the burstID) are plotted if the type is set to 'l'. By default it is 'p' which plots the coordinates of the Move object as points.\cr
	If \code{x} is a DBBMM, DBBMMStack, DBBMMBurstStack or dynBGB object its raster object is plotted with the corresponding cell values. Unlike the \code{\link{image}} function, it keeps the same cell size ratio when the plot window is re-sized.

In the argument \code{col} a vector of colors of the same length as the number of individual for a moveStack, or number of burst levels for a moveBurst object can be specified. If left empty the default 8 colors from R are used, which will be recycled if the object contains more individuals or burst levels (run \code{palette()} to obtain vector of default colors).}

\note{
	Have a look on the proportion of the graphic device when printing a track or raster. The plot function does not use equal sized units on both axes.
}

\author{Marco Smolla & Anne Scharf}

\seealso{
   \link{points}, \link{lines}
}

\examples{
data(leroy)
data(fishers)
plot(leroy) # plot a Move object
plot(leroy, type="o", col=3)
plot(fishers, col=c(3,5), lwd=3) # plot a MoveStack object
plot(fishers, type="l", col=c(3,5), lwd=3)

data(dbbmmstack)
data(leroydbbmm)
plot(leroydbbmm) # plot the raster of a DBBMM object
plot(dbbmmstack) # plot the raster of a DBBMMStack object
}
