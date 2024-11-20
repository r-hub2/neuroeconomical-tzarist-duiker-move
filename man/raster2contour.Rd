\name{raster2contour}

\alias{raster2contour}
\alias{raster2contour,.UD-method}
\alias{raster2contour,.UDStack-method}

\docType{methods}

\title{Convert raster to contour lines}

\description{The function converts a raster UD(stack) object to a SpatialLinesDataFrame. This allows to re-project the contours to different projections.}

\usage{
	\S4method{raster2contour}{.UD}(x, ...)
	\S4method{raster2contour}{.UDStack}(x, ...)
}

\arguments{
	\item{x}{a \code{DBBMM}, \code{DBBMMStack}, \code{dynBGB}, \code{.UD} or \code{.UDStack} object}
	\item{...}{additional arguments, like \code{levels} and \code{nlevels}, that can be passed to '\code{\link[raster]{rasterToContour}}' function}
}

\details{
The contour function creates a shape of the area in which the animal can be found by a certain probability (i.e. the 90\% contour describes the area in which the animal can be found with the 90\% probability).\cr 
One or several probabilities can be set with \code{levels} (numeric or vector of values between 0 and 1). If no value is set all contour lines are returned.\cr 
You can also use \code{nlevel} to set a number of fixed distance levels. \cr

The \code{raster2contour} function creates a \link[sp:SpatialLinesDataFrame-class]{SpatialLinesDataFrame} from the input raster object. This allows to re-project the contours to different projections. 
}

\value{
'\code{SpatialLinesDataFrame}'
}

\author{Marco Smolla & Anne Scharf}

\seealso{
	\code{\link{getVolumeUD}}, \code{\link{contour}}, \code{\link{outerProbability}}
}

\examples{
data(leroydbbmm)
data(leroydbgb)
data(dbbmmstack)

## from a DBBMM object
(cont1 <- raster2contour(leroydbbmm))
plot(cont1)

## from a dynBGB object
(cont2 <- raster2contour(leroydbgb, level=.95))
plot(cont2)

## from a DBBMMStack object
(cont3 <- raster2contour(dbbmmstack))
plot(cont3)
(cont4 <- raster2contour(dbbmmstack, level=c(.5,.95)))
plot(cont4)

}
