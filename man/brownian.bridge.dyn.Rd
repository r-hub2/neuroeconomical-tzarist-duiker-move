\name{brownian.bridge.dyn}

\alias{brownian.bridge.dyn}
\alias{brownian.bridge.dyn,.MoveTrackSingle,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,MoveStack,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,.MoveTrackSingle,missing,missing,numeric-method}
\alias{brownian.bridge.dyn,SpatialPointsDataFrame,missing,numeric,ANY-method}
\alias{brownian.bridge.dyn,SpatialPointsDataFrame,numeric,missing,ANY-method}
%\alias{brownian.bridge.dyn,Move,numeric,missing,numeric-method}
%\alias{brownian.bridge.dyn,Move,missing,numeric,numeric-method}
\alias{brownian.bridge.dyn,dBMvariance,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,dBMvarianceStack,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,dBMvarianceBurst,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,ANY,RasterLayer,missing,character-method}

\docType{methods}

\title{Calculates a dynamic Brownian Bridge}

\description{This function uses a Move or MoveStack object to calculate the utilization distribution (UD) of the given track. It uses the dynamic Brownian Bridge Movement Model (dBBMM) to do so, having the advantage over the other Brownian Bridge Movement Model that changes in behavior are accounted for. It does so by using the behavioral change point analysis in a sliding window. For details see 'References'.}

\usage{
brownian.bridge.dyn(object, raster=1, dimSize=10, location.error, 
                    margin=11, window.size=31, ext=.3, bbox=NA,...)
}


\arguments{
  \item{object}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}}, \code{\link[=MoveBurst-class]{moveBurst}}, \code{\link[=dBMvariance-class]{dBMvariance}}, \code{\link[=dBMvariance-class]{dBMvarianceStack}} object. This object must be in a projection different to longitude/latitude, use \code{spTransform} to transform your coordinates.}
  \item{raster}{a \code{RasterLayer} object or a numeric value. If a \code{RasterLayer} is provided the \code{brownian.bridge.dyn} starts to calculate the UD based on that raster. If a numeric value is provided it is interpreted as the resolution of the square raster cells (in map units); the according raster will be calculated internally.}
  \item{dimSize}{numeric. dimSize is only used if \code{raster} is not set. \code{dimSize} is interpreted as the number of cells along the largest dimension of the track. The according raster will be calculated internally.}
  \item{location.error}{single numeric value or vector of the length of coordinates that describes the error of the location (sender/receiver) system in map units. Or a character string with the name of the column containing the location error can be provided.}
  \item{margin}{The margin used for the behavioral change point analysis. This number has to be odd.}
  \item{window.size}{The size of the moving window along the track. Larger windows provide more stable/accurate estimates of the brownian motion variance but are less well able to capture more frequent changes in behavior. This number has to be odd.}
  \item{ext}{Describes the amount of extension of the bounding box around the animal track. It can be numeric (same extension into all four directions), vector of two (first x, then y directional extension) or vector of four  (xmin, xmax, ymin, ymax extension). Default is .3 (extends the bounding box by 30\%). Only considered in combination with a numeric raster argument or the dimSize argument.}
  \item{bbox}{vector with 4 numbers defining a bounding box for the raster. Only considered in combination with a numeric raster argument or the dimSize argument.}
  \item{...}{Additional arguments: 
  \describe{
  \item{\code{time.step}}{It correspond to the size of the time intervals taken for every integration step (in minutes) and thus specifies the temporal resolution of the numerical integration. If left \code{NULL} 15 steps are taken in the shortest time interval. See 'Details'. Optional.}
  \item{\code{verbose}}{logical. default is TRUE; if FALSE printing messages about the computational size is suppressed. Optional.}
  \item{\code{burstType}}{character vector with the name(s) of burstId(s) for which the UD should be calculated. This attribute can only be used if a \code{moveBurst} is provided in the \code{object} argument. Optional.}
  }
  }
  }

\details{
  There are four ways to launch the \code{brownian.bridge.dyn function}:\cr \cr
  1. Use a raster: \cr
  A RasterLayer object is set for the raster argument which is then used to calculate the UD. \cr
  (needed arguments: \emph{object, raster(=\code{RasterLayer}), location.error, margin, window.size}; optional arguments: \emph{time.step, verbose, burstType})\cr \cr
  2. Set the cell size\cr
  To set the cell size, set a numeric value for the raster argument without providing dimSize. The numeric raster argument is used as the cell sizes of the raster. \cr
  (needed arguments: \emph{object, raster(=\code{numeric}), location.error, margin, window.size}; optional arguments: \emph{ext, bbox, time.step, verbose, burstType})\cr \cr
  3. Set the number of cells (col/row) \cr
  To set the number of cells along the largest dimension a numeric dimSize argument can be set. \cr
  (needed arguments: \emph{object, dimSize, location.error, margin, window.size}; optional arguments: \emph{ext, bbox, time.step, verbose, burstType})\cr \cr
  4. Using default raster \cr
  When there are no values set, the default raster value is used to calculate and create a RasterLayer object, which is returned to the same function. Note: depending on the size of the area of interest, the default cell size value can result in a large number of cells which may take a very long time to calculate!\cr \cr
  
  The function prints an estimate of the size of the computational task ahead. This can give an indication of how long the computation is going to take. It should scale roughly linearly with the duration of the computations although changes in the setup mean the computational complexity still is calculated base on the extent but this is not informative any more on the computation time. It is only useful as a rough indication of calculation duration.

\code{time.step}. The default value is the shortest time interval divided by 15. This means, if there is a location recorded e.g. every 30 mins, the function divides each segment into 2 mins chunks upon which it does the calculation. If for some reason there is one time interval of 15 secs in the track, each segment of the track will be divided into 1secs chunks, increasing the calculation time immensely. Before calculating the DBBMM, use e.g. \code{min(timeLag(x=myMoveObject, units="mins"))} to check which is the duration of the shortest time interval of the track. If the track contains time intervals much shorter than the scheduled on the tag, set the \code{time.step} e.g. to the scheduled time interval at which the tag was set, divided by 15.
}


\note{
	Note that the first few and last few segments of the trajectory are omitted in the calculation of the UD since a lower number of estimates for the Brownian motion variance are obtained for those segments.

	Thanks to Ryan Nielson for making the BBMM package that served as an example for early versions of this code.
}

\value{
'\code{\link[=DBBMM-class]{DBBMM}}' object, if \code{move} or \code{dBMvariance} object is provided \cr
'\code{\link[=DBBMMStack-class]{DBBMMStack}}' object, if \code{moveStack} or \code{dBMvarianceStack} object is provided \cr
'\code{\link[=DBBMMBurstStack-class]{DBBMMBurstStack}}' object, if \code{moveBurst} object is provided
}

\seealso{
\link{brownian.motion.variance.dyn}, \link{getMotionVariance}, \link{getVolumeUD}, \link{contour}, \link{outerProbability}, \link{raster}, \link{raster2contour}, \link{dynBGB}, \link{dynBGBvariance}
}

\author{Bart Kranstauber, Marco Smolla & Anne Scharf}

\references{
	Kranstauber, B., Kays, R., LaPoint, S. D., Wikelski, M. and Safi, K. (2012), A dynamic Brownian bridge movement model to estimate utilization distributions for heterogeneous animal movement. Journal of Animal Ecology. doi: 10.1111/j.1365-2656.2012.01955.x
}

\examples{
## create a move object
data(leroy)
## change projection method to aeqd and center the coordinate system to the track
data2 <- spTransform(leroy[30:90,], CRSobj="+proj=aeqd +ellps=WGS84", center=TRUE)

## create a DBBMM object
dbbmm <- brownian.bridge.dyn(object=data2, location.error=12, dimSize=125, ext=1.2, 
			     time.step=2, margin=15)

plot(dbbmm)

\dontshow{
data(fishers)
  leroy <- leroy[1:50]
  ricky<-move(system.file("extdata","ricky.csv.gz", package="move"))[1:50]
  leroy <- spTransform(leroy, 
		       crs(ricky))
  stack <- moveStack(list(leroy,ricky))
  t <- brownian.bridge.dyn(spTransform(stack, center=TRUE), location.error=23.5, dimSize=1125, 
			   time.step=6, ext=1.2)
}
}
