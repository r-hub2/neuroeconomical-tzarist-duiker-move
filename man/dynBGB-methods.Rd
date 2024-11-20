\name{dynBGB}
\alias{dynBGB}
\alias{dynBGB,.MoveTrackSingle,RasterLayer,numeric-method}
\alias{dynBGB,.MoveTrackSingle,numeric,ANY-method}
\alias{dynBGB,dBGBvariance,RasterLayer,numeric-method}
\alias{dynBGB,.MoveTrackSingle,ANY,character-method}
\alias{dynBGB,.MoveTrackSingle,missing,ANY-method}

\docType{methods}

\title{Calculation of the dynamic Bivariate Gausian Bridge}

\description{
	This function creates a utilization distribution according to the Bivariate Gaussian Bridge model.
}

\usage{
\S4method{dynBGB}{dBGBvariance,RasterLayer,numeric}(move, raster, locErr, timeStep, ...)
\S4method{dynBGB}{.MoveTrackSingle,RasterLayer,numeric}(move, raster, locErr, margin, windowSize, ...)
\S4method{dynBGB}{.MoveTrackSingle,numeric,ANY}(move, raster, locErr, ext, ...)
\S4method{dynBGB}{.MoveTrackSingle,missing,ANY}(move, raster, locErr, dimSize, ext, ...)
}

\arguments{
  \item{move}{a \code{\link[=Move-class]{move}} or \code{\link[=dBGBvariance-class]{dBGBvariance}} object. This object must be in a projection different to longitude/latitude (one suitable for euclidean geometry), use \code{spTransform} to transform your coordinates.}
  \item{raster}{a \code{RasterLayer} object or a numeric value. If a \code{RasterLayer} is provided the \code{dynBGB} starts to calculate the UD based on that raster. If a numeric value is provided it is interpreted as the resolution of the square raster cells (in map units); the according raster will be calculated internally.}
  \item{locErr}{single numeric value or vector of the length of coordinates that describes the error of the location (sender/receiver) system in map units. Or a character string with the name of the column containing the location error can be provided.}
  \item{timeStep}{ It correspond to the size of the timer intervals taken for every integration step (in minutes). If left \code{NULL} 20.1 steps are taken in the shortest time interval. See 'Details'. Optional.}  
  \item{margin}{The margin used for the behavioral change point analysis. This number has to be odd.}
  \item{windowSize}{The size of the moving window along the track. Larger windows provide more stable/accurate estimates of the brownian motion variance but are less well able to capture more frequent changes in behavior. This number has to be odd.}
  \item{ext}{ Describes the amount of extension of the bounding box around the animal track. It can be numeric (same extension into all four directions), vector of two (first x, then y directional extension) or vector of four  (xmin, xmax, ymin, ymax extension). Only considered in combination with a numeric raster argument or the dimSize argument.}
  \item{dimSize}{ numeric. dimSize is only used if \code{raster} is not set. \code{dimSize} is interpreted as the number of cells along the largest dimension of the track. The according raster will be calculated internally.}
  \item{...}{Currently no other arguments implemented.
}
}

\details{
  There are three ways to launch the \code{dynBGB} function:\cr \cr
  1. Use a raster: \cr
  A RasterLayer object is set for the raster argument which is then used to calculate the UD. \cr
  (needed arguments: \emph{move, raster(=\code{RasterLayer}), locErr, margin, windowSize}; optional arguments: \emph{timeStep})\cr \cr
  2. Set the cell size\cr
  To set the cell size, set a numeric value for the raster argument which is used as the cell sizes of the raster. \cr
  (needed arguments: \emph{move, raster(=\code{numeric}), locErr, margin, windowSize, ext}; optional arguments: \emph{timeStep})\cr \cr
  3. Set the number of cells (col/row) \cr
  To set the number of cells along the largest dimension a numeric dimSize argument can be set. \cr
  (needed arguments: \emph{move, dimSize, locErr, margin, windowSize, ext}; optional arguments: \emph{timeStep})\cr \cr
 
\code{timeStep}. The default value is the shortest time interval divided by 20.1. This means, if there is a location recorded e.g. every 40 mins, the function divides each segment into 1.99 mins chunks upon which it does the calculation. If for some reason there is one time interval of 20 secs in the track, each segment of the track will be divided into 1secs chunks, increasing the calculation time immensely. Before calculating the DBBMM, use e.g. \code{min(timeLag(x=}myMoveObject, \code{units="mins"})) to check which is the duration of the shortest time interval of the track. If the track contains time intervals much shorter than the scheduled on the tag, set the \code{timeStep} e.g. to the scheduled time interval at which the tag was set, divided by 20.1.
}

\value{
It returns an object of the class \link{dynBGB-class}.
}

\seealso{
\link{dynBGBvariance}, \link{getMotionVariance}, \link{getVolumeUD}, \link{contour}, \link{outerProbability}, \link{raster}, \link{raster2contour}, \link{brownian.bridge.dyn}, \link{brownian.motion.variance.dyn}
}

\author{Bart Kranstauber & Anne Scharf}
\references{
	Kranstauber, B., Safi, K., Bartumeus, F.. (2014), Bivariate Gaussian bridges: directional factorization of diffusion in Brownian bridge models. Movement Ecology 2:5. doi:10.1186/2051-3933-2-5.
}
\examples{
data(leroy)
leroy <- leroy[230:265,]

## change projection method to aeqd and center the coordinate system to the track
dataAeqd <- spTransform(leroy, CRSobj="+proj=aeqd +ellps=WGS84", center=TRUE)

dBGB <- dynBGB(dataAeqd, locErr=9, raster=10, ext=0.5, windowSize=31, margin=15, timeStep=15/20.1)
plot(dBGB, col=hsv(sqrt(1:700/1000)))
lines(dataAeqd)
}


