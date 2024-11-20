\name{brownian.motion.variance.dyn}

\alias{brownian.motion.variance.dyn}
\alias{brownian.motion.variance.dyn,MoveStack,numeric,numeric,numeric-method}
\alias{brownian.motion.variance.dyn,.MoveTrackSingle,numeric,numeric,numeric-method}
\alias{brownian.motion.variance.dyn,.MoveTrackSingleBurst,numeric,numeric,numeric-method}

\title{Calculates the dynamic brownian motion variance}

\description{
	A function to calculate the dynamic brownian motion variance for a movement track. It can be also used by advanced programmers to program against.
}

\usage{
  \S4method{brownian.motion.variance.dyn}{.MoveTrackSingle,numeric,numeric,numeric}(object, location.error, window.size, margin)
}

\arguments{
	\item{object}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}} or \code{\link[=MoveBurst-class]{moveBurst}} object can be used for variance calculation. This object must be in a flat coordinate system (projection different to longitude/latitude), use \code{spTransform} to transform your coordinates.}
	\item{location.error}{single numeric value or vector of the length of coordinates that describes the error of the location (sender/receiver) system in map units.}
	\item{window.size}{The size of the moving window along the track for the variance calculation. Larger windows provide more stable/accurate estimates of the brownian motion variance but are less well able to capture more frequent changes in behavior. This number has to be odd.}
	\item{margin}{The margin size used for variance calculation. This number has to be odd.}
}

\value{
'\code{\link[=dBMvariance-class]{dBMvariance}}' object, if \code{move} object is provided \cr
'\code{\link[=dBMvariance-class]{dBMvarianceStack}}' object, if \code{moveStack} object is provided \cr
'\code{\link[=dBMvariance-class]{dBMvarianceBurst}}' object, if \code{moveBurst} object is provided
}

\references{
	Kranstauber, B., Kays, R., LaPoint, S. D., Wikelski, M. and Safi, K. (2012), A dynamic Brownian bridge movement model to estimate utilization distributions for heterogeneous animal movement. Journal of Animal Ecology. doi: 10.1111/j.1365-2656.2012.01955.x
}

\author{Bart Kranstauber & Anne Scharf}


\seealso{
	\link{brownian.bridge.dyn}, \link{dynBGBvariance}
}

\examples{
data(leroy)
data2 <- spTransform(leroy[1:80,], CRSobj="+proj=aeqd +ellps=WGS84", center=TRUE)
err<-rep(23.5,n.locs(data2))
dBMvar <- brownian.motion.variance.dyn(data2, location.error=err, margin=13, window.siz=31)
dBMvar
}
