\name{dynBGBvariance}
\alias{dynBGBvariance}
\alias{dynBGBvariance,.MoveTrackSingle,numeric,numeric,numeric-method}
\docType{methods}

\title{Calculates the Bivariate Gaussian Bridge motion variance}

\description{A function to calculate the dynamic Bivariate Gaussian Bridge orthogonal and parallel variance for a movement track}

\usage{
dynBGBvariance(move, locErr, margin, windowSize,...)
}

\arguments{
  \item{move}{a \code{\link[=Move-class]{move}} object. This object must be in a projection different to longitude/latitude, use \code{spTransform} to transform your coordinates.}
   \item{locErr}{single numeric value or vector of the length of coordinates that describes the error of the location (sender/receiver) system in map units. Or a character string with the name of the column containing the location error can be provided.}
  \item{margin}{The margin used for the behavioral change point analysis. This number has to be odd.}
  \item{windowSize}{The size of the moving window along the track. Larger windows provide more stable/accurate estimates of the brownian motion variance but are less well able to capture more frequent changes in behavior. This number has to be odd.}
  \item{...}{Additional arguments}
}
\details{
  The function uses \code{windowApply} with the \code{BGBvarbreak} function in order to implement a dynamic calculation of the variance
}
\value{
a \link{dBGBvariance-class} object
}
\references{
	Kranstauber, B., Safi, K., Bartumeus, F.. (2014), Bivariate Gaussian bridges: directional factorization of diffusion in Brownian bridge models. Movement Ecology 2:5. doi:10.1186/2051-3933-2-5.
}

\seealso{
\link{dynBGB}, \link{brownian.motion.variance.dyn}
}
\author{Bart Kranstauber & Anne Scharf}

\examples{
data(leroy)
leroy <- leroy[230:265,]

## change projection method to aeqd and center the coordinate system to the track
dataAeqd <- spTransform(leroy, CRSobj="+proj=aeqd +ellps=WGS84", center=TRUE)

dBGBvar <- dynBGBvariance(dataAeqd, locErr=9, windowSize=31, margin=15)
dBGBvar
}
