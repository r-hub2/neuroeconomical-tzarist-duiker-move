\name{getMotionVariance}
\alias{getMotionVariance}
\alias{getMotionVariance,DBBMM-method}
\alias{getMotionVariance,DBBMMBurstStack-method}
\alias{getMotionVariance,DBBMMStack-method}
\alias{getMotionVariance,dBGBvarianceTmp-method}
\alias{getMotionVariance,dBMvarianceStack-method}
\alias{getMotionVariance,dBMvarianceTmp-method}
\alias{getMotionVariance,dBMvarianceBurst-method}
\alias{getMotionVariance,dynBGB-method}


\docType{methods}

\title{Extracts the estimated motion variance}

\description{This function returns the estimated motion variance calculated by the \link[=dynBGB]{dynamic Bivariate Gaussian Bridges} or \link[=brownian.bridge.dyn]{dynamic Brownian Bridges}}

\usage{
getMotionVariance(x,...)
}

\arguments{
  \item{x}{a \code{DBBMM}, \code{DBBMMStack}, \code{DBBMMBurstStack},\code{dBMvariance}, \code{dBMvarianceBurst}, \code{dBMvarianceStack}, \code{dynBGB} or \code{dBGBvariance} object}
  \item{...}{Currently not implemented}
}

\value{
- a \code{numeric} vector of variances if a DBBMM, DBBMMBurstStack, dBMvariance or dBMvarianceBurst object is provided \cr
- a \code{list} of variances per individual if a DBBMMStack or dBMvarianceStack object is provided \cr
- a \code{matrix} of the orthogonal and parallel variances if dynBGB or dBGBvariance object is provided

}

\author{Bart Kranstauber & Anne Scharf}


\seealso{
	\code{\link{brownian.bridge.dyn}}, \code{\link{dynBGB}}, \code{\link{brownian.motion.variance.dyn}}, \code{\link{dynBGBvariance}}, \code{\link{dBMvariance-class}}, \code{\link{dBGBvariance-class}}
}

\examples{
data(leroydbbmm)
data(dbbmmstack)
getMotionVariance(leroydbbmm)[1:50] ## with a DBBMM object
str(getMotionVariance(dbbmmstack)) ## with a DBBMMStack object
}
