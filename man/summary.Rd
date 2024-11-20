\name{summary}

\alias{summary}
\alias{summary,.UD-method}
\alias{summary,.UDStack-method}

\docType{methods}

\title{Summary of Move, DBBMM, dynBGB objects}

\description{Summarizes the information contained in the input object}

\usage{
  \S4method{summary}{.UD}(object)
  \S4method{summary}{.UDStack}(object)
}

\arguments{
  \item{object}{\code{move}, \code{moveStack}, \code{moveBurst}, \code{DBBMM}, \code{DBBMMStack}, \code{DBBMMBurstStack}, \code{dynBGB}, \code{dBMvariance}, \code{dBMvarianceBurst}, \code{dBMvarianceStack},\code{dBGBvariance}, \code{.UD}, \code{.UDStack} or \code{.UDBurstStack} object}
}

\details{
Returns the projection, extent, and maximum and minimum values of the raster stored within the DBBMM, DBBMMStack, dynBGB, .UD, .UDStack or .UDBurstStack object. For the remaining objects it returns a summary of the data contained in the '\code{@data}' slot. 
}

\author{Marco Smolla & Anne Scharf}

\examples{
data(leroy)
summary(leroy) # summary of a move object
data(leroydbbmm)
summary(leroydbbmm) # summary of a DBBMM object
}
