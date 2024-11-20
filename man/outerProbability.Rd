\name{outerProbability}

\alias{outerProbability}
\alias{outerProbability,RasterLayer-method}
\alias{outerProbability,DBBMMStack-method}

\docType{methods}

\title{Calculates the probabilities at the edges of a raster}

\description{The outerProbability method calculates the summed probability of the cells at the border of a raster}

\usage{
  \S4method{outerProbability}{RasterLayer}(raster,border,...)
  \S4method{outerProbability}{DBBMMStack}(raster,border,...)
}

\arguments{
  \item{raster}{a \code{RasterLayer}, \code{DBBMM}, \code{DBBMMStack}, \code{dynBGB} or \code{.UD} object}
  \item{border}{numeric from 0 to 1; ratio of the number of columns at the border relative to the whole raster from which the probabilities should be summed up; default is 10\% (0.1)}
  \item{...}{Currently not implemented}
}

\details{
 The function returns the summed probability at the border (e.g. the outer 10\% of the cells) of a raster. This value can be used as an indicator whether the extent of the used raster is to small for the UD calculation and therefore too much probabilities are not calculated because they are outside the raster. 
 }

\value{
numeric value for a single DBBMM or dynBGB object, or a list of numeric values for a DBBMMStack
}

\examples{
data(leroydbbmm)
#calculate the probabilities of 20\% of the raster at the border from a DBBMM
outerProbability(leroydbbmm, border=.2)

#calculate the probabilities of 50\% of the raster at the border from a DBBMMStack
outerProbability(leroydbbmm, border=.5)
}
\author{Marco Smolla & Anne Scharf}
