\name{raster}

\alias{raster}
\alias{raster,DBBMM-method}
\alias{raster,DBBMMStack-method}

\docType{methods}

\title{Extract raster topology from DBBMM or dynBGB}

\description{Extracts the RasterLayer topology from a DBBMM, DBBMMStack and dynBGB object.}

\usage{
  \S4method{raster}{DBBMM}(x)
  \S4method{raster}{DBBMMStack}(x)
}

\arguments{
  \item{x}{a \code{DBBMM}, \code{DBBMMStack} or \code{dynBGB} object}
}

\details{
  This function extracts the raster topology (i.e. without values) of the input object. DBBMM, DBBMMStack and dynBGB objects can be directly used in most raster functions but in case a raster with values needs to be extracted \code{as(x,'RasterLayer')} can be used.
}

\value{
  An object from class \code{RasterLayer} is returned.
}

\author{Marco Smolla & Anne Scharf}

\examples{
data(leroydbbmm)
data(dbbmmstack)
raster(leroydbbmm) #returns the raster topology of a DBBMM object
raster(dbbmmstack) # returns the raster topology of a DBBMMStack object
}
