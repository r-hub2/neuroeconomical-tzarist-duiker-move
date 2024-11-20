\name{equalProj}

\alias{equalProj}
\alias{equalProj,list-method}

\docType{methods}

\title{Checks projections for being equal}

\description{Checks whether all objects of a list are in the same projection.}

\usage{
  \S4method{equalProj}{list}(x)
}

\arguments{
  \item{x}{a list of projected objects of class \code{raster}, \code{move}, \code{moveStack}, \code{moveBurst}, \code{DBBMM}, \code{DBBMMStack}, \code{DBBMMBurstStack}, \code{dynBGB}}
}

\details{
  \code{equalProj} checks for equal projections using the function of \code{identicalCRS} from the package sp. It returns TRUE if none of the objects have a \code{proj4string}.
}

\value{
TRUE or FALSE \cr
It returns TRUE if none of the objects have a \code{proj4string}.
}

\author{Bart Kranstauber & Anne Scharf}

\examples{
data(fishers)
ricky<-fishers[['Ricky.T']]
data(leroy)
data(leroydbbmm)

equalProj(list(leroydbbmm,leroydbbmm))
equalProj(list(leroy,leroydbbmm))
equalProj(list(leroy,ricky))
}
