\name{getVolumeUD}

\alias{getVolumeUD}
\alias{getVolumeUD,.UD-method}
\alias{getVolumeUD,.UDStack-method}

\docType{methods}

\title{Utilization distribution (UD)}

\description{The UD represents the minimum area in which an animal has some specified probability of being located (Cumulative Distribution Function).}

\usage{
  \S4method{getVolumeUD}{.UD}(x, ...)
}

\arguments{
  \item{x}{a \code{DBBMM}, \code{DBBMMStack}, \code{dynBGB}, \code{.UD} or \code{.UDStack} object}
  \item{...}{when \code{x} is of class DBBMM or dynBGB, several objects of class DBBMM or dynBGB can be added (see 'Examples')}
}

\note{
To obtain this modified UD raster from a \code{DBBMMBurstStack} object, transform the object with the \link{UDStack} function into a '.UDStack' class, and than use the \code{getVolumeUD} function upon the obtained object.
}

\value{
'\code{raster}' or '\code{rasterStack}' \cr
If several objects are provided, a list of rasters is returned
}

\author{Marco Smolla & Anne Scharf}

\seealso{
\link{raster2contour}, \link{contour}, \link{UDStack}
}

\examples{
data(leroydbbmm)
data(dbbmmstack)
data(leroydbgb)
getVolumeUD(leroydbbmm) # for a DBBMM object
getVolumeUD(dbbmmstack) # for a DBBMMStack object 
getVolumeUD(leroydbgb) # for a dynBGB object 

getVolumeUD(leroydbbmm, leroydbgb) # for several objects 

plot(getVolumeUD(leroydbbmm))

## e.g. select the raster corresponding to the 95\% UD
leroyUD <- getVolumeUD(leroydbbmm)
leroyUD[leroyUD>0.95] <- NA
plot(leroyUD)

}
