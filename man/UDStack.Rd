\name{UDStack}
\alias{UDStack}
\alias{UDStack,RasterBrick-method}
\alias{UDStack,list-method}
\alias{UDStack,RasterStack-method}
\alias{UDStack,.UDBurstStack-method}

\title{Creating UDStack objects}
\description{The function enables the easy generation of .UDStacks, which is for example useful for using other UD function such as \link{getVolumeUD}.}
\usage{
UDStack(x,...)
}
\arguments{
  \item{x}{A list of \code{rasters}, a \code{rasterBrick}, a \code{rasterStack} or a \code{DBBMMBurstStack} object that needs to be converted to a .UDStack object.}
  \item{...}{Currently not used}
}
\details{
    The values of a \code{DBBMMBurstStack} are standardized per raster layer.
}
\value{An \link[=.UD-class]{UDStack} object}

\author{Bart Kranstauber & Anne Scharf}

\examples{
data(dbbmmstack)
stk<-as(dbbmmstack,"RasterStack")
UDStack(stk)
lst<-split(dbbmmstack)
UDStack(lst)

## transforming a DBBMMBurstStack into UDStack, e.g. to than 
## use the "getVolumeUD" or "emd" function
data(leroy)
leroyB <- burst(x=leroy,f=c(rep(c("Behav.1","Behav.2"),each=400),rep("Behav.1", 118)))
leroyBdbb <- brownian.bridge.dyn(object=spTransform(leroyB[785:820], center=TRUE), 
                                 location.error=12, dimSize=115, ext=.45, 
                                 time.step=25/15, margin=15)
cellStats(leroyBdbb, sum)
leroyBud <- UDStack(leroyBdbb)
cellStats(leroyBud, sum)
}
