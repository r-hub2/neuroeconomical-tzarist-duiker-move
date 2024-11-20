\name{emd}

\alias{emd}
\alias{emd,RasterLayer,RasterLayer-method}
\alias{emd,RasterStackBrick,missing-method}
\alias{emd,RasterStackBrick,RasterStackBrick-method}
\alias{emd,SpatialPoints,SpatialPoints-method}

\docType{methods}

\title{Earth movers distance}

\description{The earth mover's distance (EMD) quantifies similarity between utilization distributions by calculating the effort it takes to shape one utilization distribution landscape into another}

\usage{
  \S4method{emd}{SpatialPoints,SpatialPoints}(x,y, gc = FALSE, threshold = NULL,...)
  \S4method{emd}{RasterLayer,RasterLayer}(x,y, ...)
}

\arguments{
  \item{x}{A \code{Raster}, \code{RasterStack}, \code{RasterBrick}, \code{SpatialPoints}, \code{SpatialPointsDataFrame}, \code{\link[=DBBMM-class]{DBBMM}} or \code{\link[=DBBMMStack-class]{DBBMMStack}} object. These objects can represent a probability surface, i.e. comparability  is easiest when the sum of values is equal to 1. In the case of a SpatialPointsDataFrame the first column of the data is used as weights. In the case of SpatialPoints all points are weighted equally.}
  \item{y}{same class as provided in '\code{x}', with the exception of RasterStack, RasterBrick and DBBMMStack, where in order to compare the utilization distributions stored within the layers of one object this argument can be left empty. Alternatively another set of rasters can be provided to compare with.}
  \item{gc}{logical, if FALSE (default) euclidean distances are calculated, if TRUE great circle distances will be used. See 'Details'.}
  \item{threshold}{numeric, the maximal distance (in map units) over which locations are compared.}
  \item{...}{Currently not used}
}
\details{
For easy interpretation of the results the utilization distributions objects compared should represent a probability surface, i.e. the sum of their values is equal to 1. Nevertheless there is also the possibility to provide utilization distributions objects with the same volume, i.e. the sum of their values is equal to the same number. In the later case interpretation of the results is probably less intuitive.

Euclidean distances are suitable for most planar spatial projections, while great circle distances, calculated using the Haversine function, could be used to compare probability distributions stretching over larger geographical distances taking into account the spherical surface of the Earth.

The function can be optimized by omitting locations that have negligible contribution to the utilization density; for example, EMD can be calculated only for the cells within the 99.99\% contour of the UD. This will maximally introduce a very small error in the EMD because only small amounts of probability were omitted, but often, given the long tail of most UDs, many cells are omitted, which greatly reduces the complexity. See 'Examples'.

For more details of the method see 'References'.
}

\value{An \code{matrix} of distances of the class 'dist'}

\author{Bart Kranstauber & Anne Scharf}
\references{Kranstauber, B., Smolla, M. and Safi, K. (2017), Similarity in spatial utilization distributions measured by the earth mover's distance. Methods Ecol Evol, 8: 155-160. doi:10.1111/2041-210X.12649}

\examples{
## with a DBBMMStack object
  data(dbbmmstack)
  ## to optimize the calculation, the cells outside of the 99.99\% UD contour 
  # are removed by setting them to zero.
  values(dbbmmstack)[values(getVolumeUD(dbbmmstack))>.999999]<-0
  ## transform each layer to a probability surface (i.e. sum of their values is 1)
  stk<-(dbbmmstack/cellStats(dbbmmstack,sum))
  emd(stk[[1]],stk[[2]])
  emd(stk)
  emd(stk, threshold=10000)
  
## with a SpatiaPointsDataFrame 
  x<-SpatialPointsDataFrame(cbind(c(1:3,5),2), data=data.frame(rep(.25,4)))
  y<-SpatialPointsDataFrame(coordinates(x), data.frame(c(0,.5,.5,0)))
  emd(x,y)
  emd(x,y,threshold=.1)

## with a DBBMMBurstStack object, to compare the utilization 
# distributions of e.g. different behaviors
  data(leroy)
  leroyB <- burst(x=leroy,f=c(rep(c("Behav.1","Behav.2"),each=400),rep("Behav.1", 118)))
  leroyBp <- spTransform(leroyB, CRSobj="+proj=aeqd +ellps=WGS84", center=TRUE)
  leroyBdbb <- brownian.bridge.dyn(object=leroyBp[750:850], location.error=12, raster=600, 
                                   ext=.45, time.step=15/15, margin=15)
  
  ## transform the DBBMMBurstStack into a UDStack
  leoryBud <- UDStack(leroyBdbb)
  ## to optimize the calculation, the cells outside of the 99.99\% UD contour 
  # are removed by setting them to zero.
  values(leoryBud)[values(getVolumeUD(leoryBud))>.999999]<-0
  ## transform each layer to a probability surface (i.e. sum of their values is 1)
  stk2<-(leoryBud/cellStats(leoryBud,sum))
  emd(stk2)
}
