\name{corridor}

\alias{corridor}
\alias{corridor,.MoveTrackSingle-method}
\alias{corridor,.MoveTrackStack-method}

\docType{methods}

\title{Corridor behavior identification}

\description{This function identifies movement track segments whose attributes suggest corridor use behavior}

\usage{
  \S4method{corridor}{.MoveTrackSingle}(x,speedProp=.75, circProp=.25, minNBsegments = 2, plot=FALSE, ...)
  \S4method{corridor}{.MoveTrackStack}(x,speedProp=.75, circProp=.25, minNBsegments = 2, plot=FALSE, ...)
}

\arguments{
  \item{x}{a \code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}} or \code{\link[=MoveBurst-class]{moveBurst}} object.}
  \item{speedProp}{numeric between 0 and 1, defines the proportion of speeds which are high enough to be a valid corridor point (default value selects speeds that are greater than 75 \% of all speeds).}
  \item{circProp}{numeric between 0 and 1, defines the proportion of the circular variances that is low enough to be a valid corridor point. Low values of the circular variance indicate that the segments are (near) parallel (default value selects variances that are lower than 25 \% of all variances).}
  \item{minNBsegments}{numeric equal or larger than 2 representing the minimum number of neiburing corridor segments that each corridor segments has to have (see Details). Default is 2.}
  \item{plot}{logical, if TRUE the track is plotted together with dots that indicate corridor points when a \code{move} object is provided (color scale indicates how many corridor points are near by, few: blue, many: pink).}
  \item{...}{\code{cex} argument can be set specifying the size of the points when \code{plot=TRUE}. Optional.}
}

\details{
  The corridor function uses the attributes of a movement step (segment) to identify movement steps that exhibit corridor use behavior. For each segment, the speed and the azimuth are calculated and assigned to the segment midpoint. \cr
A circular buffer is created around the midpoint of each segment whose radius is equal to half the segment length. The segment azimuth (180 >= azimuth > -180) is converted into a new unit, the 'pseudo-azimuth' (0 <= 360), removing the directional information. \cr
Subsequent, the circular variance of the pseudo-azimuths of all segment midpoints that fall within the circular buffer is calculated. Low values of the circular variance indicate that the segments are (near) parallel. \cr
Next, it is determined whether a segment's speed is higher than \code{speedProp} (by default the upper 25\% speeds) and its circular variance is lower than \code{circProp} (by default the lower 25\% of all variances). \cr
Segment midpoints that meet both of these requirements are considered as a 'corridor' point, all others are considered 'non-corridor' points. Finally, a corridor point is determined to be within a true corridor if within its circular buffer there are more 'corridor' points than 'non-corridor' points.
The argument \code{minNBsegments} can be used to establish the minimum number of 'corridor' points that each circular buffer needs for the focal segment to be defined as a corridor. It is useful to exclude wrongly (visual inspection) identified corridors with only a few segments by increasing the value of \code{minNBsegments}. Note that when increasing the value of \code{minNBsegments}, only segments with enough corridor neighbors are classified as corridors and not all segments that visually seem to fit to be classified as corridors. To remove the wrongly classified corridors, a value of 3 or 4 is usually sufficient.
}

\note{The default values for the \code{speedProp} and \code{circProp} can be changed by the users discretion using the according argument.
}

\value{
  The function returns a \code{\link[=MoveBurst-class]{moveBurst}} object or a list of moveBurst objects (if a MoveStack is supplied). The MoveBurst date.frame stores the following information: \cr
  - segment midpoint \cr
  - speed \cr
  - azimuth \cr
  - pseudo-azimuth \cr
  - circular variance \cr \cr 
  The object is bursted by the factor that indicates whether the segment belongs to a corridor segment or not, and is specified in the "burstId" slot. 
}

\author{Marco Smolla & Anne Scharf}

\references{
  LaPoint, S., Gallery, P., Wikelski, M. and Kays, R. (2013), Animal Behavior, Cost-based Corridor Models, and Real Corridors. Landscape Ecology. doi:10.1007/s10980-013-9910-0.
}

\examples{
if(requireNamespace("circular") ){
## with a move object 
  data(leroy)  
  tmp <- corridor(leroy, plot=TRUE)
  plot(tmp, type="l", col=c("red","black")[c(tmp@burstId,NA)])
  
## with a moveStack object   
  data(fishers)
  stacktmp <- corridor(fishers[c(1:400,sum(n.locs(fishers))-(400:1)),])
  plot(stacktmp[[2]], col=c("red","black")[stacktmp[[2]]@burstId])
  lines(stacktmp[[2]], col=c("red","black")[c(stacktmp[[2]]@burstId,NA)])
}
}