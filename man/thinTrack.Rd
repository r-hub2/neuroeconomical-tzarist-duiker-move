\name{thinTrackTime}
\alias{thinTrackTime}
\alias{thinDistanceAlongTrack}
\alias{thinTrackTime,.MoveTrackSingle-method}
\alias{thinDistanceAlongTrack,.MoveTrackSingle-method}

\title{
Thinning trajectories to a specific time interval or distance.
}
\description{
These functions thin trajectories, by selecting segments from the original track with a fixed time interval or distance.
Finding all segments of a specific time interval might for example be useful for fitting step selection functions.
}
\usage{
thinTrackTime(x, interval = NA, tolerance = NA,
  criteria = c("closest", "first", "all"), ...)
thinDistanceAlongTrack(x, interval = NA, tolerance = NA,
  criteria = c("closest", "first", "all"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{move} object}
  \item{interval}{in \code{thinTrackTime} a object of class \code{difftime} specifying a time interval. See 'Examples'.\cr
  in \code{thinDistanceAlongTrack} a numeric value specifying a distance. The units will correspond to the map units. If the coordinates are in long/lat, than the value should be provided in meters.}
  \item{tolerance}{in \code{thinTrackTime} a object of class \code{difftime} specifying the tolerance of the specified \code{interval}. See 'Examples'.\cr
  in \code{thinDistanceAlongTrack} a numeric value specifying the tolerance of the specified \code{interval}}
  \item{criteria}{the criteria ("closest", "first" or "all") to be used when multiple solutions are available. Default is "closest".}
  \item{...}{Currently not implemented.}
}

\details{The functions search for consecutive segments with a cumulative sum of the time lag (or distance) corresponding to interval and tolerance values. From each selected chunk of the track, only the first and last location are kept in the new object, this new segment is labeled with "selected". The segments labeled as "notSelected" are those parts of the track that did not fulfill the indicated interval. A "notSelected" burst can correspond to multiple consecutive segments that have a larger timelag than the one specified, or a single large time gap that is present in the original data.\cr
Note that in the case of \code{thinDistanceAlongTrack}, the distances between the locations in the new object do not represent the distance that the animal actually traveled, as the intermediate location are removed.}

\note{This function finds the maximal number of segments that meet the criteria but does not ensure that the average matches the set interval.}
\value{
A \code{\link[=MoveBurst-class]{MoveBurst}} object, with segments labeled either 'selected' or 'notSelected', only the selected segments match the criteria set in the function call.

A list of \code{\link[=MoveBurst-class]{MoveBurst}} objects will all possible solutions if the criteria is set to "all".
}
\author{
Bart Kranstauber & Anne Scharf
}


\seealso{
\code{\link{interpolateTime}}
}
\examples{
data("leroy")
leroysub <- leroy[1:200]
### selecting those segments that have a time interval of 15mins pulsminus 5mins
thintime <- thinTrackTime(leroysub, interval = as.difftime(15, units='mins'),
                          tolerance = as.difftime(5, units='mins'))
summary(timeLag(thintime,"mins")[thintime@burstId=="selected"])

### selecting those segments that have a distance of 100m pulsminus 10m
thindist <- thinDistanceAlongTrack(leroysub, interval = 100, tolerance = 10)
summary(distance(thindist)[thindist@burstId=="selected"])

\dontshow{
timeLag(thinTrackTime(move(1:40, 1:40, Sys.time()+1:40), 
as.difftime(5, units='secs'), as.difftime(1, units='secs')), units='secs')

timeLag(thinTrackTime(move(1:41, 1:41, Sys.time()+1:41), 
as.difftime(5, units='secs'), as.difftime(1, units='secs')), units='secs')
}
}
