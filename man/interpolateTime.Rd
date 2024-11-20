\name{interpolateTime}

\alias{interpolateTime}
\alias{interpolateTime,.MoveTrackSingle,numeric-method}
\alias{interpolateTime,.MoveTrackSingle,POSIXct-method}
\alias{interpolateTime,.MoveTrackSingle,difftime-method}

\docType{methods}

\title{Interpolate a trajectory}

\description{This function allows to interpolate trajectories. It does this on the basis of a simple interpolation, depending on the \code{spaceMethod} that is specified.}

\usage{
interpolateTime(x, time, spaceMethod=c('euclidean','greatcircle','rhumbline'),...)
}

\arguments{
  \item{x}{a \code{\link[=Move-class]{move}} or \code{\link[=MoveBurst-class]{moveBurst}} object.}
  \item{time}{either a number of locations (class \code{numeric}), a time interval (class \code{difftime}) or a vector of timestamps (class \code{POSIXct}) at which the interpolation should be done. See 'Details'.}
  \item{spaceMethod}{a character that indicates the interpolation function (euclidean, great circle or along the rhumb line) to be used to generate the new locations.}
  \item{...}{Currently not implemented.}
}
\details{
In the argument \code{time}:\cr
- number of locations: refer the total number of locations that the resulting track will have distributed equally over time. E.g. if \code{time=200}, the resulting track will have 200 points interpolated at a constant time interval.\cr
- time interval: refers to the time interval at which a location should be interpolated. E.g. if \code{time=as.difftime(10, units="mins")} a location will be interpolated every 10 mins.\cr
- vector of timestamps: the timestamps of this vector have to be in ascending order, and within the time range of the track.
}

\value{
\link{Move-class} object of the interpolated locations.
}

\author{Bart Kranstauber & Anne Scharf}

\examples{
data(leroy)
## providing the number of locations
plot(leroy[100:150,], col="red",pch=20)
points(mv <- interpolateTime(leroy[100:150,], time=500, spaceMethod='greatcircle'))

## providing a time interval
plot(leroy[100:150,], col="red",pch=20)
points(mv2 <- interpolateTime(leroy[100:150,], time=as.difftime(10, units="mins"), 
                              spaceMethod='greatcircle'))

## providing a vector of timestamps
plot(leroy[100:150,], col="red",pch=20)
ts <- as.POSIXct(c("2009-02-13 10:00:00", "2009-02-13 12:00:00", "2009-02-13 14:00:00",
                   "2009-02-13 16:00:00","2009-02-13 18:00:00","2009-02-13 20:00:00",
                   "2009-02-13 22:00:00","2009-02-14 00:00:00","2009-02-14 02:00:00",
                   "2009-02-14 04:00:00","2009-02-14 06:00:00", "2009-02-14 08:00:00", 
                   "2009-02-14 10:00:00"),  format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC")
points(mv3 <- interpolateTime(leroy[100:150,], time=ts, spaceMethod='greatcircle'))

}
