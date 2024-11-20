\name{coordinates}

\alias{coordinates}
\alias{coordinates,Move-method}

\docType{methods}

\title{Extract the track coordinates from a Move or MoveStack object}

\description{The coordinates method extracts the coordinates of a track.}

\usage{
  \S4method{coordinates}{Move}(obj,...)
}

\arguments{
  \item{obj}{a \code{move}, \code{moveStack}, \code{moveBurst}, \code{dBMvariance}, \code{dBMvarianceBurst}, \code{dBMvarianceStack} or \code{dBGBvariance} object}
  \item{...}{Currently not implemented}
}

\value{
Returns a \code{matrix} with the coordinates of the track
}

\examples{
## create a move object
data(leroy)
## extract the coordinates
head(coordinates(leroy))
\dontshow{
data(fishers)
coordinates(fishers)
}}

\author{Marco Smolla & Anne Scharf}
