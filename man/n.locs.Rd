\name{n.locs}

\alias{n.locs}
\alias{n.locs,SpatialPointsDataFrame-method}
\alias{n.locs,.MoveTrackStack-method}

\docType{methods}

\title{Extract the number of locations of a Move or MoveStack object}

\description{This function returns the number of locations of a track from a Move or MoveStack object.}

\usage{
  \S4method{n.locs}{SpatialPointsDataFrame}(obj)
  \S4method{n.locs}{.MoveTrackStack}(obj)
}

\arguments{
  \item{obj}{a \code{move}, \code{moveStack} or \code{moveBurst} object}
}

\value{
number of locations.\cr
If a \code{MoveStack} is provided, the number of locations per individual is returned.
}

\author{Marco Smolla}

\examples{
data(leroy)
data(fishers)
n.locs(leroy) # of Move object
n.locs(fishers) # of MoveStack object
}
