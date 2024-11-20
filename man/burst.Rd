\name{burst}

\alias{burst}
\alias{burst,ANY,character-method}
\alias{burst,Move,factor-method}
\alias{burst,ANY,numeric-method}
\alias{burst,.MoveTrackSingleBurst,missing-method}
\alias{burst,.MoveTrackSingleBurst,factor-method}

\docType{methods}

\title{Bursting a track}

\description{Bursting a track by a specified variable}

\usage{
  \S4method{burst}{Move,factor}(x, f, ...)
}

\arguments{
  \item{x}{a \code{\link[=Move-class]{move}} object}
  \item{f}{a character, factor, or numeric vector that indicates how to burst the coordinates of a Move object. It must be one shorter than the number of locations, because there are always one less segments of a track than coordinates}
  \item{...}{Currently not implemented}
}

\details{
The burst function bursts (divides) a track into segments that are specified by the burstIDs (e.g. behavioral annotations). It allows to investigate different parts of a track according to supplied variables like day and night, movement and rest, and so on.  
}
\value{
 a '\code{\link[=MoveBurst-class]{moveBurst}}' object
}

\seealso{ 
\link{burstId}, \link{split}, \link{plotBursts}
}

\author{Marco Smolla}

\examples{
data(leroy)
behav <- c(rep(c("B1","B2","B3","B2"),each=200), rep("B1", 118))
testb <- burst(x=leroy, f=behav)
plot(testb, type="l")
}
