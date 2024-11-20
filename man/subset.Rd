\name{subset-method}

\alias{[,dBGBvariance,ANY,ANY-method}
\alias{[,MoveStack,ANY,ANY-method}
\alias{[,.MoveTrack,ANY,ANY-method}
\alias{[<-,.MoveTrack,ANY,ANY-method}
\alias{[,.MoveTrackSingleBurst,ANY,ANY-method}
\alias{[,.MoveTrackStack,ANY,ANY-method}
\alias{[,dBMvariance,ANY,ANY-method}
\alias{[,dBMvarianceStack,ANY,ANY-method}
\alias{[,dBMvarianceBurst,ANY,ANY-method}
\alias{[,.unUsedRecordsStack,ANY,ANY-method}
\alias{[,.unUsedRecords,ANY,ANY-method}
\alias{[[,MoveStack,character,missing-method}
\alias{[[,.MoveTrackStack,character,missing-method}
\alias{[[,.MoveTrackStack,logical,missing-method}
\alias{[[,.MoveTrackStack,numeric,missing-method}

\docType{methods}

\title{Subset movement tracks}

\description{Extraction of a subset of locations or individuals from a movement track.}

\usage{
  \S4method{[}{MoveStack,ANY,ANY}(x,i)
  \S4method{[[}{MoveStack,character,missing}(x,i)
}

\arguments{
  \item{x}{a \code{move}, \code{moveStack}, \code{moveBurst}, \code{DBBMM}, \code{DBBMMStack}, \code{DBBMMBurstStack}, \code{dynBGB}, \code{dBMvariance}, \code{dBMvarianceBurst}, \code{dBMvarianceStack} or \code{dBGBvariance} object}
  \item{i}{numeric, character or logical vector for individuals in a stack or a set of locations}
}

\details{
The single square bracket method is used to select coordinates from a Move* object.
The double square bracket method is used for sub setting a moveStack to a single move object according to the individual name or return a stack of multiple individuals. 
}

\value{same object class as the input containing the selected locations or individuals}

\examples{
## subseting a Move, MoveBurst, DBBMM, dBMvariance, dBMvarianceBurst, 
## dBMvarianceStack or dBGBvariance object by locations
data(leroy)
leroy[1:20,] # subset to selected range of coordinates of a move objects
leroy[c(1,10,20),] # subset to selected coordinates of a move objects
leroy[c(TRUE,FALSE),] # subset to every second location
leroy[c(TRUE,FALSE,FALSE),] # subset to every third location

## subseting a moveStack, DBBMMStack or DBBMMBurstStack object, 
## by locations 
data(fishers)
# subset to selected range of coordinates of a moveStack objects. If the first individual contains
# more than, in this case 300, locations, only locations of the fist individual will be returned
fishers[1:300,] 
fishers[1] # returns first location of first individual

## or individuals
fishers[['Ricky.T']] # returns move object of named individual
fishers[[c('Leroy','Ricky.T')]] # returns subseted moveStack only with the named individual
fishers[[2]] # returns move object of 2nd individual
fishers[[c(1,2)]]# returns subseted moveStack only with the selected individual

fishers[[c(TRUE,FALSE)]] # returnes move or moveStack object with those individuals that are 'TRUE'
}

\author{Bart Kranstauber & Anne Scharf}
