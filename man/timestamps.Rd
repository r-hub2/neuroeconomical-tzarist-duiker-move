\name{timestamps}

\alias{timestamps}
\alias{timestamps<-}
\alias{timestamps,.MoveTrack-method}
\alias{timestamps,.unUsedRecords-method}
\alias{timestamps,.MoveTrackSingle-method}
\alias{timestamps<-,.MoveTrack-method}

\docType{methods}

\title{Extract or set the timestamps of a Move or MoveStack object}

\description{The timestamps method returns or sets the timestamps of a track from a Move or MoveStack object.}

\usage{
  \S4method{timestamps}{.MoveTrackSingle}(this)
  \S4method{timestamps}{.MoveTrack}(this)
  \S4method{timestamps}{.MoveTrack}(this) <- value
}

\arguments{
  \item{this}{\code{\link[=Move-class]{move}}, \code{\link[=MoveStack-class]{moveStack}}, \code{\link[=MoveBurst-class]{moveBurst}}, \code{\link[=.unUsedRecords-class]{.unUsedRecords}} or \code{\link[=.unUsedRecords-class]{.unUsedRecordsStack}} object}
  \item{value}{timestamps from class POSIXct}
}

\value{
vector of class \code{POSIXct}. \cr
Note that for moveStacks a single string is returned without splitting by individual (see 'Examples').
}

\author{Marco Smolla & Anne Scharf}

\examples{
data(leroy)
data(fishers)

## get the timestamps from a Move object
head(timestamps(leroy)) 
## get the timestamps from a MoveStack object
head(timestamps(fishers)) 
## get the timestamps from a unUsedRecords object
head(timestamps(unUsedRecords(leroy)))  

## get timestamps separatly for each individual from a MoveStack 
str(lapply(split(fishers), timestamps))

## change the timestamps and set it for a Move object
timestamps(leroy) <- timestamps(leroy)+60 
## change the timestamps and set it for a MoveStack object
timestamps(fishers) <- timestamps(fishers)+60.1
}
