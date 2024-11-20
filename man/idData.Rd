\name{idData}

\alias{idData}
\alias{idData<-}
\alias{idData<-,.MoveTrack,ANY,ANY,ANY-method}
\alias{idData<-,.MoveTrack,missing,missing,data.frame-method}
\alias{idData,.MoveTrack-method}

\docType{methods}

\title{Obtain or replace the idData slot of a Move object}

\description{This function returns or replaces the idData slot of a Move, MoveStack or MoveBurst object.}

\usage{
  \S4method{idData}{.MoveTrack}(x,i,j,...)

  \S4method{idData}{.MoveTrack,missing,missing,data.frame}(x,i,j) <- value
  \S4method{idData}{.MoveTrack,ANY,ANY,ANY}(x,i,j) <- value
}

\arguments{
  \item{x}{a \code{move}, \code{moveStack} or \code{moveBurst} object}
  \item{i}{Selection of the rows}
  \item{j}{Selection for the columns}
  \item{value}{Replacement values for the selected idData}
  \item{...}{Other arguments to the data frame subsetting such as drop=F}
}

\author{Bart Kranstauber & Anne Scharf}

\value{
	Either the idData \code{data.frame} or the modified move object
}

\examples{
data(fishers)
idData(fishers)

## obtain e.g. only the tag and individual identifier columns
idData(fishers, j=c(6,7))
idData(fishers, j=c("tag.local.identifier", "individual.local.identifier"))
}
