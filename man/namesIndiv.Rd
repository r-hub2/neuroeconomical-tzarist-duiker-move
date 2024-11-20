\name{namesIndiv}

\alias{namesIndiv}
\alias{namesIndiv,.MoveTrackSingle-method}
\alias{namesIndiv,.MoveTrackStack-method}

\docType{methods}

\title{Extract the names of the individuals of a move or moveStack object}

\description{This function returns the names of the individuals from a move or moveStack object.}

\usage{
  namesIndiv(obj)
}

\arguments{
  \item{obj}{a \code{move} or \code{moveStack} object}
}
\value{Returns the name as a character for a \code{move} object, and the names as a character vector from a \code{moveStack} object. \cr 
If no name has been provided when creating the \code{move} object, "unnamed" will be returned.} 
\author{Anne Scharf}

\examples{
data(leroy)
namesIndiv(leroy) 

data(fishers)
namesIndiv(fishers)
}
