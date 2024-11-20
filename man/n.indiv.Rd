\name{n.indiv}

\alias{n.indiv}
\alias{n.indiv,Move-method}
\alias{n.indiv,.MoveTrackStack-method}

\docType{methods}

\title{Extract the number of individuals of a MoveStack}

\description{This function returns the number of individuals from a MoveStack object.}

\usage{
  \S4method{n.indiv}{Move}(obj)
  \S4method{n.indiv}{.MoveTrackStack}(obj)
}

\arguments{
  \item{obj}{a \code{move} or \code{moveStack} object}
}
\value{Returns the number of individuals. \cr
It will be always 1 for objects of the class Move}
\author{Bart Kranstauber}

\examples{
data(leroy)
n.indiv(leroy) 

data(fishers)
n.indiv(fishers)
}
