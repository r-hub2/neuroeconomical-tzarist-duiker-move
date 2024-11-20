\name{citations}

\alias{citations}
\alias{citations<-}
\alias{citations,.MoveGeneral-method}
\alias{citations<-,.MoveGeneral-method}

\docType{methods}

\title{Extract the citation of a Move or MoveStack object}

\description{The citations method returns or sets the citation of a track from a Move or MoveStack object.}

\usage{
  \S4method{citations}{.MoveGeneral}(obj)
  \S4method{citations}{.MoveGeneral}(obj) <- value
}

\arguments{
  \item{obj}{a \code{move}, \code{moveStack} or \code{moveBurst} object}
  \item{value}{citation with class character}
}
\value{
character string of the citation
}
\seealso{\link{licenseTerms}}

\author{Marco Smolla & Anne Scharf}

\examples{
  data(leroy)
  citations(leroy) #get the citation from a Move object
  citations(leroy) <- "No paper available" #change the citation and set it for a Move object
 
  data(fishers)
  citations(fishers) #get the citation from a MoveStack object
  citations(fishers) <- "Nothing to cite" #change the citation and set it for a MoveStack object
}
