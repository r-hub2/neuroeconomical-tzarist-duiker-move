\name{licenseTerms}

\alias{licenseTerms}
\alias{licenseTerms<-}
\alias{licenseTerms,.MoveGeneral-method}
\alias{licenseTerms<-,.MoveGeneral-method}

\docType{methods}

\title{Extract the license terms of a Move or MoveStack object}

\description{The licenseTerms method returns or sets the license terms of a track from a Move or MoveStack object.}

\usage{
  \S4method{licenseTerms}{.MoveGeneral}(obj)
  \S4method{licenseTerms}{.MoveGeneral}(obj) <- value
}

\arguments{
  \item{obj}{a \code{move}, \code{moveStack} or \code{moveBurst} object}
  \item{value}{license terms with class character}
}
\value{
character string of the license terms
}
\seealso{\link{citations}}

\author{Anne Scharf}

\examples{
  data(leroy)
  licenseTerms(leroy) #get the license from a Move object
  
  ## change the license and set it for a Move object
  licenseTerms(leroy) <- "use of data only permitted after obtaining licence from the PI" 
 
  data(fishers)
  licenseTerms(fishers) #get the license from a MoveStack object
  
  ## change the license and set it for a MoveStack object
  licenseTerms(fishers) <- "use of data only permitted after obtaining licence from the PI"
}
