\name{split}

\alias{split}
\alias{split,MoveStack,missing-method}
\alias{split,.MoveTrackSingleBurst,missing-method}
\alias{split,.MoveTrackStack,missing-method}
\alias{split,DBBMMStack,missing-method}
\alias{split,.UDStack,missing-method}

\docType{methods}

\title{Splitting a MoveStack, MoveBurst or DBBMMStack}

\description{Splitting a MoveStack or MoveBurst into a list of Move objects. Splitting a DBBMMStack into a list of DBBMM objects.}

\usage{
  \S4method{split}{MoveStack,missing}(x, f, drop=FALSE, ...)
}

\arguments{
  \item{x}{a \code{\link[=MoveStack-class]{moveStack}}, \code{\link[=MoveBurst-class]{moveBurst}} or \code{\link[=DBBMMStack-class]{DBBMMStack}} object}
  \item{f}{not needed}
  \item{drop}{not needed}
  \item{...}{Currently not implemented}
}

\details{
 A \code{MoveStack} is split into a list of \code{\link[=Move-class]{Move}} objects by the trackId slot of the given \code{MoveStack}, obtaining one move object per unique trackId (usually corresponding to animal names). For staking this list of move objects use \code{\link{moveStack}}.

 A \code{MoveBurst} object is split into a list of \code{\link[=Move-class]{Move}} objects by the burstId slot of the given \code{MoveBurst}. One move object per burst (e.g. segment with given behavior) is obtained. Every location where the burst is switched will be recycled.
 
 A \code{DBBMMStack} is split into a list of \code{\link[=DBBMM-class]{DBBMM}} objects by the trackId slot of the given \code{DBBMMStack}.
}

\value{'\code{list}'}

\note{
 After splitting any object, the coordinates in the @coords slot in the resulting objects are named "coords.x1" and "coords.x2" (due to the usage of functions of other packages within this function).
}

\author{Marco Smolla & Anne Scharf}

\examples{
## splitting a MoveStack
data(fishers)
split(fishers)

## splitting a DBBMMStack
data(dbbmmstack)
split(dbbmmstack)

## splitting a MoveBurst
data(leroy)
behav <- c(rep(c("a","b","c","a"),each=200), rep("b", 118))
leroyBurst <- burst(x=leroy, f=behav)
split(leroyBurst)
}
