\name{trackId}

\alias{trackId}
\alias{trackId,.MoveTrackStack-method}
\alias{trackId,.unUsedRecordsStack-method}
\alias{trackId,MoveStack-method}

\docType{methods}

\title{Returns trackId}

\description{Obtain the Id of the individual per location of a MoveStack or unUsedRecordsStack}

\usage{
  \S4method{trackId}{MoveStack}(x)
}

\arguments{
  \item{x}{a \code{moveStack} or \code{.unUsedRecordsStack} object}
}

\value{
	Returns a factor indicating for each location to which individual it belongs.
}

\author{Bart Kranstauber}

\examples{
data(fishers)
head(trackId(fishers))
head(trackId(unUsedRecords(fishers)))
}
