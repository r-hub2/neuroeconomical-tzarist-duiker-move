\name{sensor}
\alias{sensor}
\alias{sensor,.MoveTrack-method}
\alias{sensor,.unUsedRecords-method}

\docType{methods}

\title{Extract the sensor of a Move unUsedRecords object}

\description{Extracts the sensor(s) used to record the locations of a track from a Move or unUsedRecords object.}

\usage{
\S4method{sensor}{.MoveTrack}(this,...)
\S4method{sensor}{.unUsedRecords}(this,...)
}

\arguments{
  \item{this}{a \code{move}, \code{moveStack}, \code{moveBurst}, \code{.unUsedRecords} or \code{.unUsedRecordsStack} object}
  \item{...}{Currently not used}
}

\value{
'\code{factor}' with the sensor(s) name(s).\cr
Note that the returned vector for a MoveStack or .unUsedRecordsStack is not split between animals. 
}

\author{Bart Kranstauber}

\examples{
data(leroy)
head(sensor(leroy)) ## get the sensor from a Move object

head(sensor(unUsedRecords(leroy))) ## get the sensor from the unused records of a Move object

data(fishers)
head(sensor(fishers)) ## get the sensor from a MoveStack object
}
