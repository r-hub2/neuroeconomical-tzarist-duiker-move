\name{unUsedRecords<-}

\alias{unUsedRecords<-}
\alias{unUsedRecords}
\alias{unUsedRecords<-,.MoveTrackSingle,logical-method}
\alias{unUsedRecords<-,.MoveTrackStack,logical-method}
\alias{unUsedRecords,.unUsedRecords-method}
\alias{unUsedRecords,.unUsedRecordsStack-method}

\title{Extracts or creates the unUsedRecords}

\description{
  This function returns the unUsedRecords part of the move object or assigns locations as unused, this could for example be used to remove test locations from a track. unUsedRecords can include events with no locations, locations flagged as outliers, non-location sensor data when \code{includeExtraSensors} is set to TRUE in the \code{getMovebankData} function. 
}

\usage{
 \S4method{unUsedRecords}{.unUsedRecords}(obj,...)
 \S4method{unUsedRecords}{.unUsedRecordsStack}(obj,...)
  
 \S4method{unUsedRecords}{.MoveTrackSingle,logical}(obj) <- value
 \S4method{unUsedRecords}{.MoveTrackStack,logical}(obj) <- value
}

\arguments{
  \item{obj}{a \code{move}, \code{moveStack} or \code{moveBurst} object}
  \item{value}{A logical vector of the same length as the number of locations}
  \item{...}{Currently not implemented}
}
\value{
an \code{\link[=.unUsedRecords-class]{.unUsedRecords}} or \code{\link[=.unUsedRecords-class]{.unUsedRecordsStack}} object
}

\examples{
data(leroy)
data(fishers)

## get unused records from a move or moveStack object
str(unUsedRecords(leroy)) # from a move object
str(unUsedRecords(fishers)) # from a moveStack object

## assign locations of a move object as unused record
par(mfrow=2:1)
plot(leroy, type='b')
# e.g. assign every second location as unused
unUsedRecords(leroy)<-as.logical((1:n.locs(leroy))\%\%2) 
plot(leroy, type='b')

# e.g. assign first 20 locations as unused
data(leroy)
unUsedRecords(leroy)<- as.logical(c(rep("TRUE",20), rep("FALSE",n.locs(leroy)-20)))
}

\author{Marco Smolla & Anne Scharf}
