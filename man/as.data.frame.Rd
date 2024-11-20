\name{as.data.frame}

\alias{as.data.frame}
\alias{as.data.frame,Move-method}
\alias{as.data.frame,MoveStack-method}
\alias{as.data.frame,MoveBurst-method}
\alias{as.data.frame,dBMvariance-method}
\alias{as.data.frame,.unUsedRecords-method}
\alias{as.data.frame,.unUsedRecordsStack-method}
  
\docType{methods}

\title{Returns a Data Frame}

\description{Function to create a \code{data.frame} of a Move, dBMvariance, dBGBvariance, .unUsedRecords object.}

\usage{
  \S4method{as.data.frame}{Move}(x)
  \S4method{as.data.frame}{MoveStack}(x)
  \S4method{as.data.frame}{MoveBurst}(x)
  
  \S4method{as.data.frame}{dBMvariance}(x)
 
  \S4method{as.data.frame}{.unUsedRecords}(x)
  \S4method{as.data.frame}{.unUsedRecordsStack}(x)
}

\arguments{
  \item{x}{a \code{move}, \code{moveStack}, \code{moveBurst}, \code{dBMvariance}, \code{dBMvarianceStack}, \code{dBMvarianceBurst}, \code{dBGBvariance},\code{.unUsedRecords} or \code{.unUsedRecordsStack} object}
}

\details{
 Depending on the class of the object provided, the obtained \code{data.frame} contains the information contained in the slots:
  \itemize{
    \item if class \code{move}: "timestamps", "idData", "sensor", "data", "coords".
    \item if class \code{moveStack}: "timestamps", "idData", "sensor", "data", "coords", "trackId".
    \item if class \code{moveBurst}: "timestamps", "idData", "sensor", "data", "coords", "burstId".
    \item if class \code{dBMvariance}: "timestamps", "sensor", "data", "coords", "window.size", "margin", "means", "in.windows", "interest".
    \item if class \code{dBMvarianceStack}: "timestamps", "sensor", "data", "coords", "window.size", "margin", "means", "in.windows", "interest","trackId".
    \item if class \code{dBMvarianceBurst}: "timestamps", "sensor", "data", "coords", "window.size", "margin", "means", "in.windows", "interest", "burstId".
    \item if class \code{dBGBvariance}: "timestamps", "sensor", "data", "coords", "paraSd", "orthSd", "margin", "windowSize".
    \item if class \code{.unUsedRecords}: "dataUnUsedRecords", "timestampsUnUsedRecords", "sensorUnUsedRecords".
    \item if class \code{.unUsedRecordsStack}: "trackIdUnUsedRecords", "dataUnUsedRecords", "timestampsUnUsedRecords", "sensorUnUsedRecords".
    }
}

\value{
'\code{data.frame}'
}

\examples{
## obtain data.frame from move object
data(leroy)
head(as.data.frame(leroy))

## obtain data.frame from moveStack object
data(fishers)
head(as.data.frame(fishers))

## obtain data.frame from .unUsedRecordsStack object
unUsedFishers <- unUsedRecords(fishers)
head(as.data.frame(unUsedFishers))
}

\author{Marco Smolla & Anne Scharf}
