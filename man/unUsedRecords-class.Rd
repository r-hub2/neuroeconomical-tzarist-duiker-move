\name{.unUsedRecords-class}

\alias{.unUsedRecords-class}
\alias{.unUsedRecordsStack-class}

\docType{class}

\title{The .unUsedRecords and .unUsedRecordsStack class}

\description{The \code{.unUsedRecords} and \code{.unUsedRecordsStack} object stores unused records in \code{Move}, \code{MoveBurst} and \code{MoveStack} objects and can be obtained with the function \link{unUsedRecords}.}

\section{Slots}{
  \describe{
    \item{trackIdUnUsedRecords}{Object of class \code{"factor"}: vector that indicates, which data, coordinates and timestamps of the unused records belong to each individual in a \code{.unUsedRecordsStack} object}
    \item{timestampsUnUsedRecords}{Object of class \code{"POSIXct"}: timestamps associated to the unused records.}
    \item{sensorUnUsedRecords}{Object of class \code{"factor"}: sensors used to record the unused records}
    \item{dataUnUsedRecords}{Object of class \code{"data.frame"}: data associated to the unused records}
  }
}

\section{Methods}{
  Methods defined with class "MoveStack" in the signature:
    \describe{
       \item{\code{\link{as.data.frame}}}{\code{signature(object = ".unUsedRecords")}: extracts the spatial data frame}
       \item{\code{\link{sensor}}}{\code{signature(object = ".unUsedRecords")}: extracts the sensor(s) used to record the data}
       \item{\code{\link{timestamps}}}{\code{signature(object = ".unUsedRecords")}: extracts returns or sets the timestamps}
       \item{\code{\link{trackId}}}{\code{signature(object = ".unUsedRecordsStack")}:  returning the Id of the individual per data point}
       }
}

\author{Marco Smolla & Anne Scharf}
