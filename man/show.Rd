\name{show}

\alias{show}
\alias{show,Move-method}
\alias{show,MoveStack-method}
\alias{show,MoveBurst-method}
\alias{show,.unUsedRecords-method}
\alias{show,dBMvariance-method}
\alias{show,dBMvarianceTmp-method}
\alias{show,.MoveTrack-method}
\alias{show,.MoveGeneral-method}
\alias{show,.MoveTrackStack-method}
\alias{show,.MoveTrackSingle-method}
\alias{show,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Show a Move, DBBMM, dynBGB object}

\description{Displays a summary of the input object.}

\usage{
  \S4method{show}{Move}(object)
}
\arguments{
  \item{object}{a \code{move}, \code{moveStack}, \code{moveBurst}, \code{DBBMM}, \code{DBBMMStack}, \code{DBBMMBurstStack},  \code{dBMvariance}, \code{dBMvarianceBurst}, \code{dBMvarianceStack}, \code{dynBGB}, \code{dBGBvariance}, \code{.UD}, \code{.UDStack}, \code{.UDBurstStack} or \code{movebankLogin} object}
 }

\details{
  For Move, dBMvariance and dBGBvariance objects the function displays a summary including: animal ID, species name, study name, number of track points, receiver type, projection method, date of file creation, the first three lines of the spatial data frame, study citation, data license, number of omitted locations due to NAs in the dataset, etc. If the imported data are not from the Movebank database Animal, Species, nPoints, Receiver, and Study are not shown.\cr\cr
  For DBBMM, dynBGB or .UD objects a summary of the raster properties is shown.\cr\cr
  For the movebankLogin object the username and password is shown.
}

\author{Marco Smolla & Anne Scharf}

\examples{
data(leroy)
show(leroy) # show a move object
data(leroydbbmm)
show(leroydbbmm) # show DBBMM object

}
