\name{getDuplicatedTimestamps}

\alias{getDuplicatedTimestamps}
\alias{getDuplicatedTimestamps,character-method}
\alias{getDuplicatedTimestamps,connection-method}
\alias{getDuplicatedTimestamps,data.frame-method}
\alias{getDuplicatedTimestamps,factor-method}

\docType{methods}

\title{Identifies duplicated timestamps
}
\description{
Identifies all pairs of duplicated timestamps within an individual and sensor type from data downloaded from Movebank or own data.
}
\usage{
  \S4method{getDuplicatedTimestamps}{character}(x, ..., onlyVisible = TRUE)
 
  \S4method{getDuplicatedTimestamps}{factor}(x, timestamps, sensorType, visible=NULL, ...)
}

\arguments{
  \item{x}{full path to the csv (or compressed) file location downloaded from a Movebank study, or to the zip file location downloaded from the EnvData tool in Movebank. \cr
  \code{data.frame} read into R from a csv file downloaded from Movebank, or downloaded with \code{getMovebankLocationData}.\cr
  \code{factor} containing the name(s) of the individual(s) if non-Movebank data are provided.}
  \item{timestamps}{vector containing timestamps with POSIXct conversion if non-Movebank data are provided,i.e. \code{as.POSIXct(data$timestamp, format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC")}}
  \item{sensorType}{optional, character or vector of characters containing sensor type(s) if non-Movebank data are provided.}
  \item{onlyVisible}{logical, indicating if the visible column in the movebank data should be considered when the column visible is present, the default is to ignore all non-visible/outlier locations}
  \item{visible}{optional, a logical vector indicating the locations that should be considered.}
  \item{...}{currently not implemented}
}
\details{
If own data (non-Movebank) are used, the vectors specified in "x", "timestamps" and optionally "visible" have to have the same length.
}
\value{
This function returns a list. The name of the list elements contains the individual's name, the timestamp that is duplicated and the sensor type (if provided). Each list element contains a vector with the corresponding row numbers where the duplicated timestamps are located in the table. If no duplicated timestamps are found \code{NULL} is returned.
}

\author{
Anne Scharf
}

\examples{
data(duplicatedDataExample)
getDuplicatedTimestamps(x=as.factor(duplicatedDataExample$individual.id), 
                        timestamps=as.POSIXct(duplicatedDataExample$timestamps, 
					      format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"),
                        sensorType=duplicatedDataExample$sensor.type)

filePath<-system.file("extdata","leroy.csv.gz",package="move")
getDuplicatedTimestamps(filePath)


}

