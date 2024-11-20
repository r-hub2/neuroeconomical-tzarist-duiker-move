\name{getMovebankNonLocationData}

\alias{getMovebankNonLocationData}
\alias{getMovebankNonLocationData,ANY,ANY,ANY,missing-method}
\alias{getMovebankNonLocationData,ANY,ANY,missing,missing-method}
\alias{getMovebankNonLocationData,ANY,missing,missing,missing-method}
\alias{getMovebankNonLocationData,character,ANY,ANY,MovebankLogin-method}
\alias{getMovebankNonLocationData,numeric,missing,ANY,MovebankLogin-method}
\alias{getMovebankNonLocationData,numeric,character,ANY,MovebankLogin-method}
\alias{getMovebankNonLocationData,numeric,numeric,missing,MovebankLogin-method}
\alias{getMovebankNonLocationData,numeric,numeric,character,MovebankLogin-method}
\alias{getMovebankNonLocationData,numeric,numeric,numeric,MovebankLogin-method}

\docType{methods}

\title{Download non-location data from Movebank}
\description{This function downloads the non location data for one or several sensors of a study stored in Movebank}

\usage{
\S4method{getMovebankNonLocationData}{numeric,numeric,character,MovebankLogin}(study, sensorID, animalName, login, ...)

\S4method{getMovebankNonLocationData}{numeric,numeric,numeric,MovebankLogin}(study, sensorID, animalName, login, ...)
}

\arguments{
  \item{study}{character or numeric. Character: full name of the study, as stored on Movebank. Numeric: \emph{Movebank ID} of the study which can be obtained on the \emph{Study Details} page on Movebank or with \code{\link{getMovebankID}}.}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password}
  \item{sensorID}{character or numeric. Name or ID number of sensor(s) recording non location data. A single sensor or a vector of sensors can be specified. If the argument is left empty data of all non location sensors will be downloaded. Optional. See 'Details'.}
  \item{animalName}{character. Name of the individuals as stored on Movebank. A single individual or a vector of several individuals from the same study can be specified. If the argument is left empty data of all individuals will be downloaded. Optional.}
  \item{...}{Additional arguments passed on to the movebank API through \code{\link{getMovebank}} function: 
  \describe{
  \item{\code{timestamp_start, timestamp_end}}{character or POSIXct. Starting and/or ending timestamp to download the data for a specific time period. Timestamps have to be provided in format 'yyyyMMddHHmmssSSS'. If POSIXct then it is converted to character using UTC as a time zone, note that this can change the time. Optional.}
  }
  }
}
\details{
\code{getMovebankNonLocationData} belongs to the Movebank browsing functions and returns a \code{data.frame} with data from one or multiple non-location sensors from studies with one animal or multiple animals.\cr
Remember that you need an account on \href{https://www.movebank.org}{Movebank}, see \code{\link{movebankLogin}}.\cr
Note that \link{getMovebankData} has also the option to download non location alongside with the location data.

\emph{Attribute names:}\cr
The definitions of the content of the columns of the returned \code{data.frame} is detailed in the \href{https://www.movebank.org/node/2381}{Attribute Dictionary on Movebank}

\code{sensorID}:\cr
See \link{getMovebankSensors} to obtain all available sensors of the study of interest. The valid names for this argument are those of the the columns "name" or "id" of the table returned by \code{getMovebankSensors(login)}. The valid numeric Ids are also in the column "sensor_type_id" in the table returned for a specific study with \code{getMovebankSensors(study,login)}. This function only accepts non-location sensors which are marked as "false"" in the "is_location_sensor" column of the table returned by \code{getMovebankSensors(login)}.
}

\note{
See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R.

\emph{Downloading a study with a lot of data:}\cr
If the study to be downloaded has many locations (probably in the order of 10s of millions), the download may take so long that the connection breaks, and the study cannot be downloaded. We recommend to download each individual separately to ensure a successfully download. See more details and examples in the 'browseMovebank' vignette.
}

\value{
'\code{data.frame}'
}

\author{
Anne Scharf
}

\seealso{
\link{movebankLogin}, \link{getMovebankData}, \link{getMovebankLocationData}
}
\examples{
\dontrun{
## first create the login object 
login <- movebankLogin()

## get acceleration data for one individual
str(getMovebankNonLocationData(study=74496970 , sensorID="Acceleration",
                               animalName="DER AR439", login=login))

## get acceleration data for one individual after the "2013-07-12 06:50:07.000"
str(getMovebankNonLocationData(study=74496970 , sensorID="Acceleration", animalName="DER AR439", 
                               login=login, timestamp_start="20130712065007000"))

# get acceleration data for all individuals of the study between 
# the "2013-08-15 15:00:00.000" and "2013-08-15 15:01:00.000"
str(getMovebankNonLocationData(study=74496970 , sensorID=2365683, 
                               login=login, timestamp_start="20130815150000000", 
                               timestamp_end="20130815150100000"))
}
}

