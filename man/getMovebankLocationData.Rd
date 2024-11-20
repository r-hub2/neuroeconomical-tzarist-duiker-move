\name{getMovebankLocationData}

\alias{getMovebankLocationData}
\alias{getMovebankLocationData,ANY,ANY,ANY,missing-method}
\alias{getMovebankLocationData,ANY,ANY,missing,missing-method}
\alias{getMovebankLocationData,ANY,missing,missing,missing-method}
\alias{getMovebankLocationData,character,ANY,ANY,MovebankLogin-method}
\alias{getMovebankLocationData,numeric,missing,ANY,MovebankLogin-method}
\alias{getMovebankLocationData,numeric,character,ANY,MovebankLogin-method}
\alias{getMovebankLocationData,numeric,numeric,missing,MovebankLogin-method}
\alias{getMovebankLocationData,numeric,numeric,character,MovebankLogin-method}
\alias{getMovebankLocationData,numeric,numeric,numeric,MovebankLogin-method}

\docType{methods}

\title{Download location data from Movebank as a table}
\description{This function downloads the location data for one or several sensors of a study stored in Movebank.}

\usage{
\S4method{getMovebankLocationData}{numeric,numeric,character,MovebankLogin}(study, sensorID, animalName, login, ...)

\S4method{getMovebankLocationData}{numeric,numeric,numeric,MovebankLogin}(study, sensorID, animalName, login,
                        includeOutliers=FALSE, underscoreToDots=TRUE, ...)
}

\arguments{
  \item{study}{character or numeric. Character: full name of the study, as stored on Movebank. Numeric: \emph{Movebank ID} of the study which can be obtained on the \emph{Study Details} page on Movebank or with \code{\link{getMovebankID}}.}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password}
  \item{sensorID}{character or numeric. Name or ID number of sensor(s) recording location data. A single sensor or a vector of sensors can be specified. If the argument is left empty data of all location sensors will be downloaded. Optional. See 'Details'.}
  \item{animalName}{character. Name of the individuals as stored on Movebank. A single individual or a vector of several individuals from the same study can be specified. If the argument is left empty data of all individuals will be downloaded. Optional.}
  \item{includeOutliers}{logical. If TRUE locations marked as outliers in Movebank will be included. Default is FALSE.}
  \item{underscoreToDots}{logical. Many of the functions in the \emph{Move} package rely on the column names containing dots and not underscores. Default is TRUE. See 'Details'.}
  \item{...}{Additional arguments passed on to the movebank API through \code{\link{getMovebank}} function: 
  \describe{
  \item{\code{timestamp_start, timestamp_end}}{character or POSIXct. Starting and/or ending timestamp to download the data for a specific time period. Timestamps have to be provided in format 'yyyyMMddHHmmssSSS'. If POSIXct then it is converted to character using UTC as a time zone, note that this can change the time. Optional.}
  }
  }
}
\details{
\code{getMovebankLocationData} belongs to the Movebank browsing functions and returns a \code{data.frame} with data from one or multiple location sensors from studies with one animal or multiple animals.\cr
Remember that you need an account on \href{https://www.movebank.org}{Movebank}, see \code{\link{movebankLogin}}.\cr
Note that \link{getMovebankData} has also the option to download location data directly into a \code{move/moveStack} object.

\emph{Attribute names:}\cr
The definitions of the content of the columns of the returned \code{data.frame} is detailed in the \href{https://www.movebank.org/node/2381}{Attribute Dictionary on Movebank}. The attributes \code{deployment_id, individual_id, tag_id, study_id} correspond to the internal ids of Movebank.

\code{sensorID}:\cr
See \link{getMovebankSensors} to obtain all available sensors of the study of interest. The valid names for this argument are those of the the columns "name" or "id" of the table returned by \code{getMovebankSensors(login)}. The valid numeric Ids are also in the column "sensor_type_id" in the table returned for a specific study with \code{getMovebankSensors(study,login)}. This function only accepts location sensors which are marked as "true"" in the "is_location_sensor" column of the table returned by \code{getMovebankSensors(login)}.

\code{underscoreToDots}:\cr
.csv files downloaded from the Movebank webpage contain dots in their column names, and .csv files downloaded via the API (like in the case of this function) contain instead underscores in their column names. Many of the functions in the \emph{Move} package were created based on the webpage csv downloaded data and rely on the column names with dots. If you would like to use function like e.g. \code{getDuplicatedTimestamps} or read in the csv file with \code{move} by stating the path to file, among others, than the column names have to be with dots.

\emph{Downloading a study with many locations:}\cr
If the study to be downloaded has many locations (probably in the order of 10s of millions), the download may take so long that the connection breaks, and the study cannot be downloaded. We recommend to download each individual separately to ensure a successfully download. See more details and examples in the 'browseMovebank' vignette.
}

\value{
'\code{data.frame}'
}

\author{
Anne Scharf
}
\note{
See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R.
}

\seealso{
\link{movebankLogin}, \link{getMovebankData}, \link{getMovebankNonLocationData}
}
\examples{
\dontrun{
## first create the login object 
login <- movebankLogin()

## get GPS data for one individual
str(getMovebankLocationData(study=74496970, sensorID="GPS",
                               animalName="DER AR439", login=login))

## get GPS data for one individual after the "2013-07-12 06:50:07.000"
str(getMovebankLocationData(study=74496970, sensorID="GPS", animalName="DER AR439", 
                               login=login, timestamp_start="20130712065007000"))

# get GPS data for all individuals of the study between 
# the "2013-08-15 15:00:00.000" and "2013-08-15 15:01:00.000"
str(getMovebankLocationData(study=74496970, sensorID=653, 
                               login=login, timestamp_start="20130815150000000", 
                               timestamp_end="20130815150100000"))
}
}

