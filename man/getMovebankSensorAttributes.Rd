\name{getMovebankSensorsAttributes}

\alias{getMovebankSensorsAttributes}
\alias{getMovebankSensorsAttributes,ANY,missing-method}
\alias{getMovebankSensorsAttributes,numeric,MovebankLogin-method}
\alias{getMovebankSensorsAttributes,character,MovebankLogin-method}

\docType{methods}

\title{Available attributes of Movebank sensors}

\description{This function returns all attributes of the sensors of the requested Movebank study.}

\usage{
  getMovebankSensorsAttributes(study, login, ...)
}

\arguments{
  \item{study}{character or numeric. Character: full name of the study, as stored on Movebank. Numeric: \emph{Movebank ID} of the study which can be obtained on the \emph{Study Details} page on Movebank or with \code{\link{getMovebankID}}.}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password}
  \item{...}{Extra arguments passed on to the getMovebank function}
}

\details{
  \code{getMovebankSensorAttributes} belongs to the Movebank browsing functions and returns the attributes of the sensors of a study, i.e. what is the sensor id and which data types are stored for this sensor (e.g. GPS sensors store longitude and latitude locations, and timestamps and have 653 as their ID on Movebank).
  
  The definition of each of the attributes is detailed in the \href{https://www.movebank.org/node/2381}{Attribute Dictionary on Movebank}
  
  The correspondence between the sensor type and the sensor type id can be found with the function \code{\link{getMovebankSensors}}, leaving the study argument empty.
}


\note{
See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R.
}

\value{'\code{data.frame}'}

\author{Marco Smolla}

\seealso{\code{\link{movebankLogin}}}
\examples{
\dontrun{

# obtain a login
login<-movebankLogin()
getMovebankSensorsAttributes(study=2950149, login=login)
}
}
