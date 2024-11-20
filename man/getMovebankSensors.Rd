\name{getMovebankSensors}

\alias{getMovebankSensors}
\alias{getMovebankSensors,ANY,missing-method}
\alias{getMovebankSensors,ANY,MovebankLogin-method}
\alias{getMovebankSensors,missing,MovebankLogin-method}
\alias{getMovebankSensors,numeric,MovebankLogin-method}
\alias{getMovebankSensors,character,MovebankLogin-method}
\alias{getMovebankSensors,missing,missing-method}

\docType{methods}

\title{Information about Movebank sensors}

\description{This function returns the sensor types used in a Movebank study.}
  
\usage{
  getMovebankSensors(study, login)
}

\arguments{
  \item{study}{character or numeric. Character: full name of the study, as stored on Movebank. Numeric: \emph{Movebank ID} of the study which can be obtained on the \emph{Study Details} page on Movebank or with \code{\link{getMovebankID}}.}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password}
}

\details{
  \code{getMovebankSensors} belongs to the Movebank browsing functions and returns the sensor type(s) used for each tag_id within the specified study.
 
  If the study argument is missing, information about all sensor types available on Movebank and the correspondence between sensor_type_id and the sensor name is obtained.
}

\value{'\code{data.frame}'}

\note{
See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R.
}


\author{Marco Smolla & Anne Scharf}

\seealso{\code{\link{movebankLogin}}}
\examples{
\dontrun{

## obtain a login
login<-movebankLogin()

## obtain sensors types of each tag in the specified study
getMovebankSensors(study=2950149, login=login)

## obtain all sensors available on Movebank
getMovebankSensors(login=login)
}
}
