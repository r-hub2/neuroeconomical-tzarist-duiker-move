\name{getMovebankStudy}

\alias{getMovebankStudy}
\alias{getMovebankStudy,ANY,MovebankLogin-method}
\alias{getMovebankStudy,character,MovebankLogin-method}
\alias{getMovebankStudy,numeric,MovebankLogin-method}
\alias{getMovebankStudy,ANY,missing-method}

\docType{methods}

\title{Returns information of a Movebank study}

\description{
This function returns information about the requested study as e.g. the authors of the study, licence type, citation and more.}

\usage{
  getMovebankStudy(study, login)
}

\arguments{
  \item{study}{character or numeric. Character: full name of the study, as stored on Movebank. Numeric: \emph{Movebank ID} of the study which can be obtained on the \emph{Study Details} page on Movebank or with \code{\link{getMovebankID}}.}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password}
}

\details{
\code{getMovebankStudy} belongs to the Movebank browsing functions and returns a \code{data.frame} with information about the requested study (e.g.: authors of the study, licence type, citation, etc).}


\note{
See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R.
}

\value{'\code{data.frame}'}


\author{Marco Smolla & Anne Scharf}

\examples{
\dontrun{
# obtain a login
login<-movebankLogin()
getMovebankStudy(study="Coatis on BCI Panama (data from Powell et al. 2017)", login=login)
}
}
