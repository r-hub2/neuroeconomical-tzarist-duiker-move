\name{getMovebankStudies}

\alias{getMovebankStudies}
\alias{getMovebankStudies,MovebankLogin-method}
\alias{getMovebankStudies,missing-method}

\docType{methods}

\title{All studies on Movebank}

\description{This function returns all studies available on Movebank.}

\usage{
  getMovebankStudies(login)
}

\arguments{
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password}
}

\details{
   \code{getMovebankStudies} belongs to the Movebank browsing functions and returns all studies available on Movebank.
}

\note{
See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R.
}

\value{returns an object of class '\code{factor}' with the names of all studies available on Movebank.}

\author{Marco Smolla & Anne Scharf}

\examples{
\dontrun{

# obtain a login
login <- movebankLogin()
getMovebankStudies(login=login)
}
}
