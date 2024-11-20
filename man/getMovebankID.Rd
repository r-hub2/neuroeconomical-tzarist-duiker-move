\name{getMovebankID}

\alias{getMovebankID}
\alias{getMovebankID,character,MovebankLogin-method}
\alias{getMovebankID,character,missing-method}

\docType{methods}

\title{Movebank Study ID}

\description{This function returns the numeric Movebank ID of the study which corresponds to the character study name stored on Movebank}

\usage{
  getMovebankID(study,login)
}

\arguments{
  \item{study}{character; full name of the study, as stored on Movebank}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password}
}

\details{
  \code{getMovebankID} belongs to the Movebank browsing functions and returns the Movebank ID of a study as it is stored on \href{https://www.movebank.org}{Movebank}. This number can also be found on the \emph{Study Details} page of the study on Movebank.
}

\seealso{\code{\link{movebankLogin}}}
\note{
The character study name on Movebank can be potentially edited and changed at any time by the \emph{Data Manager(s)}, whereas the \emph{Movebank ID} is uniquely assigned to each study when it is uploaded to Movebank, and cannot be modified afterwards.

See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R.
}

\value{
The function returns one '\code{numeric}' value.}

\author{Marco Smolla & Anne Scharf}

\examples{
\dontrun{

#obtain a login
login<-movebankLogin()
getMovebankID(study="Ocelots on Barro Colorado Island, Panama", login=login)
}
}
