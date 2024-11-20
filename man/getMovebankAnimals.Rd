\name{getMovebankAnimals}

\alias{getMovebankAnimals}
\alias{getMovebankAnimals,ANY,MovebankLogin-method}
\alias{getMovebankAnimals,numeric,MovebankLogin-method}
\alias{getMovebankAnimals,character,MovebankLogin-method}
\alias{getMovebankAnimals,ANY,missing-method}

\docType{methods}

\title{Animals, tags and IDs in a Movebank study}

\description{This function returns information of the animals, their tags and IDs from a Movebank study.}

\usage{
  getMovebankAnimals(study, login)
}

\arguments{
  \item{study}{character or numeric. Character: full name of the study, as stored on Movebank. Numeric: \emph{Movebank ID} of the study which can be obtained on the \emph{Study Details} page on Movebank or with \code{\link{getMovebankID}}.}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password}
}

\details{
\code{getMovebankAnimals} belongs to the Movebank browsing functions and returns a \code{data.frame} from the requested study that includes among others the \code{individual_id, tag_id, deployment_id, sensor_type_id} which are the internal ids of Movebank, the \code{tag_local_identifier, local_identifier} which are the ids uploaded to Movebank by the user and other information if available as e.g. \code{death_comments, sex, individual_taxon_canonical_name}, etc.
}

\note{
See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R.
}

\value{
'\code{data.frame}'
     
}

\author{Marco Smolla & Anne Scharf}

\seealso{
\link{movebankLogin}, \link{getMovebankReferenceTable}
}
\examples{
\dontrun{

# obtain a login
login<-movebankLogin()
getMovebankAnimals(study=2950149, login=login)
}
}
