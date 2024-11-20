\name{searchMovebankStudies}

\alias{searchMovebankStudies}
\alias{searchMovebankStudies,character,MovebankLogin-method}
\alias{searchMovebankStudies,character,missing-method}

\docType{methods}

\title{Search for a study on Movebank}

\description{This function searches for studies within Movebank by a specified keyword or phrase. }

\usage{
  searchMovebankStudies(x, login)
}

\arguments{
  \item{x}{a character string to search within the Movebank study names}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password}
}

\details{
The search function searches explicitly for the entered phrase. If you for example type 'Goose' it will not show you studies including 'goose'. So rather search for 'oose' to find both. 
}

\value{The function returns a character vector of study names.
}

\note{
See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}

\examples{
\dontrun{

# obtain a login
login <- movebankLogin()
# returns all studies that include this exact term: "MPIO"
searchMovebankStudies(x="MPIO", login=login) 
}
}
