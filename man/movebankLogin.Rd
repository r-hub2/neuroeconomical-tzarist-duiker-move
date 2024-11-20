\name{movebankLogin}

\alias{movebankLogin}
\alias{movebankLogin,character,character-method}
\alias{movebankLogin,character,missing-method}
\alias{movebankLogin,missing,character-method}
\alias{movebankLogin,missing,missing-method}
 
\docType{methods}

\title{
Login into Movebank
}

\description{
Creates an object that can be used with all Movebank browsing functions. 
}

\usage{
\S4method{movebankLogin}{character,character}(username,password)
}

\arguments{
\item{username}{Your Movebank username}
\item{password}{Your Movebank password}
}

\details{
Use this function to login to \href{https://www.movebank.org}{Movebank}. After you logged in, you can use the Movebank browsing functions from the move package.

If the function is left empty, you'll be requested to enter your username and password. This option is useful to keep Movebank login data confidential when R-code is shared.
}

\value{'\code{\link[=MovebankLogin-class]{MovebankLogin}}'}

\note{See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R.}

\author{Marco Smolla & Anne Scharf}

\examples{
\dontrun{
## First create the login object 
login <- movebankLogin(username="xxx", password="zzz")
## or
login <- movebankLogin()

## and than use it with Movebank browsing functions, e.g.
getMovebankStudies(login)
}
}
