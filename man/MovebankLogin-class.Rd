\name{MovebankLogin-class}

\alias{MovebankLogin}
\alias{MovebankLogin-class}

\docType{class}

\title{
  The MovebankLogin class
}

\description{
  The MovebankLogin object is needed for every Movebank browsing function. It is created with the function \link{movebankLogin}. Alternatively, one can also chose to enter the username and password every time one uses one of the browsing functions. The object is inherited from an \code{httr} \code{request} object.
}

\section{Methods}{
  \describe{
    \item{\code{\link{getMovebank}}}{\code{signature(object = "MovebankLogin")}: download data from Movebank}
    \item{\code{\link{getMovebankAnimals}}}{\code{signature(object = "MovebankLogin")}: get animals, tags and IDs of a Movebank study}
    \item{\code{\link{getMovebankData}}}{\code{signature(object = "MovebankLogin")}: download data from Movebank as a Move* object}
    \item{\code{\link{getMovebankLocationData}}}{\code{signature(object = "MovebankLogin")}: download location data from Movebank as a table}
    \item{\code{\link{getMovebankNonLocationData}}}{\code{signature(object = "MovebankLogin")}: download non-location data from Movebank as a table}
    \item{\code{\link{getMovebankID}}}{\code{signature(object = "MovebankLogin")}: get study ID from Movebank}
    \item{\code{\link{getMovebankSensors}}}{\code{signature(object = "MovebankLogin")}: get information about Movebank sensors}
    \item{\code{\link{getMovebankSensorsAttributes}}}{\code{signature(object = "MovebankLogin")}: get available sensor attributes of a Movebank Study}
    \item{\code{\link{getMovebankStudies}}}{\code{signature(object = "MovebankLogin")}: get all studies available on Movebank}
    \item{\code{\link{getMovebankStudy}}}{\code{signature(object = "MovebankLogin")}: get information of a Movebank study}
    \item{\code{\link{searchMovebankStudies}}}{\code{signature(object = "MovebankLogin")}: search for a Movebank study by key words}
    \item{\code{\link{getMovebankReferenceTable}}}{\code{signature(object = "MovebankLogin")}: get all reference data of a Movebank study}
    \item{\code{\link{show}}}{\code{signature(object = "MovebankLogin")}: shows user name and password contained in the MovebankLogin object}
}
}

\author{Bart Kranstauber, Marco Smolla & Anne Scharf}

\keyword{classes}
