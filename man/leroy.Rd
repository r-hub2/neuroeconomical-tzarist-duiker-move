\name{leroy}

\alias{leroy}

\docType{data}

\title{GPS track data from a fisher}

\description{This file includes spatial data from a fisher (\emph{Martes pennanti}). It can be used to test the different functions from the move package. \cr 
These location data were collected via a 105g GPS tracking collar (manufactured by E-obs GmbH) and programmed to record the animal's location every 15 minutes, continuously. The collar was deployed from 10 February 2009 through 04 March 2009 on an adult, resident, male fisher, in New York, USA (see References). The data usage is permitted for exploratory purposes. For other purposes please get in contact.}
\usage{data("leroy")}

\format{An object of the class move
}
\source{
https://www.datarepository.movebank.org/handle/10255/move.330
}
\references{
For more information, contact Scott LaPoint \email{sdlapoint@gmail.com}
}

\examples{
## create a Move object from the data set
data <- move(system.file("extdata","leroy.csv.gz",package="move"))
plot(data)
data(leroy)

}

\author{Scott LaPoint}
