\name{moveStack}

\alias{moveStack}
\alias{moveStack,list-method}
\alias{moveStack,Move-method}
\alias{moveStack,MoveStack-method}
%\alias{moveStack,character-method}

\docType{methods}

\title{Creating a MoveStack}

\description{Stacks a list of Move objects}

\usage{
  \S4method{moveStack}{list}(x, forceTz=NULL, ...)
  
  \S4method{moveStack}{Move}(x, ..., forceTz=NULL)
  
  \S4method{moveStack}{MoveStack}(x, ..., forceTz=NULL)
}

\arguments{
  \item{x}{a list of \code{move} or \code{moveStack} objects (or a combination of both). Timestamps of all objects have to be in the same time zone.}
  \item{forceTz}{The time zone, as a character, that the resulting moveStack object should have (see \code{OlsonNames()} for available time zones). If \code{NULL} the timestamps of the resulting moveStack will be in the time zone of the computer (see \code{Sys.timezone()}) }
  \item{...}{Additional \code{move} or \code{moveStack} objects.}
}

\details{
  This function stacks single Move or Movestacks objects to a \link[=MoveStack-class]{MoveStack} object. 
}

\note{
All animal names are converted into 'good names' which means, that spaces are replaced with points and duplicated names get an individual number added. For example:\cr 
'Leroy, Leroy' -> adding number to duplicated names ->'Leroy, Leroy.1' \cr
'Ricky T' -> replacing spaces -> 'Ricky.T'
}

\value{
a '\link[=MoveStack-class]{MoveStack}' object
}

\seealso{
\link{split}
}

\author{Marco Smolla, Bart Kranstauber & Anne Scharf}

\examples{
data(leroy)
ricky<-move(system.file("extdata","ricky.csv.gz", package="move"))
leroy<-spTransform(leroy, crs(ricky))

## creating a moveStack from a list of move objects
l <- list(ricky[200:270,], leroy[200:270,])
moveStack(l)

## creating a moveStack from several move objects
moveStack(ricky[200:270,], leroy[200:270,], forceTz="UTC")

## creating a moveStack with the same time zone as input move objects
moveStack(ricky[200:270,], leroy[200:270,], forceTz=attr(timestamps(ricky),"tzone"))

\dontshow{
data <- read.csv(system.file("extdata","leroy.csv.gz",package="move"))[100:199,]
ricky2 <- move(x=data$location.long, y=data$location.lat, time=as.POSIXct(data$timestamp,format="\%Y-\%m-\%d \%H:\%M:\%OS", tz="UTC"), proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), data=data, animal=paste0(data$individual.local.identifier,"a"), sensor=data$sensor)
leroy<-spTransform(leroy, crs(ricky2))
list <- list(leroy, ricky2)
stack <- moveStack(list)
}
}
