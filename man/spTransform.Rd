\name{spTransform}

\alias{spTransform}
\alias{spTransform,Move,character-method}
\alias{spTransform,Move,missing-method}

\docType{methods}

\title{Transform projection of movement track}

\description{The spTransform function transforms the coordinates stored in the Move object from the default long/lat coordinates to the default aeqd (Azimuthal Equi-distance) projection or a user defined projection.}

\usage{
  \S4method{spTransform}{Move,character}(x,CRSobj,center=FALSE)
  \S4method{spTransform}{Move,missing}(x,center=FALSE,...)
}

\arguments{
	\item{x}{a \code{move}, \code{moveStack} or \code{moveBurst} object to be transformed}
	\item{CRSobj}{object of class CRS, or of class character in which case it is converted to CRS. Can be left empty if \code{center=TRUE}}
	\item{center}{logical, if TRUE the center of the coordinate system is the center of the track; FALSE is default}
	\item{...}{for additional arguments}
}

\details{
	The spTransform function transforms the coordinates of a Move object by default from \code{"+proj=longlat"} to \code{"+proj=aeqd"}. In this format the coordinates can be used by the \code{\link{brownian.bridge.dyn}} function. \cr
	If \code{center} is TRUE the center of the coordinate system is set to the center of the track.
}

\value{
same as input object with coordinates transformed to the new coordinate reference system.
}

\examples{
## create a Move object
data(leroy)
## transform the Move object by default into "+aeqd" projection method 
## and center the coordinate system
spTransform(leroy, center=TRUE)

## transform the Move object into another projection method, like mollweide
spTransform(leroy, CRSobj="+proj=moll +ellps=WGS84")

##check projection method
proj4string(leroy)
\dontshow{
spTransform(leroy, CRSobj=CRS("+proj=moll +ellps=WGS84"))
data(fishers)
spTransform(fishers, center=TRUE)
}
}

\author{Marco Smolla & Anne Scharf}
