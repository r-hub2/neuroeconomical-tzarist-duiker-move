\name{burstId}

\alias{burstId}
\alias{burstId<-}
\alias{burstId,MoveBurst-method}
\alias{burstId,.MoveTrackSingleBurst-method}
\alias{burstId<-,.MoveTrackSingleBurst,factor-method}
\alias{burstId<-,.MoveTrackSingleBurst,character-method}

\docType{methods}

\title{Returns or sets the burstId}

\description{Obtain or set the ids of the behavioral categorization per segment of a MoveBurst object.}

\usage{
  \S4method{burstId}{MoveBurst}(x)
  \S4method{burstId}{.MoveTrackSingleBurst,factor}(x) <- value

}

\arguments{
  \item{x}{a \code{moveBurst} object}
  \item{value}{Replacement values for the burst ids, either a factor or a character vector}
}

\value{
	Returns a \code{factor} indicating the category of each segment.
}
\seealso{ 
\link{burst}
}

\author{Bart Kranstauber & Anne Scharf}

\examples{
data(leroy)
burstTrack <- burst(x=leroy,f=months(timestamps(leroy))[-1])
burstId(burstTrack)
}
