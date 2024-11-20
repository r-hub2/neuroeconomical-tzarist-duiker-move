\name{.UD-class}

\alias{.UD}
\alias{.UD-class}
\alias{.UDStack-class}
\alias{UDStack-class}
\alias{.UDBurstStack-class}

\docType{class}

\title{
The UD class
}

\description{
	The \code{.UD}, \code{.UDStack} and \code{.UDBurstStack} class represent a \code{raster} of a simple abstraction of the utilization distribution (UD) where all probabilities necessarily sum to one. A \code{.UDStack} object can be obtained with the function \link{UDStack}.}
	
\note{	
A \code{DBBMM} and \code{dynBGB} object contains a \code{.UD}. \cr
A \code{DBBMMStack} contains a \code{.UDStack}.\cr
A \code{DBBMMBurstStack} contains a \code{.UDBurstStack}. \cr
These objects can be used to program against.
}

\section{Slots}{
  \describe{
    \item{crs}{part of the \link[raster]{Raster-class}}
    \item{data}{part of the \link[raster]{Raster-class}}
    \item{extent}{part of the \link[raster]{Raster-class}}
    \item{file}{part of the \link[raster]{Raster-class}}
    \item{history}{part of the \link[raster]{Raster-class}}
    \item{names}{part of the \link[raster]{Raster-class}}
    \item{legend}{part of the \link[raster]{Raster-class}}
    \item{method}{stores the method that was used to calculate the utilization distribution (UD), e.g. dynamic Brownian Bridge}
    \item{ncols}{part of the \link[raster]{Raster-class}}
    \item{nrows}{part of the \link[raster]{Raster-class}}
    \item{rotated}{part of the \link[raster]{Raster-class}}
    \item{rotation}{part of the \link[raster]{Raster-class}}
    \item{title}{part of the \link[raster]{Raster-class}}
    \item{z}{part of the \link[raster]{Raster-class}}
  }
}

\section{Methods}{
  \describe{
      \item{\code{\link{contour}}}{\code{signature(object = ".UD")}: adds a contour line to a plot, also for .UDStack}
      \item{\code{\link{emd}}}{\code{signature(object = ".UD")}: quantifies similarity between utilization distributions, also for .UDStack}
      \item{\code{\link{getVolumeUD}}}{\code{signature(object = ".UD")}: modifies the .UD/.UDStack raster}
      \item{\code{\link{outerProbability}}}{\code{signature(object = ".UD")}: calculates the animal occurrence probabilities at the border of the raster (only for .UD class)}
      \item{\code{\link{plot}}}{\code{signature(object = ".UD")}: plots the raster from a .UD/.UDStack object with re-size insensitive proportions}
      \item{\code{\link{raster2contour}}}{\code{signature(object = ".UD")}: converts a raster to contour lines, also for .UDStack}
      \item{\code{\link{show}}}{\code{signature(object = ".UD")}: displays summary the .UD/.UDStack object}
      \item{\code{\link{summary}}}{\code{signature(object = ".UD")}: summarizes the information of the raster from a .UD/.UDStack object}
      \item{\code{\link{subset}}}{\code{signature(object = ".UD")}: subsets the .UD/.UDStack object}
      \item{\code{\link{split}}}{\code{signature(object = ".UDStack")}: splits a .UDStack into a list of .UD objects}
 }
}

\author{Bart Kranstauber & Anne Scharf}

\keyword{classes}
