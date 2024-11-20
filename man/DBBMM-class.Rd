\name{DBBMM-class}

\alias{DBBMM}
\alias{DBBMM-class}

\docType{class}

\title{
The DBBMM class
}

\description{
 The \code{DBBMM} object is created within the \link{brownian.bridge.dyn} function from a \code{Move} or \code{dBMvariance} object. It contains a \link[=dBMvariance-class]{dBMvariance} object and a \code{raster} with probabilities.
  }
  
\note{ 
  The \code{DBBMM} object contains a \code{\link[=dBMvariance-class]{dBMvariance}} and a \code{\link[=.UD-class]{.UD}} object which can be used to program against.
}


\section{Slots}{
  \describe{
    \item{crs}{part of the \link[raster]{Raster-class}}
    \item{data}{part of the \link[raster]{Raster-class}}
    \item{DBMvar}{Object of class "\link[=dBMvariance-class]{dBMvariance}": includes the window.size, margin, means, in.windows, break.list, and points of interest}
    \item{ext}{the extension factor set by the user}
    \item{extent}{part of the \link[raster]{Raster-class}}
    \item{file}{part of the \link[raster]{Raster-class}}
    \item{history}{part of the \link[raster]{Raster-class}}
    %\item{layernames}{part of the \link[raster]{Raster-class}}
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
    \item{\code{\link{contour}}}{\code{signature(object = "DBBMM")}: adds a contour line to a plot}
    \item{\code{\link{emd}}}{\code{signature(object = "DBBMM")}: quantifies similarity between utilization distributions}
    \item{\code{\link{equalProj}}}{\code{signature(object = "DBBMM")}: checks whether all objects of a list are in the same projection}
    \item{\code{\link{getMotionVariance}}}{\code{signature(object = "DBBMM")}: extracts the estimated motion variance}
    \item{\code{\link{getVolumeUD}}}{\code{signature(object = "DBBMM")}: modifies the UD raster}
    \item{\code{\link{outerProbability}}}{\code{signature(object = "DBBMM")}: calculates the animal occurrence probabilities at the border of the raster}
    \item{\code{\link{plot}}}{\code{signature(object = "DBBMM")}: plots the raster from a DBBMM object with re-size insensitive proportions}
    \item{\code{\link{raster2contour}}}{\code{signature(object = "DBBMM")}: converts a raster to contour lines}
    \item{\code{\link{show}}}{\code{signature(object = "DBBMM")}: displays summary the DBBMM object}
    \item{\code{\link{summary}}}{\code{signature(object = "DBBMM")}: summarizes the information of the raster from a DBBMM object}
    \item{\code{\link{subset}}}{\code{signature(object = "DBBMM")}: subsets the DBBMM object}
  }
}

\author{Marco Smolla & Anne Scharf}

\keyword{classes}
