\name{DBBMMStack-class}

\alias{DBBMMStack}
\alias{DBBMMStack-class}

\docType{class}

\title{
  The DBBMMStack class
}

\description{
  The \code{DBBMMStack} object is created within the \link{brownian.bridge.dyn} function from a \code{MoveStack} or \code{dBMvarianceStack} object. It contains a \link[=dBMvariance-class]{dBMvarianceStack} object and a \code{raster} with probabilities.
  }
  
\note{ 
  The \code{DBBMMStack} object contains a \code{\link[=dBMvariance-class]{dBMvarianceStack}} and a \code{\link[=.UD-class]{.UDStack}} object which can be used to program against.
}

\section{Slots}{
  \describe{
    \item{crs}{part of the \link[raster]{Raster-class}}
    \item{DBMvar}{Object of class "\link[=dBMvariance-class]{dBMvarianceStack}": includes the window.size, margin, means, in.windows, break.list, and points of interest}
    \item{ext}{the extension factor set by the user}
    \item{extent}{part of the \link[raster]{Raster-class}}
    \item{filename}{part of the \link[raster]{Raster-class}}
    \item{layers}{part of the \link[raster]{Raster-class}}    
    %\item{layernames}{part of the \link{Raster-class}}
    \item{method}{the method that was used to calculate the utilization distribution, e.g. dynamic Brwonian Bridge}
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
    \item{\code{\link{contour}}}{\code{signature(object = "DBBMMStack")}: adds a contour line to a plot}
    \item{\code{\link{emd}}}{\code{signature(object = "DBBMMStack")}: quantifies similarity between utilization distributions}
    \item{\code{\link{equalProj}}}{\code{signature(object = "DBBMMStack")}: checks whether all objects of a list are in the same projection}
    \item{\code{\link{getMotionVariance}}}{\code{signature(object = "DBBMMStack")}: extracts the estimated motion variance}
    \item{\code{\link{getVolumeUD}}}{\code{signature(object = "DBBMMStack")}: modifies the UD raster}
    \item{\code{\link{outerProbability}}}{\code{signature(object = "DBBMMStack")}: calculates the animal occurrence probabilities at the border of the raster}
    \item{\code{\link{plot}}}{\code{signature(object = "DBBMMStack")}: plots the raster from a DBBMMStack object with re-size insensitive proportions}
    \item{\code{\link{raster2contour}}}{\code{signature(object = "DBBMMStack")}: converts a raster to contour lines}
    \item{\code{\link{show}}}{\code{signature(object = "DBBMMStack")}: displays summary the DBBMMStack object}
    \item{\code{\link{split}}}{\code{signature(object = "DBBMMStack")}: splits a DBBMMStack into a list of DBBMM objects}
    \item{\code{\link{summary}}}{\code{signature(object = "DBBMMStack")}: summarizes the information of the raster from a DBBMMStack object}
    \item{\code{\link{subset}}}{\code{signature(object = "DBBMMStack")}: subsets the DBBMMStack object}
  }
}

\author{Marco Smolla & Anne Scharf}

\keyword{classes}


