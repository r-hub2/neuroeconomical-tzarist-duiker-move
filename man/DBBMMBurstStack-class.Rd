\name{DBBMMBurstStack-class}

\alias{DBBMMBurstStack}
\alias{DBBMMBurstStack-class}

\docType{class}

\title{
  The DBBMMBurstStack class
}

\description{
  The \code{DBBMMBurstStack} object is created within the \link{brownian.bridge.dyn} function from a \code{MoveBurst} or \code{dBMvarianceBurst} object. It contains a \link[=dBMvariance-class]{dBMvarianceBurst} object and a \code{raster} with probabilities.
  }
  
\note{ 
  The \code{DBBMMBurstStack} object contains a \code{\link[=dBMvariance-class]{dBMvarianceBurst}} and a \code{\link[=.UD-class]{.UDStack}} object which can be used to program against.
}

\section{Slots}{
  \describe{
    \item{crs}{part of the \link[raster]{Raster-class}}
    \item{DBMvar}{Object of class "\link[=dBMvariance-class]{dBMvarianceBurst}": includes the window.size, margin, means, in.windows, break.list, and points of interest}
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
    \item{\code{\link{getMotionVariance}}}{\code{signature(object = "DBBMMBurstStack")}: extracts the estimated motion variance}
    \item{\code{\link{plot}}}{\code{signature(object = "DBBMMBurstStack")}: plots the raster from a DBBMMBurstStack object with re-size insensitive proportions}
    \item{\code{\link{show}}}{\code{signature(object = "DBBMMBurstStack")}: displays summary the DBBMMBurstStack object}
    \item{\code{\link{subset}}}{\code{signature(object = "DBBMMBurstStack")}: subsets the DBBMMBurstStack object}
    \item{\code{\link{UDStack}}}{\code{signature(object = "DBBMMBurstStack")}: creates UDStack objects}
  }
}

\author{Anne Scharf}

\keyword{classes}


