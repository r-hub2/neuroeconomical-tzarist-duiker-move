\name{dBMvariance}

\alias{dBMvariance-class}
\alias{dBMvarianceStack-class}
\alias{dBMvarianceBurst-class}
\alias{dBMvarianceTmp-class}


\docType{class}

\title{The dBMvariance class}

\description{The \code{dBMvariance} object is created within the \link{brownian.motion.variance.dyn} function from a \code{Move} object.\cr
The \code{dBMvarianceStack} object is created when a \code{MoveStack} is the input object. \cr
The \code{dBMvarianceBurst} object when the input is a \code{MoveBurst} object. \cr
These objects contain the motion variance calculated by the dynamic Brownian Bridge Movement Model.
}

\note{The \code{dBMvariance} object contains a \code{.MoveTrackSingle} and a \code{dBMvarianceTmp} object.\cr
The \code{dBMvarianceStack} object contains a \code{.MoveTrackStack} and a \code{dBMvarianceTmp} object.\cr
The \code{dBMvarianceBurst} object contains a \code{.MoveTrackSingleBurst} and a \code{dBMvarianceTmp} object. \cr
The class \code{dBMvarianceTmp} is mostly an internal class that is made public to make inheritance easier. These objects can be used to program against.}

\section{Slots}{
  \describe{
    \item{window.size}{The window size used for dbbmm calculation}
    \item{margin}{The margin used for dbbmm calculation}
    \item{means}{The variance values}
    \item{in.windows}{The number of windows each location was included in}
    \item{interest}{Logical string, FALSE: segments have been omitted in the calculation since a lower number of estimates for variance are obtained for these segments. TRUE: segments included in the calculation}
    \item{break.list}{list of the locations of breaks found}    
  }
}

\section{Methods}{
  \describe{
    \item{\code{\link{as.data.frame}}}{\code{signature(object = "dBMvarianceTmp")}: extracts the spatial data frame}
    \item{\code{\link{brownian.bridge.dyn}}}{\code{signature(object = "dBMvarianceTmp")}: calculates the utilization distribution (UD) of the given track using the dynamic Brownian Bridge Movement Model}
    \item{\code{\link{coordinates}}}{\code{signature(object = "dBMvarianceTmp")}: extracts the coordinates from the Move object contained in the dBMvarianceTmp}
    \item{\code{\link{getMotionVariance}}}{\code{signature(object = "dBMvarianceTmp")}: extracts the estimated motion variance}
    \item{\code{\link{lines}}}{\code{signature(object = "dBMvarianceTmp")}: add lines of the track of the animal to a plot}
    \item{\code{\link{points}}}{\code{signature(object = "dBMvarianceTmp")}: add points of the track of the animal to a plot}
    \item{\code{\link{plot}}}{\code{signature(object = "dBMvarianceTmp")}: plots the track of the animal}
    \item{\code{\link{show}}}{\code{signature(object = "dBMvarianceTmp")}: displays summary the dBMvarianceTmp object}
    \item{\code{\link{summary}}}{\code{signature(object = "dBMvarianceTmp")}: summarizes the information of the raster from a dBMvarianceTmp object}
    \item{\code{\link{subset}}}{\code{signature(object = "dBMvarianceTmp")}: subsets the dBMvarianceTmp object}
  }
}


\author{Marco Smolla & Anne Scharf}
