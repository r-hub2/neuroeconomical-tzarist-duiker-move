\name{dBGBvariance-class}
\alias{dBGBvariance-class}
\alias{dBGBvarianceTmp-class}
%\alias{dynBGBvariance}
%\alias{dynBGBvarianceTmp}

\docType{class}

\title{The dynBGBvariance class}

\description{The \code{dynBGBvariance} object stores the orthogonal and parallel variances calculated by the dynamic Bivariate Gaussian Bridge model, and is created within the \link{dynBGBvariance} function from a Move object. 
}

\note{The \code{dynBGBvariance} object contains a \code{.MoveTrackSingle} and a \code{dBGBvarianceTmp} object which can be used to program against.
}

\section{Slots}{
  \describe{
    \item{windowSize}{The window size used for dynBGBvariance calculation}
    \item{margin}{The margin used for dynBGB calculation}
    \item{paraSd}{The standard deviation values of the parallel variance values}
    \item{orthSd}{The standard deviation values of the orthogonal variance values}
    \item{nEstim}{The number of windows each location was included in}
    \item{segInterest}{Logical string, FALSE: segments have been omitted in the calculation since a lower number of estimates for variance are obtained for these segments. TRUE: segments included in the calculation}    
  }
}

\section{Methods}{
  \describe{
    \item{\code{\link{as.data.frame}}}{\code{signature(object = "dBGBvarianceTmp")}: extracts the spatial data frame}
    \item{\code{\link{coordinates}}}{\code{signature(object = "dBGBvarianceTmp")}: extracts the coordinates from the Move object contained in the dBGBvarianceTmp}
    \item{\code{\link{dynBGB}}}{\code{signature(object = "dBGBvarianceTmp")}: calculates the utilization distribution (UD) of the given track using the dynamic Bivariate Gaussian Bridge model}
    \item{\code{\link{getMotionVariance}}}{\code{signature(object = "dBGBvarianceTmp")}: extracts the estimated motion variance}
    \item{\code{\link{lines}}}{\code{signature(object = "dBGBvarianceTmp")}: add lines of the track of the animal to a plot}
    \item{\code{\link{points}}}{\code{signature(object = "dBGBvarianceTmp")}: add points of the track of the animal to a plot}
    \item{\code{\link{plot}}}{\code{signature(object = "dBGBvarianceTmp")}: plots the track of the animal}
    \item{\code{\link{show}}}{\code{signature(object = "dBGBvarianceTmp")}: displays summary the dBGBvarianceTmp object}
    \item{\code{\link{summary}}}{\code{signature(object = "dBGBvarianceTmp")}: summarizes the information of the raster from a dBGBvarianceTmp object}
    \item{\code{\link{subset}}}{\code{signature(object = "dBGBvarianceTmp")}: subsets the dBGBvarianceTmp object}
  }
}


\author{Bart Kranstauber & Anne Scharf}
