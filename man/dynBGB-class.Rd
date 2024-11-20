\name{dynBGB-class}
\alias{dynBGB-class}
\docType{class}
\title{The dynBGB class}
\description{
  The \code{dynBGB} object is created within the \link{dynBGB} function from a \code{Move} object. It contains a \link[=dBGBvariance-class]{dBGBvariance} object and a \code{raster} with probabilities.
  }
  
\note{The \code{dynBGB} object contains a \code{\link[=dBGBvariance-class]{dBGBvariance}} and a \code{\link[=.UD-class]{.UD}} object which can be used to program against.}


\section{Slots}{
  \describe{
     \item{crs}{part of the \link[raster]{Raster-class}}
     \item{data}{part of the \link[raster]{Raster-class}}
     \item{var}{Object of class "\link[=dBGBvariance-class]{dBGBvariance}": includes the windowSize, margin, paraSd,orthSd, nEstim, segInterest}
     \item{extent}{part of the \link[raster]{Raster-class}}
     \item{file}{part of the \link[raster]{Raster-class}}
     \item{history}{part of the \link[raster]{Raster-class}}
     \item{legend}{part of the \link[raster]{Raster-class}}
     \item{method}{stores the method that was used to calculate the utilization distribution (UD), e.g. dynBGB}
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
    \item{\code{\link{contour}}}{\code{signature(object = "dynBGB")}: adds a contour line to a plot}
    \item{\code{\link{equalProj}}}{\code{signature(object = "dynBGB")}: checks whether all objects of a list are in the same projection}
    \item{\code{\link{getMotionVariance}}}{\code{signature(object = "dynBGB")}: extracts the estimated motion variance}
    \item{\code{\link{getVolumeUD}}}{\code{signature(object = "dynBGB")}: modifies the UD raster}
    \item{\code{\link{outerProbability}}}{\code{signature(object = "dynBGB")}: calculates the animal occurrence probabilities at the border of the raster}
    \item{\code{\link{plot}}}{\code{signature(object = "dynBGB")}: plots the raster from a dynBGB object with re-size insensitive proportions}
    \item{\code{\link{raster2contour}}}{\code{signature(object = "dynBGB")}: converts a raster to contour lines}
    \item{\code{\link{show}}}{\code{signature(object = "dynBGB")}: displays summary the dynBGB object}
    \item{\code{\link{summary}}}{\code{signature(object = "dynBGB")}: summarizes the information of the raster from a dynBGB object}
    \item{\code{\link{subset}}}{\code{signature(object = "dynBGB")}: subsets the dynBGB object}
  }
}

\author{Bart Kranstauber & Anne Scharf}
\keyword{classes}
\seealso{.UD}
