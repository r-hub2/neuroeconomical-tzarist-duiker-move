\name{contour}

\alias{contour}
\alias{contour,.UD-method}
\alias{contour,.UDStack-method}

\docType{methods}

\title{Contour plot}

\description{Contour plot of a RasterLayer from a DBBMM or dynBGB object.}

\usage{
  \S4method{contour}{.UD}(x, ...)
  \S4method{contour}{.UDStack}(x, ...)
}

\arguments{
  \item{x}{a \code{\link[=DBBMM-class]{DBBMM}}, \code{\link[=DBBMMStack-class]{DBBMMStack}}, \code{\link[=dynBGB-class]{dynBGB}}, \code{\link[=.UD-class]{.UD}} or \code{\link[=.UDStack-class]{.UDStack}} object}
  \item{...}{additional arguments, like levels and nlevels, that can be passed to \code{\link[graphics]{contour}} (graphics package). See 'Details'.}
}

\details{
The contour function creates a shape of the area in which the animal can be found by a certain probability (i.e. the 90\% contour describes the area in which the animal can be found with the 90\% probability).\cr 
One or several probabilities can be set with \code{levels} (numeric or vector of values between 0 and 1). If no value is set all contour lines are returned.\cr 
You can also use \code{nlevel} to set a number of fixed distance levels. \cr
To change parameters of the contour or line plotting use the usual parameters of the \code{plot} function (like lwd, lty, and so on).\cr
You can also add the contour lines to a plot of a \code{DBBMM}, \code{dynBGB} or \code{.UD} object by adding \code{add = TRUE}.
}

\author{Marco Smolla & Anne Scharf}

\examples{
data(leroydbbmm)
## to add a 50\% and 95\% contour to a plot from DBBMM object dbbmm
plot(leroydbbmm)
contour(leroydbbmm, levels=c(.5,.95), add=TRUE)
contour(leroydbbmm, levels=c(.5,.95))
}
