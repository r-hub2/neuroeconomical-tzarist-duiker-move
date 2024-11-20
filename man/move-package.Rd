\name{move-package}

\alias{move-package}

\docType{package}

\title{
An overview of the functions in this package
}

\description{
move is a package that contains functions to access movement data stored on \href{https://www.movebank.org}{Movebank} as well as tools to visualize and statistically analyse animal movement data. Move addresses ecological questions regarding movement.
}

\details{
 The package implements classes for movement data and supports
  \itemize{
    \item Creation of Move objects (see Move-class) representing animals and their track
    \item Calculation of utilization distributions using the dynamic Brownian bridge Movement Model
    \item Plotting tracks, utilization distributions and contours
    \item Access to raster, n.col, projection and coordinates
    \item Different CRS projection methods such as longlat or aeqd
  }
}

\section{I. Creating Move objects}{

Move objects can be created from files with the function:
  \itemize{
    \code{move} To create an object containing one animal track \cr
    \code{moveStack} To create an object containing multiple move objects\cr
    \code{getMovebankData} To create a Move or a MoveStack object with data from Movebank
  }
}

\section{II. Calculation of the utilization distribution}{

The dynamic Brownian Bridge Movement Model calculates the occurrences distribution of a given track
  \tabular{ll}{
    \code{brownian.bridge.dyn}\tab To calculate the occurrences distribution\cr
    \code{getVolumeUD}\tab To calculate the Utilization distribution (UD)\cr
  }
}

\section{III. Accessing values}{

  \tabular{ll}{
    \code{bbox}\tab Bounding box of a Move* object\cr
    \code{coordinates}\tab Track-coordinates of a Move* object\cr
    \code{extent}\tab Extent of a Move* object\cr
    \code{namesIndiv}\tab Names of a Move* object\cr
    \code{n.locs}\tab The number of locations a Move* object\cr
    \code{projection}\tab The projection method of a Move* object or raster\cr
    \code{timeLag}\tab The time lags between the locations of a Move* object\cr
    \code{timestamps}\tab Track-timestamps of a Move* object\cr
    }
}

\section{IV. Plotting data}{

The track or the utilization distribution can be plotted with the following functions:
  \tabular{ll}{
    \code{plot}\tab plots the utilization distribution with fixed width and height ratio (see DBBMM-class), or the track (see Move-class) \cr
    \code{image}\tab plots the utilization distribution fitted to the window \cr
    \code{contour}\tab adds the contours of utilization distribution to a plot\cr 
  }
}

\author{
Bart Kranstauber, Marco Smolla, Anne Scharf \cr \cr
Maintainer: Bart Kranstauber, Marco Smolla, Anne Scharf
}

\references{
\href{https://CRAN.R-project.org/package=move}{move on CRAN}
}

\keyword{package}
