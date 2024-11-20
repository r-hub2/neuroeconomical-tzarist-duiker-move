\name{Move-class}

\alias{Move-class}
\alias{.MoveTrack-class}
\alias{.MoveGeneral-class}
\alias{.MoveTrackSingle-class}
\alias{.MoveTrack}
\alias{.MoveTrackSingle}

\docType{class}

\title{
The Move class
}

\description{
The \code{Move} object contains at least time and coordinate information of an animal. It can contain further data that are specific to the animal, e.g. the sex or age, which are stored in the idData slot \code{data.frame}. Any data associated to the coordinates are stored in the data slot \code{data.frame}. If the object was created with the Movebank browsing functions it also contains the study name, licence and citation information. \cr
A \code{Move} object can be created with the functions \link{move}, \link{getMovebankData} or \link{getDataRepositoryData}.
}

\note{The \code{Move} object contains a \code{.MoveGeneral}, \code{.MoveTrack}, \code{.MoveTrackSingle} and \code{\link[=.unUsedRecords-class]{.unUsedRecords}} object which can be used to program against.}

\section{Slots}{
  \describe{
    \item{bbox}{belongs to the SpatialPointsDataFrame}
    \item{citation}{Object of class \code{"character"}: how to cite the study, when Movebank data are used}
    \item{coords}{coordinates of the track, belongs to the SpatialPointsDataFrame}
    \item{coords.nrs}{belongs to the SpatialPointsDataFrame}
    \item{data}{Object of class \code{"data.frame"}: additional data associated to the coordinates}
    \item{dataUnUsedRecords}{Object of class \code{"data.frame"}: data associated to the unused records}
    \item{dateCreation}{Object of class \code{"POSIXct"}: timestamp when the Move object was created}
    \item{idData}{Object of class \code{"data.frame"}: additional (one row) data. These data contain information associated to the animal}
    \item{license}{Object of class \code{"character"}: the license under which the data were published, when Movebank data are used}
    \item{proj4string}{Object of class \code{"CRS"}: projection of the coordinates}
    \item{sensor}{Object of class \code{"factor"}: sensors used to record the coordinates}
    \item{sensorUnUsedRecords}{Object of class \code{"factor"}: sensors used to record the unused records}
    \item{study}{Object of class \code{"character"}: name of the study, when Movebank data are used}
    \item{timestamps}{Object of class \code{"POSIXct"}: timestamps associated to the coordinates}
    \item{timestampsUnUsedRecords}{Object of class \code{"POSIXct"}: timestamps associated to the unused records, i.e. lines of the data that were removed because they included NA locations}
   }
}

\section{Methods}{
  \describe{
      \item{\code{\link{angle}}}{\code{signature(object = "Move")}: calculates angles between consecutive locations}
      \item{\code{\link{as.data.frame}}}{\code{signature(object = "Move")}: extracts the spatial data frame}
      \item{\code{\link{brownian.bridge.dyn}}}{\code{signature(object = "Move")}: calculates the utilization distribution (UD) of the given track using the dynamic Brownian Bridge Movement Model}
      \item{\code{\link{brownian.motion.variance.dyn}}}{\code{signature(object = "Move")}: calculates the motion variance of the dynamic Brownian Bridge Movement Model}
      \item{\code{\link{burst}}}{\code{signature(object = "Move")}: bursts a track by a specified variable}
      \item{\code{\link{citations}}}{\code{signature(object = "Move")}: extracts or sets the citation}
      \item{\code{\link{coordinates}}}{\code{signature(object = "Move")}: extracts the coordinates from the track}
      \item{\code{\link{corridor}}}{\code{signature(object = "Move")}: identifies track segments whose attributes suggest corridor use behavior} 
      \item{\code{\link{distance}}}{\code{signature(object = "Move")}: calculates distances between consecutive locations}
      \item{\code{\link{dynBGB}}}{\code{signature(object = "Move")}: calculates the utilization distribution (UD) of the given track using the dynamic Bivariate Gaussian Bridge model}
      \item{\code{\link{dynBGBvariance}}}{\code{signature(object = "Move")}: calculates the orthogonal and parallel motion variance of the dynamic Brownian Bridge Movement Model}
      \item{\code{\link{equalProj}}}{\code{signature(object = "Move")}: checks whether all objects of a list are in the same projection}
      \item{\code{\link{hrBootstrap}}}{\code{signature(object = "Move")}: calculates and plots the area of the Minimum Convex Polygon of a track}
      \item{\code{\link{idData}}}{\code{signature(object = "Move")}: returns or replaces the idData slot}
      \item{\code{\link{interpolateTime}}}{\code{signature(object = "Move")}: interpolates trajectories based on time}
      \item{\code{\link{lines}}}{\code{signature(object = "Move")}: add lines of the track of the animal to a plot}
      \item{\code{\link{move2ade}}}{\code{signature(object = "Move")}: converts to a adehabitat compatible object}
      \item{\code{\link{moveStack}}}{\code{signature(object = "Move")}: stacks a list of Move objects}
      \item{\code{\link{n.locs}}}{\code{signature(object = "Move")}: calculates number of locations}
      \item{\code{\link{plot}}}{\code{signature(object = "Move")}: plots the track of the animal}
      \item{\code{\link{points}}}{\code{signature(object = "Move")}: add points of the track of the animal to a plot}
      \item{\code{\link{seglength}}}{\code{signature(object = "Move")}: calculates the length of each segment of a track}
      \item{\code{\link{sensor}}}{\code{signature(object = "Move")}: extracts the sensor(s) used to record the coordinates}
      \item{\code{\link{show}}}{\code{signature(object = "Move")}: displays summary the Move object}
      \item{\code{\link{speed}}}{\code{signature(object = "Move")}: calculates speed between consecutive locations}
      \item{\code{\link{spTransform}}}{\code{signature(object = "Move")}: transforms coordinates to a different projection method}
      \item{\code{\link{summary}}}{\code{signature(object = "Move")}: summarizes the information of Move object}
       \item{\code{\link{subset}}}{\code{signature(object = "Move")}: subsets the Move object}
      \item{\code{\link{timeLag}}}{\code{signature(object = "Move")}: calculates time lag between consecutive locations}
      \item{\code{\link{timestamps}}}{\code{signature(object = "Move")}: gets the timestamps associated to the coordinates}
      \item{\code{\link{turnAngleGc}}}{\code{signature(object = "Move")}: calculates angles between consecutive locations}
      \item{\code{\link{unUsedRecords}}}{\code{signature(object = "Move")}: returns the unUsedRecords object containing the data of the unused records}
  }
}

\author{Marco Smolla & Anne Scharf}

\keyword{classes}

