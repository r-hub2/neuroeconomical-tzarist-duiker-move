\name{MoveStack-class}

\alias{MoveStack}
\alias{MoveStack-class}
\alias{[,MoveStack,ANY,ANY-class}
\alias{.MoveTrackStack-class}

\docType{class}

\title{
  The MoveStack class
}

\description{
  The \code{MoveStack} object is a stack of \code{move} objects. A \code{MoveStack} object can be created with the functions \link{move}, \link{moveStack}, \link{getMovebankData} or \link{getDataRepositoryData}
}

\note{The \code{MoveStack} object contains a \code{.MoveGeneral}, \code{.MoveTrackStack} and \code{\link[=.unUsedRecords-class]{.unUsedRecordsStack}} object which can be used to program against.}

\section{Slots}{
  \describe{
    \item{bbox}{belongs to the SpatialPointsDataFrame}
    \item{citation}{Object of class \code{"character"}: how to cite the study, when Movebank data are used}
    \item{coords}{coordinates of the track, belongs to the SpatialPointsDataFrame}
    \item{coords.nrs}{belongs to the SpatialPointsDataFrame}
    \item{data}{Object of class \code{"data.frame"}: additional data associated to the coordinates}
    \item{dataUnUsedRecords}{Object of class \code{"data.frame"}: data associated to the unused records}
    \item{dateCreation}{Object of class \code{"POSIXct"}: timestamp when the MoveStack object was created}
    \item{idData}{Object of class \code{"data.frame"}: additional (one row) data. These data contain information associated to the animal}
    \item{license}{Object of class \code{"character"}: the license under which the data were published, when Movebank data are used}
    \item{proj4string}{Object of class \code{"CRS"}: projection of the coordinates}
    \item{sensor}{Object of class \code{"factor"}: sensors used to record the coordinates}
    \item{sensorUnUsedRecords}{Object of class \code{"factor"}: sensors used to record the unused records}
    \item{study}{Object of class \code{"character"}: name of the study, when Movebank data are used}
    \item{timestamps}{Object of class \code{"POSIXct"}: timestamps associated to the coordinates}
    \item{timestampsUnUsedRecords}{Object of class \code{"POSIXct"}: timestamps associated to the unused records, i.e. lines of the data that were removed because they included NA locations}
    \item{trackId}{Object of class \code{"factor"}: vector that indicates, which data, coordinates and timestamps belong to each individual}
    \item{trackIdUnUsedRecords}{Object of class \code{"factor"}: vector that indicates, which data, coordinates and timestamps of the unused records belong to each individual}


  }
}

\section{Methods}{
  \describe{
      \item{\code{\link{angle}}}{\code{signature(object = "MoveStack")}: calculates angles between consecutive locations}
      \item{\code{\link{as.data.frame}}}{\code{signature(object = "MoveStack")}: extracts the spatial data frame}
      \item{\code{\link{brownian.bridge.dyn}}}{\code{signature(object = "MoveStack")}: calculates the utilization distribution (UD) of the given track using the dynamic Brownian Bridge Movement Model}
      \item{\code{\link{brownian.motion.variance.dyn}}}{\code{signature(object = "MoveStack")}: calculates the motion variance of the dynamic Brownian Bridge Movement Model}
      \item{\code{\link{citations}}}{\code{signature(object = "MoveStack")}: extracts or sets the citation}
      \item{\code{\link{coordinates}}}{\code{signature(object = "MoveStack")}: extracts the coordinates from the track}
      \item{\code{\link{corridor}}}{\code{signature(object = "MoveStack")}: identifies track segments whose attributes suggest corridor use behavior} 
      \item{\code{\link{distance}}}{\code{signature(object = "MoveStack")}: calculates distances between consecutive locations}
      \item{\code{\link{equalProj}}}{\code{signature(object = "MoveStack")}: checks whether all objects of a list are in the same projection}
      \item{\code{\link{hrBootstrap}}}{\code{signature(object = "MoveStack")}: calculates and plots the area of the Minimum Convex Polygon of a track}
      \item{\code{\link{idData}}}{\code{signature(object = "MoveStack")}: returns or replaces the idData slot}
      \item{\code{\link{lines}}}{\code{signature(object = "MoveStack")}: add lines of the track of the animals to a plot}
      \item{\code{\link{move2ade}}}{\code{signature(object = "MoveStack")}: converts to a adehabitat compatible object}
      \item{\code{\link{moveStack}}}{\code{signature(object = "MoveStack")}: stacks a list of MoveStack objects}
      \item{\code{\link{n.indiv}}}{\code{signature(object = "MoveStack")}: returns the number of individuals}
      \item{\code{\link{n.locs}}}{\code{signature(object = "MoveStack")}: calculates number of locations}
      \item{\code{\link{plot}}}{\code{signature(object = "MoveStack")}: plots the track of the animals}
      \item{\code{\link{points}}}{\code{signature(object = "MoveStack")}: add points of the track of the animals to a plot}
      \item{\code{\link{seglength}}}{\code{signature(object = "MoveStack")}: calculates the length of each segment of a track}
      \item{\code{\link{sensor}}}{\code{signature(object = "MoveStack")}: extracts the sensor(s) used to record the coordinates}
      \item{\code{\link{show}}}{\code{signature(object = "MoveStack")}: displays summary the MoveStack object}
      \item{\code{\link{speed}}}{\code{signature(object = "MoveStack")}: calculates speed between consecutive locations}
      \item{\code{\link{split}}}{\code{signature(object = "MoveStack")}: splits a MoveStack into a list of Move objects}
      \item{\code{\link{spTransform}}}{\code{signature(object = "MoveStack")}: transforms coordinates to a different projection method}
      \item{\code{\link{summary}}}{\code{signature(object = "MoveStack")}: summarizes the information of MoveStack object}
       \item{\code{\link{subset}}}{\code{signature(object = "MoveStack")}: subsets the MoveStack object}
      \item{\code{\link{timeLag}}}{\code{signature(object = "MoveStack")}: calculates time lag between consecutive locations}
      \item{\code{\link{timestamps}}}{\code{signature(object = "MoveStack")}: gets the timestamps associated to the coordinates}
      \item{\code{\link{trackId}}}{\code{signature(object = "MoveStack")}: returning the Id of the individual per coordinate}
      \item{\code{\link{turnAngleGc}}}{\code{signature(object = "MoveStack")}: calculates angles between consecutive locations}
      \item{\code{\link{unUsedRecords}}}{\code{signature(object = "MoveStack")}: returns the unUsedRecordsStack object containing the data of the unused records}
  }
}

\author{Marco Smolla & Anne Scharf}

\keyword{classes}
