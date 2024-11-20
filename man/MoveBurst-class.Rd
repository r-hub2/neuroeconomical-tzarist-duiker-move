\name{MoveBurst}

\alias{MoveBurst-class}
\alias{.MoveTrackSingleBurst-class}

\docType{class}

\title{MoveBurst class}

\description{The class \code{MoveBurst} is used to store the track of one individual with a categorical assignment to each segment. Every segment between two locations has a class of for example a behavioral category. A \code{MoveBurst} object is created with the functions \link{burst} and \link{corridor}.}

\note{The \code{MoveBurst} object contains a \code{.MoveGeneral}, \code{.MoveTrackSingleBurst} and \code{\link[=.unUsedRecords-class]{.unUsedRecords}} object which can be used to program against.}

\section{Slots}{
  \describe{
    \item{bbox}{See \code{\link{Move-class}}}
    \item{burstId}{Id of the behavioral categorization assigned to each segment, one shorter then the number of locations.}
    \item{citation}{See \code{\link{Move-class}}}
    \item{coords}{See \code{\link{Move-class}}}
    \item{coords.nrs}{See \code{\link{Move-class}}}
    \item{data}{See \code{\link{Move-class}}}
    \item{dataUnUsedRecords}{See \code{\link{Move-class}}}
    \item{dateCreation}{See \code{\link{Move-class}}}
    \item{idData}{See \code{\link{Move-class}}}
    \item{license}{See \code{\link{Move-class}}}
    \item{proj4string}{See \code{\link{Move-class}}}
    \item{sensor}{See \code{\link{Move-class}}}
    \item{sensorUnUsedRecords}{See \code{\link{Move-class}}}
    \item{study}{See \code{\link{Move-class}}}
    \item{timestamps}{See \code{\link{Move-class}}}
    \item{timestampsUnUsedRecords}{See \code{\link{Move-class}}}
  }
}

\section{Methods}{
  \describe{
      \item{\code{\link{angle}}}{\code{signature(object = "MoveBurst")}: calculates angles between consecutive locations}
      \item{\code{\link{as.data.frame}}}{\code{signature(object = "MoveBurst")}: extracts the spatial data frame}
      \item{\code{\link{brownian.bridge.dyn}}}{\code{signature(object = "MoveBurst")}: calculates the utilization distribution (UD) of the given track using the dynamic Brownian Bridge Movement Model}
      \item{\code{\link{brownian.motion.variance.dyn}}}{\code{signature(object = "MoveBurst")}: calculates the motion variance of the dynamic Brownian Bridge Movement Model}
      \item{\code{\link{burstId}}}{\code{signature(object = "MoveBurst")}: returns the Id of the behavioral categorization assigned to each segment}
      \item{\code{\link{citations}}}{\code{signature(object = "MoveBurst")}: extracts or sets the citation}
      \item{\code{\link{coordinates}}}{\code{signature(object = "MoveBurst")}: extracts the coordinates from the track}
      \item{\code{\link{corridor}}}{\code{signature(object = "MoveBurst")}: identifies track segments whose attributes suggest corridor use behavior} 
      \item{\code{\link{distance}}}{\code{signature(object = "MoveBurst")}: calculates distances between consecutive locations}
      \item{\code{\link{dynBGB}}}{\code{signature(object = "MoveBurst")}: calculates the utilization distribution (UD) of the given track using the dynamic Bivariate Gaussian Bridge model}
      \item{\code{\link{dynBGBvariance}}}{\code{signature(object = "MoveBurst")}: calculates the orthogonal and parallel motion variance of the dynamic Brownian Bridge Movement Model}
      \item{\code{\link{equalProj}}}{\code{signature(object = "MoveBurst")}: checks whether all objects of a list are in the same projection}
      \item{\code{\link{hrBootstrap}}}{\code{signature(object = "MoveBurst")}: calculates and plots the area of the Minimum Convex Polygon of a track}
      \item{\code{\link{idData}}}{\code{signature(object = "MoveBurst")}: returns or replaces the idData slot}
      \item{\code{\link{interpolateTime}}}{\code{signature(object = "MoveBurst")}: interpolates trajectories based on time}
      \item{\code{\link{lines}}}{\code{signature(object = "MoveBurst")}: add lines of the track of the animal to a plot}
      \item{\code{\link{move2ade}}}{\code{signature(object = "MoveBurst")}: converts to a adehabitat compatible object}
      \item{\code{\link{n.locs}}}{\code{signature(object = "MoveBurst")}: calculates number of locations}
      \item{\code{\link{plot}}}{\code{signature(object = "MoveBurst")}: plots the track of the animal}
      \item{\code{\link{plotBursts}}}{\code{signature(object = "MoveBurst")}: plots the centroids of a bursted track}
      \item{\code{\link{points}}}{\code{signature(object = "MoveBurst")}: add points of the track of the animal to a plot}
      \item{\code{\link{seglength}}}{\code{signature(object = "MoveBurst")}: calculates the length of each segment of a track}
      \item{\code{\link{sensor}}}{\code{signature(object = "MoveBurst")}: extracts the sensor(s) used to record the coordinates}
      \item{\code{\link{show}}}{\code{signature(object = "MoveBurst")}: displays summary the MoveBurst object}
      \item{\code{\link{speed}}}{\code{signature(object = "MoveBurst")}: calculates speed between consecutive locations}
      \item{\code{\link{split}}}{\code{signature(object = "MoveBurst")}: splits a MoveBurst into a list of Move objects}
      \item{\code{\link{spTransform}}}{\code{signature(object = "MoveBurst")}: transforms coordinates to a different projection method}
      \item{\code{\link{summary}}}{\code{signature(object = "MoveBurst")}: summarizes the information of MoveBurst object}
       \item{\code{\link{subset}}}{\code{signature(object = "MoveBurst")}: subsets the MoveBurst object}
      \item{\code{\link{timeLag}}}{\code{signature(object = "MoveBurst")}: calculates time lag between consecutive locations}
      \item{\code{\link{timestamps}}}{\code{signature(object = "MoveBurst")}: gets the timestamps associated to the coordinates}
      \item{\code{\link{turnAngleGc}}}{\code{signature(object = "MoveBurst")}: calculates angles between consecutive locations}
      \item{\code{\link{unUsedRecords}}}{\code{signature(object = "MoveBurst")}: returns the unUsedRecords object containing the data of the unused records}
  }
}

\author{Marco Smolla & Anne Scharf}

