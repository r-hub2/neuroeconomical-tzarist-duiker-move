setGeneric("corridor", function(x, speedProp = .75, circProp = .25, minNBsegments = 2, plot = FALSE,  ...) {
  standardGeneric("corridor")
})
setMethod(
  f = "corridor",
  signature = c(x = ".MoveTrackSingle"),
  definition = function(x, speedProp, circProp, minNBsegments, plot, ...) {
    if (!isLonLat(x)) stop("Convert your dataset to longlat projection (use spTransform).")
    if (n.locs(x) > 2) {
      segLength <- apply(cbind(coordinates(x)[-n.locs(x), ], coordinates(x)[-1, ]), 1, function(y) spDistsN1(as.matrix(t(y[1:2])), as.matrix(t(y[3:4])), longlat = T)) ## kilometer
    } else {
      stop("The data-set has 2 or less fixes")
    }

    speedCalc <- speed(x) # meter/second
    speedQuant <- speedCalc >= quantile(speedCalc, probs = speedProp, na.rm = T)

    segMid <- midPoint(coordinates(x)[-n.locs(x), ], coordinates(x)[-1, ])
    # Bart mid points works on spatialpoints* so we can omit the coordinates function here
    ## Marco that is right, but how can you hand over the coordinates once cut at the end and once at the beginning
    segRadius <- segLength / 2
    pkgLoad <-requireNamespace("circular", quietly = TRUE)
    if (pkgLoad) {
      tAzimuth <- trackAzimuth(coordinates(x))
      pAzimuth <- ((180 + tAzimuth) * 2) %% 360

      inCircle <- lapply(1:(n.locs(x) - 1), function(i, segRadius, segMid) {
        which(spDistsN1(pts = as.matrix(segMid), pt = segMid[i, ], longlat = T) <= segRadius[i])
      }, segMid = segMid, segRadius = segRadius)

      inpAzimuth <- lapply(inCircle, function(i, pAzimuth) pAzimuth[i], pAzimuth = pAzimuth)
      circVar <- lapply(lapply(inpAzimuth, circular::circular, units = "degrees"), circular::var.circular)
    } else {
      stop("The packages circular need to be installed")
    }

    if (length(unique(circVar)) == 1) {
      stop("There were less than the required 2 midpoints within the buffer along the whole track to calculate a variance.")
    } else {
      circVar <- unlist(circVar)
    }
    
    if (minNBsegments < 2){minNBsegments <- 2} # ensuring that minNBsegments always is at least 2
    
    circVarQuant <- (circVar <= as.numeric(quantile(circVar[circVar != 0], probs = circProp, na.rm = T))) & circVar != 0
    corrBehav <- which(speedQuant & circVarQuant)
    inCircleCorrBehav <- lapply(inCircle, function(v, behav) v[v %in% behav], behav = corrBehav)

    nBehav <- unlist(lapply(inCircleCorrBehav, length))
    nPoint <- unlist(lapply(inCircle, length))
    corrPointsTmp <- which(nPoint > as.integer(minNBsegments) & (nBehav > (nPoint - nBehav)))

    if (length(corrPointsTmp) == 0) warning("No corridor points found!")
    if (plot) {
      plot(coordinates(x), col = "darkgrey", type = "l")
      if (length(corrPointsTmp) != 0) {
        corrPoints <- segMid[corrPointsTmp, , drop = F]
        maxPoints <- max(nBehav[corrPointsTmp])
        corrPointsCol <- apply(data.frame(corrPointsTmp), 1, function(i, tmp) {
          grDevices::rgb(length(unlist(tmp[i])) / maxPoints, 1 - length(unlist(tmp[i])) / maxPoints, 1)
        }, tmp = inCircleCorrBehav)

        points(corrPoints[, 1], corrPoints[, 2], col = corrPointsCol, pch = 20, ...)
      }
    }
    ## create a MoveBurst object, thus all information (speed, azimuth, ...) are stored in a way, that the actual segment midpoint is represented by the first coordinate of the segement
    
    ## this is the only(?) function in the move package where the property of the segment gets assigned to the first location (all others have it assigned to the last). Maybe change it at some point, or at least keep it in mind when (if) creating a new function for the new package
    x$segMid_x <- c(segMid[, 1], NA) # not elegant to add a zero again
    x$segMid_y <- c(segMid[, 2], NA)
    x$speed <- c(speedCalc, NA)
    x$azimuth <- c(tAzimuth, NA)
    x$pseudoAzimuth <- c(pAzimuth, NA)
    x$circVar <- c(circVar, NA)
    corr <- rep("no corridor", n.locs(x))
    corr[corrPointsTmp] <- "corridor"
    return(move::burst(x, f = factor(corr[-length(corr)])))
  }
)

setMethod(
  f = "corridor",
  signature = c(x = ".MoveTrackStack"),
  definition = function(x, speedProp, circProp, minNBsegments, ...) {
    lapply(split(x), corridor,speedProp=speedProp, circProp=circProp, minNBsegments=minNBsegments, plot=plot,...)
  }
)


# This code has been copied from maptools to prepare for maptools becoming archived. By using this code we ensure the calculations stay numerically the same

# Copyright (c) 2006 Roger Bivand

# http://www.geocities.com/sualeh85/sfweb/QTable.html
# http://patriot.net/users/abdali/ftp/qibla.pdf
# http://www.patriot.net/users/abdali/ftp/praytimer.zip
# http://www.world-gazetteer.com/

gzAzimuth <- function(from, to, type="snyder_sphere") {
  deg2rad <- function(x) x*pi/180
  rad2deg <- function(x) x*180/pi
  # note negative longitudes
  if (is.matrix(from)) {
    lon <- -deg2rad(from[,1])
    lat <- deg2rad(from[,2])
  } else {
    lon <- -deg2rad(from[1])
    lat <- deg2rad(from[2])
  }
  if (is.matrix(to)) {
    if (nrow(to) > 1) stop("to: single coordinate")
    to <- c(to)
  } 
  lon0 <- -deg2rad(to[1])
  lat0 <- deg2rad(to[2])
  # bug found by Sebastian Luque
  dflon = lon-lon0
  # results in degrees from N, negative west
  if (type == "abdali") res <- atan2(sin(dflon), ((cos(lat)*tan(lat0)) - 
                                                    (sin(lat)*cos(dflon))))
  else if (type == "snyder_sphere") res <- atan2((cos(lat0)*sin(dflon)), 
                                                 (cos(lat)*sin(lat0)) - (sin(lat)*cos(lat0)*cos(dflon)))
  else stop("type unkown")
  is.na(res) <- lon == lon0 & lat == lat0 
  rad2deg(res)
}

trackAzimuth <- function(track, type="snyder_sphere") {
  if (!is.matrix(track)) stop("track must be two-column matrix")
  if (ncol(track) != 2) stop("track must be two-column matrix")
  n1 <- nrow(track)-1
  if (n1 < 2) stop("less than two points")
  res <- numeric(n1)
  for (i in seq(along=res)) res[i] <- gzAzimuth(track[i,], track[(i+1),],
                                                type=type)
  res
} 

