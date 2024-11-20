setGeneric("move2ade", function(x) {
  standardGeneric("move2ade")
})
setMethod("move2ade",
  signature = ".MoveTrackSingle",
  definition = function(x) {
    SpatialPointsDataFrame(as(x, "SpatialPoints"),
      data = data.frame(id = rep(rownames(idData(x, drop = F)), n.locs(x)))
    )
  }
)

setMethod("move2ade",
  signature = ".MoveTrackStack",
  definition = function(x) {
    #            SpatialPointsDataFrame(coords=coordinates(x),
    #                                  data=data.frame(id=as.character(trackId(x))))
    SpatialPointsDataFrame(as(x, "SpatialPoints"), data = data.frame(id = as.character(trackId(x))))
  }
)

## ltraj adehabitat
# define ltrajs when neede
if (!isClass("ltraj")) {
  setOldClass("ltraj")
}


setAs("Move", "ltraj", function(from) {
  if (!requireNamespace("adehabitatLT")) {
    stop("adehabitatLT needs to be installed for this conversion to work")
  }
  if (isLonLat(from)) {
    warning("Converting a long lat projected object while the ltraj does not deal with long lat projected data")
  }
  adehabitatLT::as.ltraj(as.data.frame(coordinates(from)), date = timestamps(from), id = rownames(from@idData), typeII = T, infolocs = data.frame(sensor = from@sensor, from@data))
})
setAs("MoveStack", "ltraj", function(from) {
  if (!requireNamespace("adehabitatLT")) {
    stop("adehabitatLT needs to be installed for this conversion to work")
  }
  if (isLonLat(from)) {
    warning("Converting a long lat projected object while the ltraj does not deal with long lat projected data")
  }
  adehabitatLT::as.ltraj(as.data.frame(coordinates(from)), date = timestamps(from), id = from@trackId, typeII = T, infolocs = data.frame(sensor = from@sensor, from@data))
})

setAs("ltraj", "Move", function(from) {
  if (!requireNamespace("adehabitatLT")) {
    stop("adehabitatLT needs to be installed for this conversion to work")
  }

  if (!inherits(from, "ltraj")) {
    stop("from should be of class \"ltraj\"")
  }
  if (length(from) != 1) {
    stop("Can only convert one individual to a move object")
  }
  if (!attr(from, "typeII")) {
    stop("Can only work on typeII objects")
  }
  spdf <- adehabitatLT::ltraj2spdf(from)
  new("Move", data = (attr(from[[1]], "infolocs")), spdf, sensor = rep(factor("unknown"), nrow(spdf)), timestamps = spdf$date, idData = data.frame(row.names = paste0(attr(from[[1]], "id"), "_", attr(from[[1]], "id")), burst = attr(from[[1]], "burst"), id = attr(from[[1]], "id")), sensorUnUsedRecords = factor(levels = "unknown"))
})

setAs("ltraj", "MoveStack", function(from) {
  if (!inherits(from, "ltraj")) {
    stop("from should be of class \"ltraj\"")
  }
  res <- list()
  for (i in 1:length(from))
  {
    res[[i]] <- as(from[i], "Move")
  }
  moveStack(res)
})

## telemetry from ctmm
setAs("telemetry", "Move", function(from) {
  if (inherits(from, "telemetry")) {
    mv <- move(
      x = from$x, y = from$y,
      time = as.POSIXct(from$t, origin = "1970-01-01", tz = from@info$timezone),
      data = data.frame(from),
      proj = if (is.null(from@info$projection)) {
        as.character(NA)
      } else {
        from@info$projection
      },
      animal = if (is.null(from@info$identity)) {
        "unnamed"
      } else {
        as.character(from@info$identity)
      }
    )
    if ("VAR.xy" %in% names(from)) {
      mv$error.loc.mts <- sqrt(from$VAR.xy)
    }
    return(mv)
  }
})

setAs("list", "MoveStack", function(from) {
  if (unique(unlist(lapply(from, class))) == "telemetry") {
    stk <- moveStack(lapply(from, function(x) {
      mv <- move(
        x = x$x, y = x$y,
        time = as.POSIXct(x$t, origin = "1970-01-01", tz = x@info$timezone),
        data = data.frame(x),
        proj = if (is.null(x@info$projection)) {
          as.character(NA)
        } else {
          x@info$projection
        },
        animal = if (is.null(x@info$identity)) {
          "unnamed"
        } else {
          as.character(x@info$identity)
        }
      )
      if ("VAR.xy" %in% names(x)) {
        mv$error.loc.mts <- sqrt(x$VAR.xy)
      }
      return(mv)
    }), forceTz = from[[1]]@info$timezone)
    return(stk)
  } else {
    stop("This list cannot be transformed into a movestack object, only a list of objects of class \"telemetry\" can be transformed")
  }
})


# track_xyt from amt
setAs("track_xyt", "Move", function(from) {
  if (inherits(from, "track_xyt")) {
    if ("id" %in% names(from)) {
      if (any(duplicated(paste0(from$t_, from$id)))) {
        warning("data contains duplicates. By default 1st entery is kept, and subsequent duplicates are removed. If this is not wanted, please remove duplicates from original input object")
        from <- from[!duplicated(paste0(from$t_, from$id)), ]
      }
    } else {
      if (any(duplicated(from$t_))) {
        warning("data contains duplicates. By default 1st entery is kept, and subsequent duplicates are removed. If this is not wanted, please remove duplicates from original input object")
        from <- from[!duplicated(from$t_), ]
      }
    }
    mv <- move(
      x = from$x_,
      y = from$y_,
      time = from$t_,
      data = data.frame(from[!names(from) %in% c("x_", "y_", "t_", "id")]),
      proj = if (is.null(attr(from, "crs"))) {
        as.character(NA)
      } else {
        attr(from, "crs")
      },
      animal = if ("id" %in% names(from)) {
        as.character(from$id)
      } else {
        "unnamed"
      }
    )
    return(mv)
  }
})

## track from bcpa
setAs("track", "Move", function(from) {
  if (inherits(from, "track")) {
    warning("Objects of class 'track' do not contain projection information. You can add this information to the move object with the function 'projection()'")
    if ("POSIXct" %in% class(from$Time) | "POSIXt" %in% class(from$Time)) {
      mv <- move(
        x = from$X,
        y = from$Y,
        time = from$Time
      )
      return(mv)
    } else {
      stop("Time is not in POSIXct format. Please transform the 'Time' element of the 'track' object into POSIXct format.")
    }
  }
})


## binClstPath and binClstStck from EMbC
setAs("binClstPath", "Move", function(from) {
  if (inherits(from, "binClstPath")) {
    mv <- move(
      x = from@pth$lon,
      y = from@pth$lat,
      time = from@pth$dTm,
      data = data.frame(velocity.m.s = from@X[, 1], turn.rad = from@X[, 2], W = from@W, A = from@A),
      proj = from@tracks@proj4string@projargs,
      animal = "unnamed"
    )
    return(mv)
  }
})

setAs("binClstStck", "MoveStack", function(from) {
  if (inherits(from, "binClstStck")) {
    mv <- moveStack(lapply(1:length(from@bCS), function(i) {
      move(
        x = from@bCS[[i]]@pth$lon,
        y = from@bCS[[i]]@pth$lat,
        time = from@bCS[[i]]@pth$dTm,
        data = data.frame(velocity.m.s = from@bCS[[i]]@X[, 1], turn.rad = from@bCS[[i]]@X[, 2], W = from@bCS[[i]]@W, A = from@bCS[[i]]@A),
        proj = if (from@bCS[[i]]@tracks@proj4string@projargs == "") {
          as.character(NA)
        } else {
          from@bCS[[i]]@tracks@proj4string@projargs
        },
        animal = paste0("unnamed", i)
      )
    }), forceTz = if (is.null(attr(from@bCS[[1]]@pth$dTm[1], "tzone"))) {
      Sys.timezone()
    } else {
      attr(from@bCS[[1]]@pth$dTm[1], "tzone")
    })
    return(mv)
  }
})


## data.frame from Movebank
setAs("data.frame", "Move", function(from) {
  if (inherits((from), "data.frame")) {
    warning("Assumptions: data.frame comes from Movebank; coordinates are in lat/long; timestamp format is: \"%Y-%m-%d %H:%M:%S\"; timezone is UTC. If this is not the case please use the alternative import function")

    if (sum(c(all(c("timestamp", "location.long", "location.lat", "individual.local.identifier", "sensor.type") %in% names(from)), all(c("location_lat", "location_long", "timestamp", "individual_local_identifier", "sensor_type") %in% names(from)))) == 0) stop("The entered data.frame does not seem to be from Movebank. Please use the alternative import function.")
    if (sum(c(any(c("location.long", "location.lat", "individual.local.identifier", "sensor.type") %in% names(from)), any(c("location_lat", "location_long", "individual_local_identifier", "sensor_type") %in% names(from)))) == 2) stop("The entered data.frame seems to contain several columns refering to longitude, latitude, individual ID or sensor type. Please use the alternative import function.")

    ## data.frame downloaded from webpage
    if (all(c("timestamp", "location.long", "location.lat", "individual.local.identifier", "sensor.type") %in% names(from))) {
      from$individual.local.identifier <- as.character(from$individual.local.identifier)
      from <- from[order(from$individual.local.identifier, from$timestamp), ]
      mv <- move(
        x = from$location.long,
        y = from$location.lat,
        time = as.POSIXct(from$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        data = from,
        proj = CRS("+proj=longlat +datum=WGS84"),
        animal = from$individual.local.identifier,
        sensor = from$sensor.type
      )
      return(mv)
    }

    ## data.frame downloaded with getMovebankLocationData
    if (all(c("location_lat", "location_long", "timestamp", "individual_local_identifier", "sensor_type") %in% names(from))) {
      from$individual_local_identifier <- as.character(from$individual_local_identifier)
      from <- from[order(from$individual_local_identifier, from$timestamp), ]
      mv <- move(
        x = from$location_long,
        y = from$location_lat,
        time = as.POSIXct(from$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        data = from,
        proj = CRS("+proj=longlat +datum=WGS84"),
        animal = from$individual_local_identifier,
        sensor = from$sensor_type
      )
      return(mv)
    }
  }
})
