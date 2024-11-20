setGeneric("move", function(x, y, time, data, proj = as.character(NA), ...) standardGeneric("move"))
setMethod(
  f = "move",
  signature = c(x = "character", y = "missing", time = "missing", data = "missing", proj = "missing"),
  definition = function(x, ...) {
    if (!file.exists(x)) {
      stop("x should be a file on disk but it cant be found")
    }
    if (grepl(".zip", x)) {
      files <- as.character(unzip(x, list = T)$Name)
      if (1 != sum(rd <- (files == "readme.txt")) | length(files) != 2) {
        stop("zip file not as expected")
      }
      m <- move(unz(x, files[!rd]), ...)
      m@license <- paste(m@license, readLines(con <- unz(x, files[rd])), collapse = "\n")
      close(con)
      return(m)
    } else {
      return(move(file(x), ...))
    }
  }
)
setMethod(
  f = "move",
  signature = c(x = "connection", y = "missing", time = "missing", data = "missing", proj = "missing"),
  definition = function(x, removeDuplicatedTimestamps = F, ...) {
    if (version$major == "3" & version$minor == "1.0") { # exception to type convert back then that got reverted, setting colclasses causes problems with quoted downloads from envData
      df <- read.csv(x, header = TRUE, sep = ",", dec = ".", stringsAsFactors = T, colClasses = c(location.long = "numeric", location.lat = "numeric"))
    } else {
      df <- read.csv(x, header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)
    }
    if (!all(c(
      "timestamp",
      "location.long",
      "location.lat",
      # 			     "study.timezone",
      # 			     "study.local.timestamp",
      "sensor.type",
      "individual.local.identifier",
      "individual.taxon.canonical.name"
    ) %in% colnames(df))) {
      stop("The entered file does not seem to be from Movebank. Please use the alternative import function.")
    }
    if ("study.name" %in% colnames(df)) {
      study <- as.character(unique(df$study.name))
    } else {
      study <- character()
    }

    if (any(dups <- duplicated(do.call("paste", c(df[duplicated(df$timestamp) | duplicated(df$timestamp, fromLast = T), names(df) != "event.id"], list(sep = "__")))))) {
      # first find atleast the ones where the timestamp (factor) is duplicated
      warning("Exact duplicate records removed (n=", sum(dups), ") (movebank allows them but the move package can't deal with them)")
      df <- df[!duplicated(do.call("paste", c(df[, names(df) != "event.id"], list(sep = "__")))), ]
      # cant use dups here since it that uses the optimization of only looking at timestamps first
    }

    df$timestamp <- as.POSIXct(strptime(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tz = "UTC") # need to make character out of it to ensure milli seconds are considerd
    if ("study.local.timestamp" %in% names(df)) {
      df$study.local.timestamp <- as.POSIXct(strptime(df$study.local.timestamp, format = "%Y-%m-%d %H:%M:%OS"))
    }

    if (any(tapply(df$sensor.type, df$individual.local.identifier, length) != 1)) {
      df <- df[with(df, order(df$individual.local.identifier, timestamp)), ]
    }
    df$individual.local.identifier <- as.factor(df$individual.local.identifier)
    levels(df$individual.local.identifier) <- validNames(levels(factor(df$individual.local.identifier))) # changing names to 'goodNames' skipping spaces

    if ("visible" %in% colnames(df)) {
      v <- df$visible == "false"
    } else {
      v <- F
    }
    unUsed <- is.na(df$location.long) | is.na(df$location.lat) | v | is.na(df$individual.local.identifier) | df$individual.local.identifier == ""
    sensor <- df$sensor.type
    timestamps <- df$timestamp
    individual.local.identifier <- df$individual.local.identifier
    uniquePerID <- unlist(lapply(df, function(x, y) {
      all(tapply(x, y, function(x) {
        length(unique(x))
      }) == 1)
    }, y = factor(df$individual.local.identifier)))
    uniquePerID[c("location.long", "location.lat")] <- F
    idData <- subset(df, select = names(uniquePerID[uniquePerID]), !duplicated(df$individual.local.identifier))
    rownames(idData) <- idData$individual.local.identifier
    df <- df[, !(names(df) %in% unique(c("sensor.type", "timestamps", colnames(idData))))]
    unUsedDf <- df[unUsed, , drop = F]
    unUsedRecords <- new(".unUsedRecords", dataUnUsedRecords = unUsedDf, timestampsUnUsedRecords = timestamps[unUsed], sensorUnUsedRecords = sensor[unUsed])
    if (stk <- nrow(idData) != 1) {
      unUsedRecords <- new(".unUsedRecordsStack", unUsedRecords, trackIdUnUsedRecords = individual.local.identifier[unUsed])
    }
    if (any(s <- !(rownames(idData) %in% as.character(unique(individual.local.identifier[!unUsed]))))) {
      warning("omiting ", sum(s), " individual(s) because they do not have observation data")
      unUsedRecords <- unUsedRecords[(as.character(unUsedRecords@trackIdUnUsedRecords) %in% rownames(idData)[!s]), ]
      idData <- idData[!s, drop = FALSE, ]
      unUsedRecords@trackIdUnUsedRecords <- factor(unUsedRecords@trackIdUnUsedRecords, levels = rownames(idData))
    }

    df <- df[!unUsed, , drop = FALSE]
    coordinates(df) <- ~ location.long + location.lat
    proj4string(df) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
    if (class(df)[1] == "SpatialPoints") { # to make sure we feed a spdf
      df <-
        new("SpatialPointsDataFrame", df, data = data.frame(coordinates(df)))
    }
    track <-
      new(
        ".MoveTrack",
        df,
        timestamps = timestamps[!unUsed],
        sensor = sensor[!unUsed],
        idData = idData
      )
    individual.local.identifier <-
      factor(individual.local.identifier[!unUsed])
    if (removeDuplicatedTimestamps) {
      message(
        "removeDupilcatedTimestamps was set to TRUE, this will retain the first of multiple records with the same animal ID and timestamp, and remove any subsequent duplicates"
      )
      dupsDf <-
        (data.frame(format(timestamps(track), "%Y %m %d %H %M %OS4"), track@sensor))
      dupsDfUn <-
        (data.frame(
          format(timestamps(unUsedRecords), "%Y %m %d %H %M %OS4"),
          unUsedRecords@sensorUnUsedRecords
        ))

      if (stk) {
        dupsDf <- data.frame(id = individual.local.identifier, dupsDf)
        dupsDfUn <- data.frame(id = unUsedRecords@trackIdUnUsedRecords, dupsDfUn)
      }
      dups <- duplicated(dupsDf)
      track <- track[!dups, ]
      individual.local.identifier <- individual.local.identifier[!dups]
      if (sum(dups) != 0) {
        warning(sum(dups), " location(s) is/are removed by removeDuplicatedTimestamps")
      }
      unUsedRecords <- unUsedRecords[inNormRe <- !(apply(dupsDfUn, 1, paste, collapse = "_") %in% apply(dupsDf, 1, paste, collapse = "_")), T]
      if (sum(!inNormRe) != 0) {
        warning(sum(!inNormRe), " location(s) is/are removed by removeDuplicatedTimestamps from the un used records")
      }
    }
    if (stk) {
      return(new("MoveStack", track, unUsedRecords, trackId = individual.local.identifier, study = study))
    } else {
      return(new("Move", track, unUsedRecords, study = study))
    }
  }
)

# if non-Movebank data are used, table is new defined
setMethod(
  f = "move",
  signature = c(x = "numeric", y = "numeric", time = "POSIXct", data = "missing", proj = "ANY"),
  definition = function(x, y, time, data, proj, ...) {
    data <- data.frame(x, y, time)
    move(x = x, y = y, time = time, proj = proj, data = data, ...)
  }
)

setMethod(
  f = "move",
  signature = c(x = "numeric", y = "numeric", time = "POSIXct", data = "data.frame", proj = "character"),
  definition = function(x, y, time, data, proj, ...) {
    move(x = x, y = y, time = time, proj = CRS(proj), data = data, ...)
  }
)

setMethod(
  f = "move",
  signature = c(x = "numeric", y = "numeric", time = "POSIXct", data = "data.frame", proj = "missing"),
  definition = function(x, y, time, data, proj, ...) {
    move(x = x, y = y, time = time, proj = CRS(), data = data, ...)
  }
)

setMethod(
  f = "move",
  signature = c(x = "numeric", y = "numeric", time = "POSIXct", data = "data.frame", proj = "CRS"),
  definition = function(x, y, time, data, proj, sensor = "unknown", animal = "unnamed", ...) {
    # 		  data$individual.local.identifier <- animal
    if (1 == length(sensor)) {
      sensor <- rep(sensor, length(x))
    }

    sensor <- factor(sensor)
    if (1 == length(animal)) {
      animal <- rep(animal, length(x))
    }
    animal <- factor(animal)

    if (any(is.na(x) | is.na(y))) {
      warning("There were NA locations detected and omitted. Currently they are not stored in unusedrecords")
      s <- !(is.na(x) | is.na(y))
      data <- data[s, ]
      time <- time[s]
      animal <- animal[s]
      sensor <- sensor[s]
      x <- x[s]
      y <- y[s]
    }
    animal <- factor(animal, levels = unique(animal))
    levels(animal) <- validNames(levels(factor(animal))) # changing names to 'goodNames' skipping spaces

    ids <- as.list(as.character(unique(animal)))
    uniquePerID <-
      unlist(lapply(colnames(data), function(x, data, animal) {
        nrow(unique(cbind(data[, x], animal)))
      }, data = data, animal = animal)) == sum(!duplicated(animal))
    names(uniquePerID) <- colnames(data)
    # 		  uniquePerID["sensor"] <- FALSE
    idData <- subset(data, select = names(uniquePerID[uniquePerID]), !duplicated(animal))


    if (length(unique(idData$citation)) > 0) {
      if (length(unique(idData$citation)) > 1) {
        warning("There were more than one citation for this study found! Only using the first.")
      }
      citations <- as.character(unique(idData$citation))[1]
    } else {
      citations <- character()
    }

    rownames(idData) <- unique(animal)
    auxData <- data.frame(data[names(data)[!names(data) %in% c(colnames(idData))]])
    crds <- cbind(x, y, deparse.level = 0)
    if (ncol(auxData) == 0) {
      auxData <- data.frame(auxData, empty = NA)
      row.names(auxData) <- row.names(crds)
    }

    spdf <- SpatialPointsDataFrame(
      #  coords = cbind(location.long=x,location.lat=y),
      coords = crds,
      data = auxData,
      proj4string = proj,
      match.ID = TRUE
    )

    if (length(ids) == 1) {
      return(new("Move",
        timestamps = time,
        sensor = sensor,
        sensorUnUsedRecords = factor(levels = levels(sensor)),
        spdf,
        citation = citations,
        idData = idData
      ))
    } else {
      return(new("MoveStack",
        spdf,
        idData = idData,
        sensor = sensor,
        sensorUnUsedRecords = factor(levels = levels(sensor)),
        timestamps = time,
        citation = citations,
        trackId = animal,
        trackIdUnUsedRecords = factor(levels = levels(animal))
      ))
    }
  }
)

## convert other classes to move objects

setMethod(
  f = "move",
  signature = c(
    x = "ltraj", y = "missing", time = "missing", data = "missing", proj = "missing"
  ),
  definition = function(x, ...) {
    if (length(x) == 1) {
      return(as(x, "Move"))
    } else {
      return(as(x, "MoveStack"))
    }
  }
)


setMethod(
  f = "move",
  signature = c(x = "telemetry", y = "missing", time = "missing", data = "missing", proj = "missing"),
  definition = function(x, ...) {
    return(as(x, "Move"))
  }
)

setMethod(
  f = "move",
  signature = c(x = "list", y = "missing", time = "missing", data = "missing", proj = "missing"),
  definition = function(x, ...) {
    return(as(x, "MoveStack"))
  }
)

setMethod(
  f = "move",
  signature = c(x = "track_xyt", y = "missing", time = "missing", data = "missing", proj = "missing"),
  definition = function(x, ...) {
    return(as(x, "Move"))
  }
)

setMethod(
  f = "move",
  signature = c(x = "track", y = "missing", time = "missing", data = "missing", proj = "missing"),
  definition = function(x, ...) {
    return(as(x, "Move"))
  }
)

setMethod(
  f = "move",
  signature = c(x = "binClstPath", y = "missing", time = "missing", data = "missing", proj = "missing"),
  definition = function(x, ...) {
    return(as(x, "Move"))
  }
)

setMethod(
  f = "move",
  signature = c(x = "binClstStck", y = "missing", time = "missing", data = "missing", proj = "missing"),
  definition = function(x, ...) {
    return(as(x, "MoveStack"))
  }
)

setMethod(
  f = "move",
  signature = c(x = "data.frame", y = "missing", time = "missing", data = "missing", proj = "missing"),
  definition = function(x, ...) {
    return(as(x, "Move"))
  }
)
