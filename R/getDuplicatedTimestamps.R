
setGeneric("getDuplicatedTimestamps", function(x, ...) {
  standardGeneric("getDuplicatedTimestamps")
})
setMethod("getDuplicatedTimestamps",
  signature = c("character"),
  definition = function(x, ..., onlyVisible = TRUE) {
    if (length(x) == 1) {
      if (!file.exists(x)) {
        stop("x should be a file on disk but it cant be found")
      }
      if (grepl(".zip", x)) {
        files <- as.character(unzip(x, list = T)$Name)
        if (1 != sum(rd <- (files == "readme.txt")) | length(files) != 2) {
          stop("zip file not as expected")
        }
        x <- unz(x, files[!rd])
      } else {
        x <- file(x)
      }
    } else {
      x <- factor(x)
    }
    callGeneric()
  }
)
setMethod("getDuplicatedTimestamps",
  signature = c("connection"),
  definition = function(x, .., onlyVisible = TRUE) {
    x <- read.csv(x, header = T, stringsAsFactor = T, sep = ",", dec = ".")
    callGeneric()
  }
)
setMethod("getDuplicatedTimestamps",
  signature = c("data.frame"),
  definition = function(x, ..., onlyVisible = TRUE) {
    if (!all(c("timestamp", "sensor.type", "individual.local.identifier") %in% colnames(x))) {
      stop("The entered file does not seem to be from Movebank. Please specify which columns correspond to the individual IDs and timestamps. See help file.")
    }

    if (onlyVisible & ("visible" %in% colnames(x))) {
      callGeneric(
        x = factor(x$individual.local.identifier),
        timestamps = as.POSIXct(strptime(as.character(x$timestamp),
          format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"
        ),
        tz = "UTC"
        ), # need to make character out of it to ensure milli seconds are considerd
        sensorType = as.factor(x$sensor.type), visible = x$visible == "true",
        ...
      )
    } else {
      callGeneric(
        x = factor(x$individual.local.identifier),
        timestamps = as.POSIXct(strptime(as.character(x$timestamp),
          format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"
        ),
        tz = "UTC"
        ), # need to make character out of it to ensure milli seconds are considerd
        sensorType = as.factor(x$sensor.type), ...
      )
    }
  }
)


setMethod("getDuplicatedTimestamps",
  signature = c("factor"),
  definition = function(x, timestamps, sensorType, visible = NULL, ...) {
    stopifnot(class(timestamps)[1] == "POSIXct")
    stopifnot(length(x) == length(timestamps))
    if (missing(sensorType)) {
      sensorType <- "unknown sensor"
    }
    df <- data.frame(i = x, t = format(timestamps, "%Y-%m-%d %H:%M:%OS4"), s = sensorType)
    if (!is.null(visible)) {
      stopifnot(is.logical(visible))
      stopifnot(length(x) == length(visible))
      stopifnot(!any(is.na(visible)))
      df$visible <- rep(1, length(x))
      if (sum(!visible) != 0) {
        df$visible[!visible] <- 2:(1 + sum(!visible))
      }
    }
    if (anyDuplicated(df)) {
      dup <- duplicated(df) | duplicated(df, fromLast = T)
      return(split((1:nrow(df))[dup], df[dup, c("i", "t", "s")], drop = T, sep = "|"))
    } else {
      return(NULL)
    }
  }
)
