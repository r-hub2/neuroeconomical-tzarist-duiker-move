setGeneric("moveStack", function(x, ...) standardGeneric("moveStack"))
setMethod(
  f = "moveStack",
  signature = c(x = "Move"),
  definition = function(x, ..., forceTz = NULL) {
    callGeneric(c(list(x), list(...)), forceTz = forceTz)
  }
)
setMethod(
  f = "moveStack",
  signature = c(x = "MoveStack"),
  definition = function(x, ..., forceTz = NULL) {
    callGeneric(c(list(x), list(...)), forceTz = forceTz)
  }
)
setMethod(
  f = "moveStack",
  signature = c(x = "list"),
  definition = function(x, forceTz = NULL, ...) {
    if (!all((cls <- unlist(lapply(x, class))) %in% c("Move", "MoveStack"))) {
      stop("One or more objects in the list are not from class Move")
    }
    if (!all(unlist(lapply(x, validObject)))) {
      stop("One or more objects in the list are not valid Move(Stack) objects")
    }
    if (length(unique(unlist(as.character(lapply(lapply(x, timestamps), attr, "tzone"))))) != 1) {
      stop("Not all move object in the list have the same timezone in the timestamp")
    }
    if (any(cls == "MoveStack")) {
      x <- unlist(
        recursive = F,
        unlist(
          recursive = F,
          lapply(x, function(t) ifelse(class(t) == "MoveStack", list(split(t)), list(list(t))))
        )
      )
    }
    if (!equalProj(x)) {
      stop("All objects need to be equally projected.")
    }
    if (any(duplicated(unlist(lapply(lapply(x, slot, "idData"), rownames))))) {
      nnames <- make.names(unlist(lapply(lapply(x, slot, "idData"), rownames)), unique = T)
      lapply(1:length(nnames), function(z, nnames, x) {
        rownames(x[[z]]@idData) <- nnames[z]
        return(x[[z]])
      }, x = x, nnames = nnames)
      warning("Detected duplicated names. Renamed the duplicated individuals accordingly.")
    }
    allData <- lapply(x, function(y) slot(y, "data"))
    allColumns <- unique(unlist(lapply(allData, names)))
    ### DATA
    DATA <- do.call("rbind", lapply(allData, FUN = function(entry) {
      missingColumns <- allColumns[which(!allColumns %in% names(entry))]
      entry[, missingColumns] <- NA
      entry[, allColumns, drop = F]
    })) # thanks to: Karl Ove Hufthammer
    ### idData
    allidData <- lapply(x, function(y) slot(y, "idData"))
    allidColumns <- unique(unlist(lapply(allidData, names)))
    IDDATA <- lapply(allidData, FUN = function(entry) {
      missingColumns <- allidColumns[which(!allidColumns %in% names(entry))]
      entry[, missingColumns] <- NA
      entry[, allidColumns, drop = F]
    })
    if (0 == sum(unlist(lapply(lapply(IDDATA, dim), tail, 1)))) {
      IDDATA <- do.call("rbind", lapply(IDDATA, cbind, a = 1))
      IDDATA$a <- NULL
    } else {
      IDDATA <- do.call("rbind", IDDATA)
    }
    rownames(IDDATA) <- validNames(rownames(IDDATA))
    o <- lapply(x, as, "SpatialPoints")
    o <- mapply("row.names<-",
      x = o,
      mapply(
        paste,
        lapply(lapply(x, idData, drop = F), row.names),
        lapply(o, row.names),
        sep = ".",
        SIMPLIFY = F
      )
    )
    spdftmp <- SpatialPointsDataFrame(do.call(rbind, o), data = DATA)
    # unused records
    unUsedList <- lapply(x, as, ".unUsedRecords")
    unUsedList <- mapply(function(x, i, j) {
      if (length(i) != 0) {
        x[i, j]
      } else {
        x
      }
    },
    unUsedList,
    i = lapply(
      lapply(unUsedList, slot, "timestampsUnUsedRecords"),
      function(x) {
        if (is.null(x)) {
          return(NULL)
        } else {
          return(order(x))
        }
      }
    ),
    MoreArgs = list(j = T), SIMPLIFY = F
    )
    tz <- unique(unlist(lapply(
      tsl <- lapply(unUsedList, slot, "timestampsUnUsedRecords"),
      attr,
      "tzone"
    )))
    if (!(length(tz) == 1 | is.null(tz))) {
      stop("Concatinating multiple time zone for unusedrecords")
    }
    ts <- do.call("c", tsl[!unlist(lapply(tsl, is.null))])
    if (!is.null(forceTz) & !is.null(ts)) {
      attr(ts, "tzone") <- forceTz
    }
    dataUnUsed <- lapply(unUsedList, slot, "dataUnUsedRecords")
    cols <- unique(unlist(lapply(dataUnUsed, colnames)))
    dataUnUsed <- lapply(dataUnUsed, function(x, i) {
      i <- i[!(i %in% colnames(x))]
      if (nrow(x) == 0) {
        x <- as.data.frame(matrix(nrow = 0, ncol = length(i)))
        colnames(x) <- i
      } else {
        x[, i] <- NA
      }
      return(x)
    }, i = cols) # fill unused columns with NA
    sensorLevels <- unique(unlist(c(
      lapply(lapply(x, slot, "sensor"), levels),
      lapply(lapply(x, slot, "sensorUnUsedRecords"), levels)
    )))
    unUsed <- new(".unUsedRecordsStack",
      timestampsUnUsedRecords = ts,
      dataUnUsedRecords = do.call("rbind", dataUnUsed),
      sensorUnUsedRecords = factor(unlist(lapply(
        lapply(unUsedList, slot, "sensorUnUsedRecords"),
        as.character
      )),
      levels = sensorLevels
      ),
      trackIdUnUsedRecords = factor(unlist(mapply(
        rep, rownames(IDDATA),
        unlist(lapply(tsl, length))
      )),
      levels = rownames(IDDATA)
      )
    )
    length <- lapply(x, n.locs)
    ts <- do.call("c", tt <- lapply(x, timestamps))
    if (!is.null(forceTz)) {
      attr(ts, "tzone") <- forceTz
      if (!(forceTz %in% OlsonNames())) {
        warning("Possibly changing to not existing time zone")
      }
    }
    if (!is.null(unique(unlist(lapply(tt, attr, "tzone"))))) {
      if (unique(unlist(lapply(tt, attr, "tzone"))) == "" & is.null(attr(ts, "tzone"))) {
        attr(ts, "tzone") <- ""
      }
    }
    res <- new("MoveStack",
      idData = IDDATA,
      spdftmp,
      timestamps = ts,
      sensor = factor(do.call("c", lapply(lapply(x, slot, "sensor"), as.character)), levels = sensorLevels),
      trackId = factor(rep(rownames(IDDATA), length), levels = rownames(IDDATA)),
      unUsed
    )
    return(res)
  }
)
