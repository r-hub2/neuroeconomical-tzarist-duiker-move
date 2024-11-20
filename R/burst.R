setGeneric("burst", function(x, f, ...) {
  standardGeneric("burst")
})
setMethod("burst",
  signature = c(x = "Move", f = "factor"),
  definition = function(x, f, ...) {
    levels(f) <- validNames(levels(f))
    new("MoveBurst",
      as(x, ".MoveTrackSingle"),
      as(x, ".MoveGeneral"),
      burstId = f,
      idData = x@idData
    )
  }
)

setMethod("burst",
  signature = c(x = "ANY", f = "numeric"),
  definition = function(x, f, ...) {
    burst(x = x, f = as.factor(f))
  }
)

setMethod("burst",
  signature = c(x = "ANY", f = "character"),
  definition = function(x, f, ...) {
    if (length(f) == 1) {
      burst(x = x, f = do.call("$", list(x, f))[-n.locs(x)])
    } else {
      burst(x = x, f = as.factor(f))
    }
  }
)

setMethod("burst",
  signature = c(x = ".MoveTrackSingleBurst", f = "factor"),
  definition = function(x, f, ...) {
    burstId(x) <- f
    return(x)
  }
)



setMethod("burst",
  signature = c(x = ".MoveTrackSingleBurst", "missing"),
  definition = function(x, f, ...) {
    return(slot(x, "burstId"))
  }
)


setAs("MoveBurst", "Move", function(from) {
  new(
    "Move",
    as(from, ".MoveGeneral"),
    as(from, ".MoveTrackSingle")
  )
})
setAs("MoveStack", "Move", function(from) {
  if (length(unique(from@trackId)) != 1) {
    stop("Does only work with one id")
  }
  new("Move",
    as(from, ".MoveGeneral"),
    as(from, ".MoveTrack"),
    timestamps = timestamps(from), as(unUsedRecords(from), ".unUsedRecords")
  )
})
setAs("dBMvarianceBurst", "dBMvariance", function(from) {
  new("dBMvariance", as(from, "dBMvarianceTmp"), as(from, ".MoveTrackSingle"))
})
