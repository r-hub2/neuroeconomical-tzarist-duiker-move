
if (!isGeneric("timeLag", where = topenv(parent.frame()))) {
  setGeneric("timeLag", function(x, ...) standardGeneric("timeLag"))
}
setMethod(
  "timeLag",
  ".MoveTrackSingle",
  function(x, ...) {
    return(as.numeric(diff(timestamps(x)), ...))
  }
)

setMethod("timeLag", ".MoveTrackStack", function(x, units, ...) {
  if (missing(units)) {
    warning("Units not specified this could lead to different units for the time differences between individuals")
    return(lapply(lapply(split(timestamps(x), x@trackId), diff), as.numeric, ...))
  } else {
    return(lapply(lapply(split(timestamps(x), x@trackId), diff), as.numeric, units = units, ...))
  }
})
