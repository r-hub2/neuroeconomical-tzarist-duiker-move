setGeneric("burstId", function(x) {
  standardGeneric("burstId")
})
setMethod("burstId",
  signature = ".MoveTrackSingleBurst",
  definition = function(x) {
    return(slot(x, "burstId"))
  }
)
setGeneric("burstId<-", function(x, value) standardGeneric("burstId<-"))
setReplaceMethod(
  "burstId", c(".MoveTrackSingleBurst", "factor"),
  function(x, value) {
    if (n.locs(x) != (length(value) + 1)) {
      stop("A new burst id does not have the same length as the orginal object")
    }
    x@burstId <- value
    x
  }
)
setReplaceMethod(
  "burstId", c(".MoveTrackSingleBurst", "character"),
  function(x, value) {
    value <- as.factor(value)
    callGeneric()
  }
)
