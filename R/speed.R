

setGeneric("speed", function(x) {
  standardGeneric("speed")
})
setMethod("speed",
  signature = ".MoveTrackSingle",
  definition = function(x) {
    # if(length(seglength(x)>0)){
    Speed <- (distance(x)) / timeLag(x, units = "secs") # meter per sec
    return(Speed) # } else {return(NA)}
  }
)

setMethod("speed",
  signature = ".MoveTrackStack",
  definition = function(x) {
    lst <- lapply(split(x), speed)
    return(lst)
  }
)
