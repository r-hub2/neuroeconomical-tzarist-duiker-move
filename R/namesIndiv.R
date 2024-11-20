### extract names of individuals from Move
if (!isGeneric("namesIndiv", where = topenv(parent.frame()))) {
  setGeneric("namesIndiv", function(obj) standardGeneric("namesIndiv"))
}
setMethod("namesIndiv", ".MoveTrackSingle", function(obj) {
  return(rownames(idData(obj, drop = F)))
})

setMethod("namesIndiv", ".MoveTrackStack", function(obj) {
  return(rownames(idData(obj, drop = F)))
})
