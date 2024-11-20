

setGeneric("angle", function(x) {
  standardGeneric("angle")
})
setMethod("angle",
  signature = ".MoveTrackSingle",
  definition = function(x) {
    if (nrow(coordinates(x)) >= 3) {
      if (!isLonLat(x)) x <- spTransform(x, CRSobj = "+proj=longlat")
      tAzimuth <- geosphere::bearing(coordinates(x)[-n.locs(x), ], coordinates(x)[-1, ])
      return(tAzimuth)
    } else {
      NA
    }
  }
)

setMethod("angle",
  signature = ".MoveTrackStack",
  definition = function(x) {
    lst <- lapply(split(x), angle)
    return(lst)
  }
)
