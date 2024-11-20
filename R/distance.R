

# setGeneric("distance")#, function(x){standardGeneric("distance")})
setMethod("distance",
  signature = c(x = ".MoveTrackSingle", y = "missing"),
  definition = function(x) {
    Dists <- raster::pointDistance(x[-sum(n.locs(x)), ], x[-1, ], longlat = isLonLat(x))
    return(Dists)
  }
)

setMethod("distance",
  signature = c(".MoveTrackStack", y = "missing"),
  definition = function(x) {
    lst <- lapply(split(x), distance)
    return(lst)
  }
)
