setGeneric("summary")
setMethod(
  f = "summary",
  signature = c(object = ".UD"),
  definition = function(object) {
    return(list(Raster_proj = object@crs@projargs, Raster_ext = object@extent, Raster_max_val = maxValue(object), Raster_min_val = minValue(object)))
  }
)

setMethod("summary",
  signature = ".UDStack",
  definition = function(object) {
    lst <- lapply(split(object), summary)
    return(lst)
  }
)
