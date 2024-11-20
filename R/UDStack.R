setGeneric("UDStack", function(x, ...) {
  standardGeneric("UDStack")
})
setMethod("UDStack",
  signature = "RasterStack",
  definition = function(x, method = "unknown", ...) {
    new(".UDStack", x, method = method)
  }
)
setMethod("UDStack",
  signature = "RasterBrick",
  definition = function(x, method, ...) {
    callGeneric(stack(x), ...)
  }
)
setMethod("UDStack",
  signature = "list",
  definition = function(x, method, ...) {
    callGeneric(stack(x), ...)
  }
)
setMethod("UDStack",
  signature = ".UDBurstStack",
  definition = function(x, method = "unknown", ...) {
    callGeneric(x / cellStats(x, sum), ...)
  }
)
