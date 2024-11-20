### Redifining spTransform, because it changes the class of the object to SpatialPointsDataFrame
setMethod(
  f = "spTransform",
  signature = c(x = ".MoveTrack", CRSobj = "missing"),
  function(x, center = FALSE, ...) {
    if (!center) {
      stop("spTransform without center or proj string does not make much sense")
    }
    spTransform(x = x, center = center, CRSobj = "+proj=aeqd +datum=WGS84")
  }
)

setMethod(
  f = "spTransform",
  signature = c(x = ".MoveTrack", CRSobj = "character"),
  function(x, CRSobj, center = FALSE, ...) {
    x<-spTransform(x = x, CRSobj = CRS(CRSobj), center = center)
    x
  }
)

setMethod(
  f = "spTransform",
  signature = c(x = ".MoveTrack", CRSobj = "CRS"),
  function(x, CRSobj, center = FALSE, ...) {
    if (center) {
      if (!isLonLat(x)) {
        stop("Center only works with Longitude Latitude projection")
      }
      mid.range.lon <- (max(coordinates(x)[, 1]) + min(coordinates(x)[, 1])) / 2
      mid.range.lat <- (max(coordinates(x)[, 2]) + min(coordinates(x)[, 2])) / 2
      if (grepl(pattern = "+lat_0=0", CRSobj@projargs)) {
        CRSobj <- CRS(sub("lat_0=0", paste0("lat_0=", mid.range.lat), sub("lon_0=0", paste0("lon_0=", mid.range.lon), CRSobj@projargs)))
      } else {
        CRSobj <- CRS(paste(CRSobj@projargs, " +lon_0=", mid.range.lon, " +lat_0=", mid.range.lat, sep = ""))
      }
    }

    coordsnew <- spTransform(x = as(x, "SpatialPointsDataFrame"), CRSobj = CRSobj)

    dimnames(coordsnew@coords)[[2]]<-dimnames(x@coords)[[2]]

    dimnames(coordsnew@bbox)[[2]]<-dimnames(x@bbox)[[2]]
    x <- new(class(x), coordsnew, x)

    return(x)
  }
)
