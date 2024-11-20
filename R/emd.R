setGeneric("emd", function(x, y, ...) {
  standardGeneric("emd")
})
setMethod("emd",
  signature = c(x = "RasterLayer", y = "RasterLayer"),
  function(x, y, ...) {
    # if (class(x) == "RasterLayer")
    x <- (rasterToPoints(x, spatial = T))
    #  if (class(y) == "RasterLayer")
    y <- (rasterToPoints(y, spatial = T))
    emd(x, y, ...)
  }
)


setMethod("emd",
  signature = c(x = "SpatialPoints", y = "SpatialPoints"),
  function(x,
           y,
           gc = FALSE,
           threshold = NULL,
           ...) {
    if (inherits(x, "SpatialPointsDataFrame")) {
      xv <- data.frame(x)[, names(x)[1]]
    } else {
      xv <- rep(1, length(x)) / length(x)
    }
    if (inherits(y, "SpatialPointsDataFrame")) {
      yv <- data.frame(y)[, names(y)[1]]
    } else {
      yv <- rep(1, length(y)) / length(y)
    }
    if (!isTRUE(all.equal(sum(xv), sum(yv)))) {
      warning(paste("datasets dont have equal mass, delta:", sum(xv) -
        sum(yv)))
    }
    if (!isTRUE(all.equal(sum(xv), 1))) {
      warning("data does not represent probability surface")
    }
    res <- 1
    fun <- "emdR"
    if (!is.null(threshold)) {
      if (!isTRUE(all.equal(coordinates(y), coordinates(x),
        check.attributes =
          F
      ))) {
        stop("spatial data locations are unequal between datasets, this does not work with the fast emd")
      }
      fun <- "emdR_gd"
      s <- !(xv == 0 & yv == 0)
      xv <- xv[s]
      yv <- yv[s]
      x <- x[s, ]
      y <- y[s, ]
    } else {
      x <- x[xv != 0, ]
      y <- y[yv != 0, ]
      xv <- xv[xv != 0]
      yv <- yv[yv != 0]
    }
    a <- .C(
      fun,
      Pn = as.integer(length(x)),
      Qn = as.integer(length(y)),
      Px = as.double(coordinates(x)[, 1]),
      Py = as.double(coordinates(x)[, 2]),
      Pw = as.double(xv),
      Qx = as.double(coordinates(y)[, 1]),
      Qy = as.double(coordinates(y)[, 2]),
      Qw = as.double(yv),
      res = as.double(res),
      th = ifelse(is.null(threshold), 1, as.double(threshold)),
      gc = as.integer(gc)
    )
    return(as.dist(matrix(c(0, a$res, a$res, 0), ncol = 2)))
  }
)


setMethod("emd",
  signature = c(x = "RasterStackBrick", y = "missing"),
  function(x, y, ...) {
    comb <- combn(n <- names(x), 2)
    m <- matrix(0, length(n), length(n), dimnames = list(n, n))
    xx <- lapply(unstack(x), rasterToPoints, spatial = T)
    names(xx) <- names(x)
    for (i in 1:ncol(comb))
    {
      r <- emd(xx[[comb[1, i]]], xx[[comb[2, i]]], ...)
      m[comb[1, i], comb[2, i]] <- r
      m[comb[2, i], comb[1, i]] <- r
    }
    return(as.dist(m))
  }
)

setMethod("emd",
  signature = c(x = "RasterStackBrick", y = "RasterStackBrick"),
  function(x, y, ...) {
    comb <- expand.grid(nx <- names(x), ny <- names(y))
    m <-
      matrix(0, length(nx), length(ny), dimnames = list(nx, ny))
    xx <- lapply(unstack(x), rasterToPoints, spatial = T)
    names(xx) <- names(x)
    yy <- lapply(unstack(y), rasterToPoints, spatial = T)
    names(yy) <- names(y)
    for (i in 1:nrow(comb))
    {
      r <- emd(xx[[comb[i, 1]]], yy[[comb[i, 2]]], ...)
      m[comb[i, 1], comb[i, 2]] <- r
    }
    return((m))
  }
)


# Todo
# check for memory leaking /optimizations
