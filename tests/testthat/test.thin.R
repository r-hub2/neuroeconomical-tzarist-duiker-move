context("thin")
test_that("thin", {
  d <- 1
  t <- .2
  set.seed(234)
  x <- cumsum(c(rgamma(100, 10, 10), 1, .1, 1, .1, 1.03, .8, 12, 1.2, 234, .2, .8, .33, 5, .8, 5, 1.2, 77, 1, .1, 1, .1, 33))
  selType <- "first"
  expect_equal(move:::filterThinFun(x, d, t, selType), move:::filterThinFun2(x, d, t, selType))
  selType <- "closest"
  expect_equal(move:::filterThinFun(x, d, t, selType), move:::filterThinFun2(x, d, t, selType))
  selType <- "all"
  expect_equal(move:::filterThinFun(x, d, t, selType), move:::filterThinFun2(x, d, t, selType))
  x <- cumsum(rep(3, 5))
  expect_equal(move:::filterThinFun(x, d, t, selType), move:::filterThinFun2(x, d, t, selType))
})
