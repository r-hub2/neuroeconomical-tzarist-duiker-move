context("corridor")
test_that("corridor", {
  skip_if_not(requireNamespace("circular"))
  data(leroy)
  expect_s4_class(corridor(leroy), "MoveBurst")
  expect_true(all.equal(sum(corridor(leroy)$pseudoAzimuth, na.rm=T),167005.6068999207, tolerance = 1e-11))
  expect_warning(expect_s4_class(corridor(leroy, speedProp = .99, circProp = .01), "MoveBurst"), "No corridor points found")
})

test_that("cor length", {
  expect_error(corridor(move(1, 1, Sys.time(), proj = "+proj=longlat")), "The data-set has 2 or less fixes")
  expect_error(corridor(move(1:2, 1:2, Sys.time() + 1:2, proj = "+proj=longlat")), "The data-set has 2 or less fixes")
})
test_that("corridor", {
  skip_if_not(requireNamespace("circular"))
  data(leroy)
  expect_s4_class(corridor(leroy), "MoveBurst")
  expect_warning(expect_s4_class(corridor(leroy, speedProp = .99, circProp = .01), "MoveBurst"), "No corridor points found")
})
