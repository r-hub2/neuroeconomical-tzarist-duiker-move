context("move ade")
test_that("moveAde", {
  skip_on_os("mac")
  skip_if_not(requireNamespace("adehabitatLT"))

  data(fishers)
  data <- fishers[["Ricky.T"]]
  dataSp <- spTransform(data, center = T)
  dataLtraj <- as(dataSp, "ltraj")
  dataBack <- as(dataLtraj, "Move")
  expect_equivalent(coordinates(dataSp), coordinates(dataBack))
  expect_equal(timestamps(dataSp), timestamps(dataBack))
  spLtraj <- move2ade(dataSp)
  # suppressMessages(require(adehabitatLT))
  a <- as(adehabitatLT::simm.crw(1:100), "Move")
  expect_true(validObject(a))
  aa <- as(a, "ltraj")
  ma <- move(aa)
  ma$sensor <- NULL
  idData(ma) <- idData(a)
  expect_equal(a, ma)
  expect_true(validObject(a))
  a <- as(adehabitatLT::simm.crw(1:100, id = gl(25, 4)), "MoveStack")
  expect_true(validObject(a))
  aa <- as(a, "ltraj")
  expect_true(validObject(aa))
  ma <- move(aa)
  ma$sensor <- NULL
  idData(ma) <- idData(a)
  ma@trackId <- trackId(a)
  levels(ma@trackIdUnUsedRecords) <- levels(trackId(unUsedRecords(a)))
  expect_equal(a, ma)


  data(leroy)

  ## expect_equal(timestamps(leroy), timestamps(me)) ## embc transforms tz into local

  df <- as.data.frame(leroy)
  mdf <- move(df)
  expect_true(validObject(mdf))
  expect_equivalent(coordinates(leroy), coordinates(mdf))
  expect_equal(timestamps(leroy), timestamps(mdf))
})

test_that("bcpa conversion", {
  skip_on_os("mac")
  skip_if_not(requireNamespace("bcpa"))
  data(leroy)
  tk <- bcpa::MakeTrack(X = coordinates(leroy)[, 1], Y = coordinates(leroy)[, 2], Time = timestamps(leroy))
  mb <- move(tk)
  expect_true(validObject(mb))
  expect_equivalent(coordinates(leroy), coordinates(mb))
  expect_equal(timestamps(leroy), timestamps(mb))
})

test_that("EMBC conversion", {
  skip_on_os("mac")
  skip_if_not(requireNamespace("EMbC"))
  data(leroy)

  bcp <- EMbC::stbc(leroy)
  me <- move(bcp)
  expect_true(validObject(me))
  expect_equivalent(coordinates(leroy), coordinates(me))
})


test_that("ctmm conversion", {
  skip_on_os("mac")
  skip_if_not(requireNamespace("ctmm"))
  data(leroy)

  tlm <- ctmm::as.telemetry(leroy)
  mc <- move(tlm)
  mc <- spTransform(mc, projection(leroy))
  expect_true(validObject(mc))
  expect_equivalent(coordinates(leroy), coordinates(mc))
  expect_equal(timestamps(leroy), timestamps(mc))
})
test_that("amt conversion", {
  data(leroy)
  skip_if_not(requireNamespace("amt"))
  atk <- amt::track(x = coordinates(leroy)[, 1], y = coordinates(leroy)[, 2], t = timestamps(leroy))
  ma <- move(atk)
  expect_true(validObject(ma))
  expect_equivalent(coordinates(leroy), coordinates(ma))
  expect_equal(timestamps(leroy), timestamps(ma))
})
