test_that(".Normalize works", {
  expect_equal(.Normalize(0, c(0, 1)), 0)
  expect_equal(.Normalize(1, c(0, 1)), 1)
  expect_equal(.Normalize(1, c(0, 2)), 1/2)
  
  expect_equal(.Normalize(1:3, c(1, 3)), 0:2 / 2)
  expect_equal(.Unnormalize(0:2 / 2, c(1, 3)), 1:3)
})

test_that(".SetRegion() is stable", {
  original <- .SetRegion(ternRegionDefault)
  on.exit(options(ternRegion = original))
  expect_equal(getOption("ternRegion"), ternRegionDefault)
})

test_that(".SetRegion() handles bad input", {
  expect_warning(
    original <- .SetRegion(ternRegionDefault * 2),
    "Largest possible region is"
  )
  on.exit(options(ternRegion = original))
  
  expect_equal(getOption("ternRegion"), ternRegionDefault)
  
  expect_warning(
    .SetRegion(ternRegionDefault * 0),
    "Region must have positive size"
  )
  expect_equal(getOption("ternRegion"), ternRegionDefault)
})

test_that("Region validation works", {
  expect_true(.RegionIsValid(ternRegionDefault))
  expect_true(.RegionIsValid(ternRegion20))
  expect_true(.RegionIsValid(ternRegionA))
  
  expect_true(.RegionIsEquilateral(cbind(c(10, 30), c(0, 20), c(60, 80))))
  expect_false(.RegionIsEquilateral(cbind(c(10, 30), c(0, 20), c(60, 81))))
  
  expect_false(.RegionInRange(cbind(c(-10, 30), c(0, 20), c(60, 81))))
  expect_false(.RegionInRange(cbind(c(10, 100.1), c(0, 20), c(60, 81))))
  
  expect_false(.RegionCorners100(cbind(c(10, 30), c(60, 80), c(0, 20))))
})
