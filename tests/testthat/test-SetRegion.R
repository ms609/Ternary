test_that(".Normalize works", {
  expect_equal(.Normalize(0, 0, 1), 0)
  expect_equal(.Normalize(1, 0, 1), 1)
  expect_equal(.Normalize(1, 0, 2), 1/2)
  
  expect_equal(.Normalize(1:3, 1, 3), 0:2 / 2)
  expect_equal(.Unnormalize(0:2 / 2, 1, 3), 1:3)
  
  expect_equal(
    # TODO DELETE
    .NormalizeToRegionABC(1:3, region = list(c(0, 4), c(1, 5), c(0, 4))),
    c(.Normalize(1, 0, 4), 1/4, 3/4)
  )
  
  expect_equal(
    .UnnormalizeFromRegion(1:3 / 4, region = list(c(0, 4), c(1, 5), c(0, 4))),
    c(1, 3, 3)
  )
  
})

test_that("SetRegion() is stable", {
  expect_equal(SetRegion(ternRegionDefault), ternRegionDefault)
})

test_that("Region validation works", {
  expect_true(.RegionIsEquilateral(cbind(c(10, 30), c(0, 20), c(60, 80))))
  expect_false(.RegionIsEquilateral(cbind(c(10, 30), c(0, 20), c(60, 81))))
  
  expect_true(.RegionInRange(ternRegionDefault))
  expect_false(.RegionInRange(cbind(c(-10, 30), c(0, 20), c(60, 81))))
  expect_false(.RegionInRange(cbind(c(10, 100.1), c(0, 20), c(60, 81))))
})