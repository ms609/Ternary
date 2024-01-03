test_that("Not a goof", {
  skip_if(TRUE)
  # Define points corresponding to corners of a region to plot
  my_corners <- list(c(22, 66, 12), c(22, 72, 6), c(15, 80, 5), c(12, 76, 12))
  
  par(mar = rep(0, 4))
  TernaryPlot(region = list(c(10, 60, 0), c(30, 80, 20)))
  
  abc <- c(15, 80, 5)
  expect_equal(sum(abc), 100)
  xy <- TernaryToXY(abc, region = ternRegionDefault)
  range <- getOption("ternRegion")
  rangeXY <- .RegionXY(range)
  points(rangeXY[c(1, 2, 2, 1), 1], rangeXY[c(1, 1, 2, 2), 2], pch = 3)
  corners <- .RegionCorners(range)
  cornerXY <- apply(corners, 2, TernaryToXY, region = ternRegionDefault)
  points(t(cornerXY), pch = 2)
  points(
    .Rebase(cornerXY[1], rangeXY[, 1], TernaryXRange()),
    .Rebase(cornerXY[2], rangeXY[, 2], TernaryYRange()),
    pch = 2, xpd = NA,
    col = "red")
  
  .Rebase(xy[2], TernaryYRange)
  .Normalize(xy[1], TernaryXRange())
  .Normalize(xy[2], TernaryYRange())
  points(TernaryToXY(abc)[1], TernaryToXY(abc)[2], pch = 3)
  
  TernaryPoints(abc)
  TernaryText(abc, paste(abc, collapse = ", "), pos = 3, cex = 0.8)
  TernaryPolygon(my_corners, col = "#2cbe4e88")
  TernaryText(my_corners, sapply(my_corners, paste, collapse = ", "),
              pos = c(2, 3, 4, 2), cex = 0.8)
})


test_that(".Normalize works", {
  expect_equal(.Normalize(0, c(0, 1)), 0)
  expect_equal(.Normalize(1, c(0, 1)), 1)
  expect_equal(.Normalize(1, c(0, 2)), 1/2)
  
  expect_equal(.Normalize(1:3, c(1, 3)), 0:2 / 2)
  expect_equal(.Unnormalize(0:2 / 2, c(1, 3)), 1:3)
})

test_that("SetRegion() is stable", {
  original <- SetRegion(ternRegionDefault)
  on.exit(options(ternRegion = original))
  expect_equal(getOption("ternRegion"), ternRegionDefault)
})

test_that("SetRegion() handles bad input", {
  expect_warning(
    original <- SetRegion(ternRegionDefault * 2),
    "Largest possible region is"
  )
  on.exit(options(ternRegion = original))
  
  expect_equal(getOption("ternRegion"), ternRegionDefault)
  
  expect_warning(
    SetRegion(ternRegionDefault * 0),
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
