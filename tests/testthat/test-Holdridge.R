test_that("Holdridge coordinate transformation", {
  pet <- c(0.125, 32, 2, 0.125)
  prec <- c(62.5, 62.5, 1000, 16000)
  tern <- cbind(c(1, 0, 0), c(0, 0, 1), c(0, 1, 1), c(0, 1, 0))
  expect_equal(TernaryToXY(tern), HoldridgeToXY(pet, prec))
})