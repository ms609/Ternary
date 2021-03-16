test_that("Holdridge coordinate transformation", {
  pet <- c(0.125, 32, 2, 0.125)
  prec <- c(62.5, 62.5, 1000, 16000)
  tern <- cbind(c(1, 0, 0), c(0, 0, 1), c(0, 1, 1), c(0, 1, 0))
  expect_equal(TernaryToXY(tern), HoldridgeToXY(pet, prec))
})

test_that("Holdridge plotting", {
  HoldridgeBasic <- function () {
    HoldridgePlot()
    pet <- holdridge[, 2]
    prec <- holdridge$Precipition
    HoldridgePoints(pet, prec, cex = 2)
    HoldridgeText(pet, prec, seq_along(pet[-12]), cex = 0.6)
  }
  
  skip_if_not_installed('vdiffr')
  vdiffr::expect_doppelganger('holdridge-basic', HoldridgeBasic)
})
