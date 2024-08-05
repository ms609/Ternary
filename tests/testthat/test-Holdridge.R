test_that("Holdridge coordinate transformation", {
  pet <- c(0.125, 32, 2, 0.125)
  prec <- c(62.5, 62.5, 1000, 16000)
  tern <- cbind(c(1, 0, 0), c(0, 0, 1), c(0, 1, 1), c(0, 1, 0))
  expect_equal(TernaryToXY(tern), HoldridgeToXY(pet, prec))
})

test_that("Holdridge plotting", {
  HoldridgeBasic <- function() {
    pet <- holdridge[, 2]
    prec <- holdridge$Precipitation
    lat <- holdridge$Latitude
    latCol <- hcl.colors(90, palette = "plasma", alpha = 0.6)[ceiling(abs(lat))]
    oPar <- par(mar = rep(0, 4))
    on.exit(par(oPar), TRUE)

    HoldridgePlot(hex.labels = holdridgeLifeZonesUp, hex.cex = 0.5)
    HoldridgeBelts()
    HoldridgePoints(pet, prec, cex = 2.2, pch = 21, bg = latCol)
    HoldridgeText(pet, prec, seq_along(pet)[-12],
      cex = 0.6,
      col = "white", font = 2
    )
  }

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("holdridge-basic", HoldridgeBasic)
})
