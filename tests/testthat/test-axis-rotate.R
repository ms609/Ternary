test_that("Axis labels are rotatable", {
  AxisRotate <- function() {
    par(mfrow = c(3, 3), xpd = NA, mar = rep(0, 4))
    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.rotate = FALSE,
      point = 1,
      clockwise = TRUE
    )

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.rotate = FALSE,
      point = 2,
      clockwise = TRUE
    )

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.rotate = FALSE,
      point = 3,
      clockwise = TRUE
    )

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.rotate = FALSE,
      point = 4,
      clockwise = TRUE
    )

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.rotate = FALSE,
      point = 1,
      clockwise = FALSE
    )

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.rotate = FALSE,
      point = 2,
      clockwise = FALSE
    )

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.rotate = FALSE,
      point = 3,
      clockwise = FALSE
    )

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.rotate = FALSE,
      point = 4,
      clockwise = FALSE
    )

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.rotate = c(TRUE, FALSE, TRUE)
    )
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("AxisRotate", AxisRotate)
})

test_that("axis.pos", {
  AxisPos <- function() {
    par(mfrow = c(1, 2), xpd = NA, mar = rep(0, 4))

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.pos = 1,
      point = 1,
      clockwise = TRUE
    )

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      axis.pos = c(3, 1, 2),
      point = 2,
      clockwise = TRUE
    )
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("AxisPos", AxisPos)
})
