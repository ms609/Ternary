test_that("Errors are handled gracefully", {
  expect_error(TernaryCoords(c(1, 2)))
  expect_error(TernaryCoords(c("a", "b", "c")))
  expect_error(TernaryPlot(point = "nowhere"))
  expect_error(TernaryPoints(rep(1, 5)))
  expect_error(HorizontalGrid(direction = 5))
  expect_warning(TernaryPlot(xlim = c(0, 10), ylim = c(0, 1), isometric = TRUE))
  expect_warning(TernaryPlot(xlim = c(0, 1), ylim = c(0, -10), isometric = TRUE))
  expect_error(CoordinatesToXY(array(1, dim = c(1, 1, 1))))
})

test_that("TernaryCoords gives correct coordinates", {
  options("ternDirection" = 2L)
  input <- cbind(
    c(a = 0, b = 0, c = 1),
    c(a = 0, b = 2, c = 0),
    c(a = 3, b = 0, c = 0),
    c(a = 0, b = 9, c = 9),
    c(a = 2, b = 1, c = 1)
  )
  listInput <- lapply(1:5, function(i) input[, i])
  output <- cbind(
    c(0, 0.5),
    c(0, -0.5),
    c(sin(pi / 3), 0),
    c(0, 0),
    c(sin(pi / 3) / 2, 0)
  )
  expect_equal(output, apply(input, 2, TernaryCoords))
  expect_equal(output, CoordinatesToXY(listInput))
  expect_equal(output, CoordinatesToXY(input))
  expect_equal(matrix(output[, 1], 2, 1), CoordinatesToXY(input[, 1]))

  expect_equal(input / rep(colSums(input), each = 3), XYToTernary(output[1, ], output[2, ]))
  expect_error(TernaryCoords(rep(1, 5), 1, 1))
  expect_equal(c(0, 0.5), TernaryCoords(0, 0, 1))
})

test_that("Ternary plotting functions", {
  TernaryPlotterXlim <- function() {
    TernaryPlot("A", "B", "C", xlim = c(0, 0.86), point = 2)
    JoinTheDots(list(c(1, 1, 1), c(0, 1, 2)), lwd = 2, col = "green")
    TernaryPoints(c(0.5, 1, 1))
    TernaryText(c(1.5, 1, 1), "A")
  }
  TernaryPlotterYlim <- function() {
    TernaryPlot("A", "B", "C", ylim = c(0, 0.82), point = 1, main = "... to plot")
    TernaryLines(list(c(1, 1, 1), c(0, 1, 2)), lwd = 2)
    TernaryArrows(c(1, 1.2, 1), c(0, 1.2, 2), lwd = 1)
    TernaryPolygon(matrix(c(
      30, 40, 30,
      30, 30, 40,
      55, 20, 25
    ), ncol = 3, byrow = TRUE))
  }

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("plot-to-ternary-x", TernaryPlotterXlim)
  vdiffr::expect_doppelganger("plot-to-ternary-y", TernaryPlotterYlim)
})

test_that("Axes can be styled individually", {
  AxisStyle <- function() {
    par(mfrow = 1:2, xpd = NA)
    cols <- c("red", "darkgreen", "blue")
    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      atip = "a", btip = "b", ctip = "c",
      lab.col = cols,
      lab.cex = 1:3 / 2,
      lab.offset = 1:3 / 15,
      grid.lines = 4,
      axis.lwd = 1:3,
      axis.lty = 1:3,
      ticks.lwd = 1:3,
      grid.col = cols,
      grid.lwd = 1:3,
      grid.lty = 1:3,
      grid.minor.col = c("#ff9999", "#99ff99", "#9999ff"),
      grid.minor.lty = 4:6,
      grid.minor.lwd = 1:3,
      axis.col = cols,
      axis.font = 1:3,
      ticks.col = cols,
      ticks.length = 1:3 / 50,
      tip.font = 1:3,
      tip.col = cols
    )
    TernaryPoints(c(2, 1, 0))

    TernaryPlot(
      alab = "a", blab = "b", clab = "c",
      atip = "a", btip = "b", ctip = "c",
      clockwise = FALSE,
      lab.col = cols,
      lab.cex = 1:3 / 2,
      lab.offset = 1:3 / 15,
      grid.lines = 4,
      axis.lwd = 1:3,
      axis.lty = 1:3,
      ticks.lwd = 1:3,
      grid.col = cols,
      grid.lwd = 1:3,
      grid.lty = 1:3,
      grid.minor.col = c("#ff9999", "#99ff99", "#9999ff"),
      grid.minor.lty = 4:6,
      grid.minor.lwd = 1:3,
      axis.col = cols,
      axis.font = 2,
      ticks.col = cols,
      ticks.length = 1:3 / 50,
      tip.font = 1:3,
      tip.col = cols
    )
    TernaryPoints(c(2, 1, 0))
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("AxisStyle", AxisStyle)
})

test_that("panel.xxst parameter", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("panel.xxst-parameter", function() {
    TernaryPlot(
      panel.first = rect(0, -1, -1, 1, col = 2),
      panel.last = rect(0, -1, 1, 1, col = 3),
      atip = "Tip A", btip = "Tip B", clab = "Axis C",
      xpd = NA
    )
  })
})

test_that("Vignette plots are rendered correctly", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("Blank plot", TernaryPlot)

  BlankTernary <- function(dir, clockwise = TRUE) {
    TernaryPlot(
      point = dir,
      atip = "A vertex", btip = "B vertex", ctip = "C vertex",
      alab = "Aness", blab = "Bness", clab = "Cness",
      clockwise = clockwise
    )
    TernaryText(list(A = c(10, 01, 01), B = c(01, 10, 01), C = c(01, 01, 10)),
      col = cbPalette8[4], font = 2
    )
    HorizontalGrid(grid.col = "red")
  }

  TernaryUp <- function() BlankTernary("up")
  TernaryRight <- function() BlankTernary("right")
  TernaryDown <- function() BlankTernary("down")
  TernaryLe <- function() BlankTernary("le")


  TernaryUpAC <- function() BlankTernary("up", clockwise = FALSE)
  TernaryRightAC <- function() BlankTernary("right", clockwise = FALSE)
  TernaryDownAC <- function() BlankTernary("down", clockwise = FALSE)
  TernaryLeAC <- function() BlankTernary("le", clockwise = FALSE)

  vdiffr::expect_doppelganger("Blank up", TernaryUp)
  vdiffr::expect_doppelganger("Blank right", TernaryRight)
  vdiffr::expect_doppelganger("Blank down", TernaryDown)
  vdiffr::expect_doppelganger("Blank le", TernaryLe)

  vdiffr::expect_doppelganger("Blank up anticlockwise", TernaryUpAC)
  vdiffr::expect_doppelganger("Blank right anticlockwise", TernaryRightAC)
  vdiffr::expect_doppelganger("Blank down anticlockwise", TernaryDownAC)
  vdiffr::expect_doppelganger("Blank le anticlockwise", TernaryLeAC)

  TernaryCols <- function() {
    par(mfrow = c(1, 2), mar = rep(0.3, 4))
    TernaryPlot(
      alab = "Redder \u2192", blab = "\u2190 Greener", clab = "Bluer \u2192",
      lab.col = c("red", "green", "blue"),
      point = "right", lab.cex = 0.8, grid.minor.lines = 0,
      grid.lty = "solid", grid.col = "white",
      axis.col = rgb(0.6, 0.6, 0.6), ticks.col = rgb(0.6, 0.6, 0.6),
      padding = 0.08
    )
    cols <- TernaryPointValues(rgb)
    ColourTernary(cols, spectrum = NULL)
    data_points <- list(
      R = c(255, 0, 0),
      O = c(240, 180, 52),
      Y = c(210, 222, 102),
      G = c(111, 222, 16),
      B = c(25, 160, 243),
      I = c(92, 12, 243),
      V = c(225, 24, 208)
    )
    AddToTernary(points, data_points,
      pch = 21, cex = 2.8,
      bg = vapply(
        data_points,
        function(x) rgb(x[1], x[2], x[3], 128, maxColorValue = 255),
        character(1)
      )
    )
    AddToTernary(text, data_points, names(data_points), cex = 0.8, font = 2)
    legend("bottomright",
      legend = c("Red", "Orange", "Yellow", "Green"),
      cex = 0.8, bty = "n", pch = 21, pt.cex = 1.8,
      pt.bg = c(
        rgb(255, 0, 0, 128, NULL, 255),
        rgb(240, 180, 52, 128, NULL, 255),
        rgb(210, 222, 102, 128, NULL, 255),
        rgb(111, 222, 16, 128, NULL, 255)
      ),
    )

    TernaryPlot("Steam", "Ice", "Water",
      grid.lines = 5L, grid.lty = "dotted",
      grid.minor.lines = 1L, grid.minor.lty = "dotted",
      btip.rotate = 330L, ctip.rotate = 30L,
      col = rgb(0.9, 0.9, 0.9, alpha = 0.2),
      point = "West"
    )
    HorizontalGrid()
    middle_triangle <- matrix(c(
      30, 40, 30,
      30, 30, 40,
      55, 20, 25
    ), ncol = 3, byrow = TRUE)
    TernaryPolygon(middle_triangle, col = "#aaddfa", border = "grey")
    TernaryLines(list(c(0, 100, 0), middle_triangle[1, ]), col = "grey")
    TernaryLines(list(c(0, 0, 100), middle_triangle[2, ]), col = "grey")
    TernaryLines(list(c(100, 0, 0), middle_triangle[3, ]), col = "grey")
    TernaryArrows(c(20, 20, 60), c(30, 30, 40), length = 0.2, col = "darkblue")
    TernarySegments(rbind(c(25, 15, 60), c(15, 25, 60)),
      rbind(c(30, 30, 40), c(30, 30, 40)),
      col = c("lightblue", "#0000eecc")
    )
  }

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("colours-and-water", TernaryCols)

  Cartesian <- function() {
    TernaryPlot(point = "right")
    arrows(x0 = 0.5, y0 = 0.4, x1 = sqrt(3) / 2, y1 = 0.4, length = 0.1, col = cbPalette8[2])
    text(x = mean(c(0.5, sqrt(3) / 2)), y = 0.4, "Increasing X", pos = 3, col = cbPalette8[2])
    text(x = 0.5, y = 0, "(0.5, 0)", col = cbPalette8[3])
    text(x = 0.8, y = -0.5, "(0.8, -0.5)", col = cbPalette8[3])
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("Cartesian", Cartesian)

  Padding <- function() {
    TernaryPlot(xlim = c(0.28, 0.38), ylim = c(0.1, 0.2), padding = 0.04)

    # Annotate grid lines at user-specified points:
    TernaryText(list(c(8, 72, 20), c(8, 82, 10)), c(20, 10), srt = -60, cex = 0.9, col = "darkgrey")
    TernaryText(list(c(10, 69, 21), c(20, 64, 16)), c(10, 20), srt = 0, cex = 0.9, col = "darkgrey")

    # Plot desired polygon
    my_corners <- list(c(22, 66, 12), c(22, 72, 6), c(15, 80, 5), c(12, 76, 12))
    TernaryPolygon(my_corners, col = "#2cbe4e")

    # Show xlim, ylim and padding, using cartesian coordinates
    lines(c(0.28, 0.28, 0.38, 0.38, 0.28), c(0.1, 0.2, 0.2, 0.1, 0.1))
    text(0.28, 0.15, "xlim[1]", pos = 2, srt = 90)
    text(0.38, 0.15, "xlim[2]", pos = 4, srt = 90)
    text(0.33, 0.1, "ylim[1]", pos = 1)
    text(0.33, 0.2, "ylim[2]", pos = 3)
    text(0.38, 0.1, "<padding>", pos = 4, cex = 0.75)
    text(0.38, 0.1, "<padding> ", pos = 2, cex = 0.75, srt = 90)
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("padding", Padding)
})
