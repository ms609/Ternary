test_that("Densities are correctly calculated", {
  coordinates <- list(
    middle = c(1, 1, 1),
    top = c(1, 0, 0),
    belowTop = c(2, 1, 1),
    leftSideSolid = c(9, 2, 9),
    leftSideSolid2 = c(9, 2, 9) / 2,
    right3way = c(1, 2, 0),
    rightEdge = c(2.5, 0.5, 0),
    leftBorder = c(1, 1, 4),
    topBorder = c(2, 1, 3),
    rightBorder = c(1, 2, 3)
  )

  values <- TernaryDensity(coordinates, resolution = 3L, direction = 1L)
  expect_equal(
    c(3, 10, 4, 3, 2, 16, 7, 3, 12),
    values["z", ]
  )
})

test_that("Contours are plotted", {
  Contours <- function() {
    par(mar = rep(0, 4), mfrow = c(2, 2))

    FunctionToContour <- function(a, b, c, ...) {
      a - c + (4 * a * b) + (27 * a * b * c)
    }

    TernaryPlot(alab = "a", blab = "b", clab = "c", point = 1L)
    ColourTernary(TernaryPointValues(FunctionToContour, resolution = 6L))
    TernaryContour(FunctionToContour, resolution = 12L, legend = 3, bty = "n")

    TernaryPlot(alab = "a", blab = "b", clab = "c", point = 2L)
    ColourTernary(TernaryPointValues(FunctionToContour, resolution = 6L))
    TernaryContour(FunctionToContour, resolution = 12L, legend = TRUE)

    TernaryPlot(alab = "a", blab = "b", clab = "c", point = 3L,
                region = ternRegion20)
    ColourTernary(TernaryPointValues(FunctionToContour, resolution = 6L),
                  legend = TRUE, x = "bottomleft", bty = "n")
    TernaryContour(FunctionToContour, resolution = 12L)

    TernaryPlot(alab = "a", blab = "b", clab = "c", point = 4L,
                region = ternRegionA)
    ColourTernary(TernaryPointValues(FunctionToContour, resolution = 6L))
    val <- TernaryContour(FunctionToContour, resolution = 12L,
                          legend = letters[1:5],
                          legend... = list(bty = "n", x = "bottomleft"))
    expect_equal(val$x, seq(-sqrt(0.75), 0, length.out = 12L))
    expect_equal(val$y, seq(-0.5, 0.5, length.out = 12L))
    abc <- XYToTernary(val$x[4], val$y[7])
    expect_equal(val$z[4, 7], FunctionToContour(abc[1], abc[2], abc[3]))
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("Contours", Contours)
  
  FilledContours <- function() {
    par(mar = rep(0, 4), mfrow = c(2, 2))

    FunctionToContour <- function(a, b, c) {
      a - c + (4 * a * b) + (27 * a * b * c)
    }

    TernaryPlot(alab = "a", blab = "b", clab = "c", point = 1L)
    TernaryContour(FunctionToContour, filled = TRUE)

    TernaryPlot(alab = "a", blab = "b", clab = "c", point = 2L)
    TernaryContour(FunctionToContour, filled = TRUE,
                   color.palette = function(n) 
                     hcl.colors(n, alpha = 0.6, rev = TRUE))

    TernaryPlot(alab = "a", blab = "b", clab = "c", point = 3L,
                region = ternRegion20)
    TernaryContour(FunctionToContour, filled = TRUE, nlevels = 9,
                   fill.col = 0:8)

    TernaryPlot(alab = "a", blab = "b", clab = "c", point = 4L,
                region = ternRegionA)
    TernaryContour(FunctionToContour, filled = TRUE, nlevels = 4)
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("FilledContours", FilledContours)

  ContoursSkiwiff <- function() {
    FunctionToContour <- function(a, b, c) {
      a - c + (4 * a * b) + (27 * a * b * c)
    }
    SubTest <- function(direction) {
      ColourTernary(TernaryPointValues(FunctionToContour,
        resolution = 6L,
        direction = direction
      ))
      TernaryContour(FunctionToContour,
                     resolution = 12L,
                     direction = direction,
                     within = -t(TernaryToXY(diag(3))))
    }

    par(mar = rep(0, 4), mfrow = c(2, 2))
    TernaryPlot(point = 3L, ylim = c(0, 1))
    SubTest(1)

    TernaryPlot(point = 4L, xlim = c(0, 1))
    SubTest(2)

    TernaryPlot(point = 1L, ylim = c(-1, 0))
    SubTest(3)

    TernaryPlot(point = 2L, xlim = c(-1, 0))
    SubTest(4)
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("Contours-skiwiff", ContoursSkiwiff)


  DensityContours <- function() {
    par(mar = rep(0.2, 4), mfrow = c(1, 2))
    TernaryPlot()

    nPoints <- 400L
    set.seed(0)
    coordinates <- cbind(
      abs(rnorm(nPoints, 2, 3)),
      abs(rnorm(nPoints, 1, 1.5)),
      abs(rnorm(nPoints, 1, 0.5))
    )

    ColourTernary(TernaryDensity(coordinates, resolution = 10L),
                  legend = 4:1, x = "topleft", bty = "n")
    TernaryPoints(coordinates, col = "red", pch = ".")
    val <- TernaryDensityContour(coordinates, resolution = 10L)
    expect_equal(names(val), letters[24:26])
    expect_equal(val$x, seq.int(-0.5, 0.5, length.out = 10))
    expect_equal(val$y, seq.int(0, sqrt(0.75), length.out = 10))
    expect_equal(val$z[10, 10], NA_real_)
    
    TernaryPlot()
    TernaryDensityContour(coordinates, resolution = 10L, filled = TRUE)
    TernaryPoints(coordinates, col = "red", pch = ".")
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("density-contours", DensityContours)


  DensityContours2 <- function() {
    par(mar = rep(0.2, 4))
    TernaryPlot(point = 2)

    nPoints <- 400L
    set.seed(0)
    coordinates <- cbind(
      abs(rnorm(nPoints, 2, 3)),
      abs(rnorm(nPoints, 1, 1.5)),
      abs(rnorm(nPoints, 1, 0.5))
    )

    TernaryPoints(coordinates, col = "red", pch = ".")
    TernaryDensityContour(coordinates, resolution = 10L, edgeCorrection = FALSE)
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("density-contours-2", DensityContours2)



  DensityContours3 <- function() {
    par(mar = rep(0.2, 4))
    TernaryPlot(point = 3)

    nPoints <- 400L
    set.seed(0)
    coordinates <- cbind(
      abs(rnorm(nPoints, 2, 3)),
      abs(rnorm(nPoints, 1, 1.5)),
      abs(rnorm(nPoints, 1, 0.5))
    )

    TernaryPoints(coordinates, col = "red", pch = ".")
    TernaryDensityContour(coordinates, resolution = 10L)
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("density-contours-3", DensityContours3)

  LoResDensCont <- function() {
    coordinates <- list(
      middle = c(1, 1, 1),
      top = c(3, 0, 0),
      belowTop = c(2, 1, 1),
      leftSideSolid = c(9, 2, 9),
      leftSideSolid2 = c(9.5, 2, 8.5),
      right3way = c(1, 2, 0),
      rightEdge = c(2.5, 0.5, 0),
      leftBorder = c(1, 1, 4),
      topBorder = c(2, 1, 3),
      rightBorder = c(1, 2, 3)
    )
    par(mfrow = c(2, 2), mar = rep(0.2, 4))
    TernaryPlot(grid.lines = 3, axis.labels = 1:3, point = "up")
    values <- TernaryDensity(coordinates, resolution = 3L)
    ColourTernary(values)
    TernaryPoints(coordinates, col = "red")
    text(values[1, ], values[2, ], paste(values[3, ], "/ 6"), cex = 0.8)

    TernaryPlot(grid.lines = 3, axis.labels = 1:3, point = "right")
    values <- TernaryDensity(coordinates, resolution = 3L)
    ColourTernary(values)
    TernaryPoints(coordinates, col = "red")
    text(values[1, ], values[2, ], paste(values[3, ], "/ 6"), cex = 0.8)

    TernaryPlot(grid.lines = 3, axis.labels = 1:3, point = "down")
    values <- TernaryDensity(coordinates, resolution = 3L)
    ColourTernary(values)
    TernaryPoints(coordinates, col = "red")
    text(values[1, ], values[2, ], paste(values[3, ], "/ 6"), cex = 0.8)

    TernaryPlot(grid.lines = 3, axis.labels = 1:3, point = "left")
    values <- TernaryDensity(coordinates, resolution = 3L)
    ColourTernary(values)
    TernaryPoints(coordinates, col = "red")
    text(values[1, ], values[2, ], paste(values[3, ], "/ 6"), cex = 0.8)

    TernaryDensityContour(t(vapply(coordinates, I, double(3L))),
      resolution = 12L, tolerance = -0.02, col = "orange"
    )
  }
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("lo-res-density-contours", LoResDensCont)
})

test_that("Colours are drawn", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("RGBColours", function() {
    TernaryPlot()
    values <- TernaryPointValues(rgb, resolution = 20, alpha = 0.5)
    ColourTernary(values, spectrum = NULL)
  })
})

test_that("Errors are handled", {
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("contour-error-handling", function() {
    TernaryPlot()
    # Non-vectorized Func
    expect_warning(expect_warning(TernaryContour(max)))
    expect_warning(TernaryPointValues(max))

    # Positive bandwidths
    expect_error(TernaryDensityContour(rbind(c(1, 1, 1)), -1))
    expect_error(ColourTernary(TernaryPointValues(as.character, 5)))
  })
})

test_that("TriangleInHull()", {
  expect_error(
    TriangleInHull(coord = 1:5),
    "`coordinates` must be a matrix with two \\(xy\\) or three \\(abc\\) rows"
  )
  # From example
  set.seed(0)
  nPts <- 50
  a <- runif(nPts, 0.3, 0.7)
  b <- 0.15 + runif(nPts, 0, 0.7 - a)
  c <- 1 - a - b
  coordinates <- rbind(a, b, c)
  triangles <- TriangleCentres(resolution = 5)

  # Coordinate transform resilience
  fromABC <- TriangleInHull(triangles, coordinates)
  fromXY <- TriangleInHull(triangles, TernaryToXY(coordinates))
  expect_equal(fromABC, fromXY)
})
