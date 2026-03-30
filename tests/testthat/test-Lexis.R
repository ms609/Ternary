test_that("LexisToXY / XYToLexis round-trip (default range, a free)", {
  b <- c(0, 0.25, 0.5, 1)
  c_vals <- c(0, 0.75, 0.1, 1)
  xy <- LexisToXY(b = b, c = c_vals, bRange = c(0, 1), cRange = c(0, 1))
  bc <- XYToLexis(xy[1, ], xy[2, ], bRange = c(0, 1), cRange = c(0, 1))
  expect_equal(as.numeric(bc["b", ]), b)
  expect_equal(as.numeric(bc["c", ]), c_vals)
})

test_that("LexisToXY / XYToLexis round-trip (custom range, a free)", {
  bR <- c(10, 50)
  cR <- c(100, 300)
  b <- c(10, 25, 50)
  c_vals <- c(100, 200, 300)
  xy <- LexisToXY(b = b, c = c_vals, bRange = bR, cRange = cR)
  bc <- XYToLexis(xy[1, ], xy[2, ], bRange = bR, cRange = cR)
  expect_equal(as.numeric(bc["b", ]), b)
  expect_equal(as.numeric(bc["c", ]), c_vals)
})

test_that("LexisToXY / XYToLexis round-trip (c free)", {
  aR <- c(0, 100)
  bR <- c(1900, 2000)
  a_vals <- c(0, 50, 100)
  b_vals <- c(1900, 1950, 2000)
  xy <- LexisToXY(a = a_vals, b = b_vals, aRange = aR, bRange = bR)
  ab <- XYToLexis(xy[1, ], xy[2, ], aRange = aR, bRange = bR)
  expect_equal(as.numeric(ab["a", ]), a_vals)
  expect_equal(as.numeric(ab["b", ]), b_vals)
})

test_that("LexisToXY / XYToLexis round-trip (b free)", {
  aR <- c(0, 1)
  cR <- c(0, 1)
  a_vals <- c(0.2, 0.5, 0.8)
  c_vals <- c(0.1, 0.6, 0.9)
  xy <- LexisToXY(a = a_vals, c = c_vals, aRange = aR, cRange = cR)
  ac <- XYToLexis(xy[1, ], xy[2, ], aRange = aR, cRange = cR)
  expect_equal(as.numeric(ac["a", ]), a_vals)
  expect_equal(as.numeric(ac["c", ]), c_vals)
})

test_that("LexisToXY / XYToLexis round-trip (scalar)", {
  xy <- LexisToXY(b = 0.3, c = 0.7, bRange = c(0, 1), cRange = c(0, 1))
  bc <- XYToLexis(xy[1], xy[2], bRange = c(0, 1), cRange = c(0, 1))
  expect_equal(as.numeric(bc["b", ]), 0.3)
  expect_equal(as.numeric(bc["c", ]), 0.7)
})

test_that("TernaryPoints lands at same spot as LexisPoints (a free)", {
  b0 <- 0.6; c0 <- 0.4
  bR <- c(0, 1); cR <- c(0, 1)
  lexis_xy <- LexisToXY(b = b0, c = c0, bRange = bR, cRange = cR)

  k <- diff(bR) + diff(cR)
  a <- k - b0 - c0
  ternary_xy <- TernaryCoords(a, b0, c0, direction = 1L,
                               region = ternRegionDefault)
  expect_equal(as.numeric(lexis_xy), as.numeric(ternary_xy))
})

test_that("LexisPlot returns invisibly with correct class (default)", {
  lex <- LexisPlot()
  expect_s3_class(lex, "LexisPlot")
  expect_equal(lex$freeAxis, "a")
  expect_equal(lex$boundedAxes, c("b", "c"))
  expect_equal(lex$range1, c(0, 1))
  expect_equal(lex$range2, c(0, 1))
})

test_that("LexisPlot respects asymmetric ranges (a free)", {
  lex <- LexisPlot(bRange = c(0, 200), cRange = c(0, 100))
  expect_equal(lex$range1, c(0, 200))
  expect_equal(lex$range2, c(0, 100))
  expect_equal(lex$freeAxis, "a")
})

test_that("LexisPlot with c free (APC layout)", {
  lex <- LexisPlot(aRange = c(0, 100), bRange = c(1900, 2000))
  expect_equal(lex$freeAxis, "c")
  expect_equal(lex$boundedAxes, c("a", "b"))
  expect_equal(lex$range1, c(0, 100))
  expect_equal(lex$range2, c(1900, 2000))
})

test_that("LexisPlot with b free", {
  lex <- LexisPlot(aRange = c(0, 1), cRange = c(0, 1))
  expect_equal(lex$freeAxis, "b")
  expect_equal(lex$boundedAxes, c("a", "c"))
})

test_that("LexisPlot errors on wrong number of ranges", {
  expect_error(LexisPlot(aRange = c(0, 1)),
               "Specify exactly two")
  expect_error(LexisPlot(aRange = c(0, 1), bRange = c(0, 1),
                          cRange = c(0, 1)),
               "Specify exactly two")
})

test_that(".ResolveLexisAxes returns correct axisMap", {
  res <- Ternary:::.ResolveLexisAxes(NULL, c(0, 1), c(0, 1))
  expect_equal(res$axisMap[["axis1"]], 2L) # b
  expect_equal(res$axisMap[["axis2"]], 3L) # c
  expect_equal(res$axisMap[["free"]], 1L)  # a

  res <- Ternary:::.ResolveLexisAxes(c(0, 1), c(0, 1), NULL)
  expect_equal(res$axisMap[["axis1"]], 1L) # a
  expect_equal(res$axisMap[["axis2"]], 2L) # b
  expect_equal(res$axisMap[["free"]], 3L)  # c
})

test_that(".LexisDiagEndpoints clips correctly", {
  span1 <- 2; span2 <- 1

  # d = 0: only corner (0, 0)
  pts <- Ternary:::.LexisDiagEndpoints(0, span1, span2)
  expect_equal(nrow(pts), 1)

  # d = 1: crosses bottom (1, 0) and left (0, 1)
  pts <- Ternary:::.LexisDiagEndpoints(1, span1, span2)
  expect_equal(nrow(pts), 2)

  # d = 2: crosses bottom (2, 0) and top (1, 1)
  pts <- Ternary:::.LexisDiagEndpoints(2, span1, span2)
  expect_equal(nrow(pts), 2)

  # d = 3: only corner (2, 1)
  pts <- Ternary:::.LexisDiagEndpoints(3, span1, span2)
  expect_equal(nrow(pts), 1)
})

test_that("LexisPlot default vdiffr snapshot", {
  expect_doppelganger("lexis-default", function() LexisPlot())
})

test_that("LexisPlot asymmetric vdiffr snapshot", {
  expect_doppelganger("lexis-asymmetric", function() {
    LexisPlot(bRange = c(0, 2), cRange = c(0, 1),
              blab = "Age", clab = "Period",
              grid.lines = 5, grid.minor.lines = 0,
              col = "white")
  })
})

test_that("LexisPlot rhombus with labels vdiffr snapshot", {
  expect_doppelganger("lexis-labelled", function() {
    LexisPlot(blab = "b", clab = "c", alab = "a",
              atip = "A", btip = "B", ctip = "C",
              col = "white", grid.lines = 5, grid.minor.lines = 1)
  })
})

test_that("LexisPlot c-free APC vdiffr snapshot", {
  expect_doppelganger("lexis-apc", function() {
    LexisPlot(aRange = c(0, 100), bRange = c(1900, 2000),
              alab = "Age", blab = "Period", clab = "Cohort",
              grid.lines = 5, grid.minor.lines = 0, col = "white")
  })
})

test_that("LexisPlot free.fun produces Cohort labels", {
  expect_doppelganger("lexis-apc-cohort", function() {
    LexisPlot(aRange = c(0, 100), bRange = c(1900, 2000),
              alab = "Age", blab = "Period", clab = "Cohort",
              free.fun = function(a, b) b - a,
              grid.lines = 5, grid.minor.lines = 0, col = "white")
  })
})


# ---- Tile and contour tests --------------------------------------------------

test_that(".LexisTriangleCentres produces correct tile count (a free)", {
  # bSpan = 2, cSpan = 1, resolution = 4
  # spacing = max(2, 1) / 4 = 0.5;  nR1 = 2/0.5 = 4;  nR2 = 1/0.5 = 2
  # Total tiles = 2 * 4 * 2 = 16
  lex <- LexisPlot(bRange = c(0, 2), cRange = c(0, 1))
  centres <- Ternary:::.LexisTriangleCentres(resolution = 4, lex)
  expect_equal(ncol(centres), 16)
  expect_equal(sum(centres["triDown", ] == 0), 8)
  expect_equal(sum(centres["triDown", ] == 1), 8)
})

test_that(".LexisTriangleCentres symmetric case", {
  lex <- LexisPlot()
  centres <- Ternary:::.LexisTriangleCentres(resolution = 5, lex)
  # spacing = 1/5 = 0.2;  nR1 = 5;  nR2 = 5;  total = 50
  expect_equal(ncol(centres), 50)
  expect_true(!is.null(attr(centres, "effectiveResolution")))
})

test_that(".LexisTriangleCentres works with c-free plot", {
  lex <- LexisPlot(aRange = c(0, 100), bRange = c(1900, 2000))
  centres <- Ternary:::.LexisTriangleCentres(resolution = 5, lex)
  expect_equal(ncol(centres), 50)
})

test_that("LexisPointValues evaluates Func correctly (a free)", {
  LexisPlot(bRange = c(0, 1), cRange = c(0, 1))
  vals <- LexisPointValues(function(b, c) b + c, resolution = 6)

  expect_equal(nrow(vals), 4)
  expect_true(all(c("x", "y", "z", "down") %in% rownames(vals)))
  expect_true(!is.null(attr(vals, "effectiveResolution")))

  bc <- XYToLexis(vals["x", ], vals["y", ],
                   bRange = c(0, 1), cRange = c(0, 1))
  expected_z <- bc["b", ] + bc["c", ]
  expect_equal(as.numeric(vals["z", ]), as.numeric(expected_z),
               tolerance = 1e-10)
})

test_that("LexisPointValues evaluates Func correctly (c free)", {
  LexisPlot(aRange = c(0, 100), bRange = c(0, 100))
  vals <- LexisPointValues(function(a, b) a + b, resolution = 6)

  expect_equal(nrow(vals), 4)

  ab <- XYToLexis(vals["x", ], vals["y", ],
                   aRange = c(0, 100), bRange = c(0, 100))
  expected_z <- ab["a", ] + ab["b", ]
  expect_equal(as.numeric(vals["z", ]), as.numeric(expected_z),
               tolerance = 1e-10)
})

test_that("LexisPointValues centres approximately cover bRange/cRange", {
  bR <- c(10, 50)
  cR <- c(100, 300)
  LexisPlot(bRange = bR, cRange = cR)
  vals <- LexisPointValues(function(b, c) b, resolution = 8)

  bc <- XYToLexis(vals["x", ], vals["y", ], bRange = bR, cRange = cR)
  spacing <- max(diff(bR), diff(cR)) / 8

  expect_true(all(bc["b", ] >= bR[1] - spacing & bc["b", ] <= bR[2] + spacing))
  expect_true(all(bc["c", ] >= cR[1] - spacing & bc["c", ] <= cR[2] + spacing))

  # At least some centres near each boundary
  expect_true(any(bc["b", ] < bR[1] + spacing))
  expect_true(any(bc["b", ] > bR[2] - spacing))
})

test_that("ColourLexis vdiffr snapshot", {
  expect_doppelganger("lexis-colour", function() {
    par(mar = rep(0.2, 4))
    LexisPlot(blab = "b", clab = "c", grid.lines = 5,
              grid.minor.lines = 0)
    vals <- LexisPointValues(function(b, c) b * c, resolution = 12)
    ColourLexis(vals)
  })
})

test_that("LexisContour vdiffr snapshot", {
  expect_doppelganger("lexis-contour", function() {
    par(mar = rep(0.2, 4))
    LexisPlot(blab = "b", clab = "c", grid.lines = 5,
              grid.minor.lines = 0)
    LexisContour(function(b, c) b * c, resolution = 36)
  })
})

test_that("ColorLexis is an alias for ColourLexis", {
  expect_identical(ColorLexis, ColourLexis)
})

test_that("AddToLexis errors on missing bounded axes", {
  LexisPlot(aRange = c(0, 1), bRange = c(0, 1))
  # c-free plot needs a and b, not b and c
  expect_error(AddToLexis(points, b = 0.5, c = 0.5),
               "Provide the two bounded axes")
})

test_that("LexisToXY reads ranges from plot state", {
  LexisPlot(aRange = c(0, 100), bRange = c(1900, 2000))
  xy <- LexisToXY(a = 50, b = 1950)
  ab <- XYToLexis(xy[1], xy[2])
  expect_equal(as.numeric(ab["a", ]), 50)
  expect_equal(as.numeric(ab["b", ]), 1950)
})

test_that("Edge normals point outward for both winding orders", {
  # a-free (clockwise winding)
  lex1 <- LexisPlot()
  midX <- mean(lex1$corners[1, ])
  midY <- mean(lex1$corners[2, ])
  # Edge 1 normal should point away from the centre
  edge1mid <- (lex1$corners[, 1] + lex1$corners[, 2]) / 2
  toNormal <- edge1mid + lex1$edgeNormals[, 1] * 0.01
  toCentre <- c(midX, midY)
  expect_gt(sum((toNormal - edge1mid)^2),
            0) # Normal has nonzero length
  # Normal moves away from centre (dot product with outward direction > 0)
  outward <- edge1mid - toCentre
  expect_gt(sum(lex1$edgeNormals[, 1] * outward), 0)

 # c-free (counterclockwise winding)
  lex2 <- LexisPlot(aRange = c(0, 100), bRange = c(1900, 2000))
  midX <- mean(lex2$corners[1, ])
  midY <- mean(lex2$corners[2, ])
  edge1mid <- (lex2$corners[, 1] + lex2$corners[, 2]) / 2
  outward <- edge1mid - c(midX, midY)
  expect_gt(sum(lex2$edgeNormals[, 1] * outward), 0)
})
