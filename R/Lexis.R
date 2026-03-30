# Lexis diagram (parallelogram) plots
#
# A Lexis plot relaxes the ternary constraint on one axis, producing a
# parallelogram of equilateral triangles rather than a single triangle.
# The user specifies ranges for two of the three axes; the third is derived.


# ---- Internal axis resolution ------------------------------------------------

# Determine which axis is free from the provided ranges.
# Returns list with freeAxis, boundedAxes, range1, range2, axisMap,
# aRange, bRange, cRange.
.ResolveLexisAxes <- function(aRange, bRange, cRange) {
  provided <- c(a = !is.null(aRange), b = !is.null(bRange),
                c = !is.null(cRange))
  nProvided <- sum(provided)

  if (nProvided == 0L) {
    bRange <- c(0, 1)
    cRange <- c(0, 1)
    provided <- c(a = FALSE, b = TRUE, c = TRUE)
  } else if (nProvided != 2L) {
    stop("Specify exactly two of aRange, bRange, cRange (or none for defaults)")
  }

  freeAxis <- names(provided)[!provided]
  boundedAxes <- names(provided)[provided]
  ranges <- list(a = aRange, b = bRange, c = cRange)

  # Map axis roles to triplicate indices (a=1, b=2, c=3)
  idx <- c(a = 1L, b = 2L, c = 3L)
  axisMap <- c(
    axis1 = as.integer(idx[boundedAxes[1]]),
    axis2 = as.integer(idx[boundedAxes[2]]),
    free  = as.integer(idx[freeAxis])
  )

  list(freeAxis = freeAxis, boundedAxes = unname(boundedAxes),
       range1 = ranges[[boundedAxes[1]]],
       range2 = ranges[[boundedAxes[2]]],
       aRange = aRange, bRange = bRange, cRange = cRange,
       axisMap = axisMap)
}


# ---- Internal coordinate transforms -----------------------------------------

# Convert Lexis coordinates (two bounded axes) to Cartesian XY.
.LexisCoordsToXY <- function(coord1, coord2,
                              range1, range2, freeAxis,
                              direction) {
  c1 <- coord1 - range1[1]
  c2 <- coord2 - range2[1]
  k <- diff(range1) + diff(range2)
  free <- k - c1 - c2

  abc <- switch(freeAxis,
    a = rbind(free, c1, c2),
    b = rbind(c1, free, c2),
    c = rbind(c1, c2, free)
  )

  TernaryCoords(abc, direction = direction, region = ternRegionDefault)
}

# Convert Cartesian XY to Lexis coordinates (two bounded axes).
# Returns a 2-row matrix named after the bounded axes.
.XYToLexisCoords <- function(x, y, range1, range2, freeAxis, direction) {
  abc <- XYToTernary(x, y, direction = direction,
                     region = ternRegionDefault)
  k <- diff(range1) + diff(range2)

  rows <- switch(freeAxis, a = c(2L, 3L), b = c(1L, 3L), c = c(1L, 2L))

  rbind(
    coord1 = abc[rows[1], ] * k + range1[1],
    coord2 = abc[rows[2], ] * k + range2[1]
  )
}


# ---- Public coordinate transforms -------------------------------------------

#' @describeIn CoordinatesToXY Convert from Lexis coordinates
#'
#' @param a,b,c Numeric vectors of Lexis coordinates.
#'   Provide exactly two of the three: the two axes that were bounded
#'   when creating the plot.  If ranges are not supplied and a Lexis plot
#'   is active, ranges are read from the plot state; otherwise the
#'   default \eqn{[0, 1]} range is used with axis \code{a} as the free
#'   (derived) axis.
#' @param aRange,bRange,cRange Numeric vectors of length 2 giving the
#'   range of each axis, or \code{NULL}.  Supply ranges for the same two
#'   axes that were bounded in the corresponding [LexisPlot()] call.
#'
#' @examples
#' LexisToXY(b = 0.5, c = 0.3)
#' XYToLexis(0.05, 0.52, bRange = c(0, 1), cRange = c(0, 1))
#'
#' @family Lexis plotting functions
#' @template MRS
#' @export
#' @keywords internal
LexisToXY <- function(a = NULL, b = NULL, c = NULL,
                      aRange = NULL, bRange = NULL, cRange = NULL,
                      direction = getOption("ternDirection", 1L)) {
  # Which coords provided?
  provided <- c(a = !is.null(a), b = !is.null(b), c = !is.null(c))
  if (sum(provided) != 2L) {
    stop("Provide exactly two of a, b, c")
  }

  freeAxis <- names(provided)[!provided]
  bounded <- names(provided)[provided]
  coords <- list(a = a, b = b, c = c)

  # Resolve ranges: explicit > plot state > default
  if (is.null(aRange) && is.null(bRange) && is.null(cRange)) {
    tri <- getOption(".Last.triangle")
    if (!is.null(tri) && inherits(tri, "LexisPlot")) {
      aRange <- tri$aRange
      bRange <- tri$bRange
      cRange <- tri$cRange
    }
  }
  ranges <- list(a = aRange, b = bRange, c = cRange)
  r1 <- ranges[[bounded[1]]]
  r2 <- ranges[[bounded[2]]]
  if (is.null(r1)) r1 <- c(0, 1)
  if (is.null(r2)) r2 <- c(0, 1)

  .LexisCoordsToXY(coords[[bounded[1]]], coords[[bounded[2]]],
                    r1, r2, freeAxis, direction)
}

#' @rdname XYToTernary
#' @inheritParams LexisToXY
#' @export
XYToLexis <- function(x, y,
                      aRange = NULL, bRange = NULL, cRange = NULL,
                      direction = getOption("ternDirection", 1L)) {
  # Resolve axes: explicit ranges > plot state > default (a free)
  if (is.null(aRange) && is.null(bRange) && is.null(cRange)) {
    tri <- getOption(".Last.triangle")
    if (!is.null(tri) && inherits(tri, "LexisPlot")) {
      resolved <- list(freeAxis = tri$freeAxis,
                       range1 = tri$range1, range2 = tri$range2,
                       boundedAxes = tri$boundedAxes)
    } else {
      resolved <- .ResolveLexisAxes(NULL, NULL, NULL)
    }
  } else {
    resolved <- .ResolveLexisAxes(aRange, bRange, cRange)
  }

  result <- .XYToLexisCoords(x, y, resolved$range1, resolved$range2,
                              resolved$freeAxis, direction)
  rownames(result) <- resolved$boundedAxes
  result
}


# ---- Dispatcher and wrappers ------------------------------------------------

#' @rdname AddToTernary
#' @param a,b,c Numeric vectors of Lexis coordinates.  Provide the two
#'   bounded axes (the same two specified in [LexisPlot()]).
#' @family Lexis plotting functions
#' @order 30
#' @export
AddToLexis <- function(PlottingFunction, a = NULL, b = NULL, c = NULL,
                       ...) {
  tri <- getOption(".Last.triangle")
  bounded <- tri$boundedAxes
  coords <- list(a = a, b = b, c = c)

  c1 <- coords[[bounded[1]]]
  c2 <- coords[[bounded[2]]]
  if (is.null(c1) || is.null(c2)) {
    stop(sprintf("Provide the two bounded axes: %s and %s",
                 bounded[1], bounded[2]))
  }

  xy <- .LexisCoordsToXY(c1, c2, tri$range1, tri$range2,
                          tri$freeAxis, tri$direction)
  PlottingFunction(xy[1, ], xy[2, ], ...)
}

#' @describeIn AddToTernary Add \link[graphics]{lines} to Lexis plot
#' @importFrom graphics lines
#' @order 35
#' @export
LexisLines <- function(a = NULL, b = NULL, c = NULL, ...) {
  AddToLexis(lines, a = a, b = b, c = c, ...)
}

#' @describeIn AddToTernary Add \link[graphics]{points} to Lexis plot
#' @importFrom graphics points
#' @order 35
#' @export
LexisPoints <- function(a = NULL, b = NULL, c = NULL, ...) {
  AddToLexis(points, a = a, b = b, c = c, ...)
}

#' @describeIn AddToTernary Add \link[graphics:polygon]{polygons} to Lexis plot
#' @importFrom graphics polygon
#' @order 35
#' @export
LexisPolygon <- function(a = NULL, b = NULL, c = NULL, ...) {
  AddToLexis(polygon, a = a, b = b, c = c, ...)
}

#' @describeIn AddToTernary Add \link[graphics]{text} to Lexis plot
#' @importFrom graphics text
#' @order 35
#' @export
LexisText <- function(a = NULL, b = NULL, c = NULL, ...) {
  AddToLexis(text, a = a, b = b, c = c, ...)
}


# ---- Internal drawing helpers ------------------------------------------------

# Outward unit normals for the 4 edges of an ordered parallelogram.
# Corrects for winding direction so normals always point outward.
.LexisEdgeNormals <- function(corners) {
  normals <- vapply(1:4, function(i) {
    j <- if (i < 4L) i + 1L else 1L
    dx <- corners[1, j] - corners[1, i]
    dy <- corners[2, j] - corners[2, i]
    len <- sqrt(dx * dx + dy * dy)
    c(-dy / len, dx / len)
  }, double(2))

  # Signed area: positive = counterclockwise
  signedArea <- sum(vapply(1:4, function(i) {
    j <- if (i < 4L) i + 1L else 1L
    corners[1, i] * corners[2, j] - corners[1, j] * corners[2, i]
  }, double(1)))
  if (signedArea > 0) normals <- -normals

  normals
}


# Find points where coord1' + coord2' = d crosses [0, span1] x [0, span2].
.LexisDiagEndpoints <- function(d, span1, span2) {
  eps <- sqrt(.Machine$double.eps)
  pts <- matrix(nrow = 0, ncol = 2)
  .Clamp <- function(x, lo, hi) min(max(x, lo), hi)

  # Bottom (c2' = 0): c1' = d
  if (d >= -eps && d <= span1 + eps)
    pts <- rbind(pts, c(.Clamp(d, 0, span1), 0))
  # Left (c1' = 0): c2' = d
  if (d >= -eps && d <= span2 + eps)
    pts <- rbind(pts, c(0, .Clamp(d, 0, span2)))
  # Top (c2' = span2): c1' = d - span2
  c1p <- d - span2
  if (c1p >= -eps && c1p <= span1 + eps)
    pts <- rbind(pts, c(.Clamp(c1p, 0, span1), span2))
  # Right (c1' = span1): c2' = d - span1
  c2p <- d - span1
  if (c2p >= -eps && c2p <= span2 + eps)
    pts <- rbind(pts, c(span1, .Clamp(c2p, 0, span2)))

  unique(round(pts, 10))
}

# Insert n equally-spaced values between each consecutive pair.
.MinorVals <- function(vals, n) {
  if (n <= 0L || length(vals) < 2L) return(numeric(0))
  unlist(lapply(seq_len(length(vals) - 1L), function(i) {
    seq(vals[i], vals[i + 1L], length.out = n + 2L)[-c(1L, n + 2L)]
  }))
}

# Draw one family of grid lines.
# family: "1" (constant axis1), "2" (constant axis2), "d" (diagonal).
.LexisGridFamily <- function(lex, vals, family, col, lty, lwd,
                             excludeBoundary = TRUE) {
  range1 <- lex$range1; range2 <- lex$range2
  freeAxis <- lex$freeAxis; dir <- lex$direction
  eps <- sqrt(.Machine$double.eps)

  if (excludeBoundary) {
    if (family == "1") {
      vals <- vals[vals > range1[1] + eps & vals < range1[2] - eps]
    } else if (family == "2") {
      vals <- vals[vals > range2[1] + eps & vals < range2[2] - eps]
    } else {
      dMax <- diff(range1) + diff(range2)
      vals <- vals[vals > eps & vals < dMax - eps]
    }
  }
  if (length(vals) == 0L) return(invisible(NULL))

  if (family == "1") {
    n <- length(vals)
    xy0 <- .LexisCoordsToXY(vals, rep(range2[1], n),
                              range1, range2, freeAxis, dir)
    xy1 <- .LexisCoordsToXY(vals, rep(range2[2], n),
                              range1, range2, freeAxis, dir)
    segments(xy0[1, ], xy0[2, ], xy1[1, ], xy1[2, ],
             col = col, lty = lty, lwd = lwd)
  } else if (family == "2") {
    n <- length(vals)
    xy0 <- .LexisCoordsToXY(rep(range1[1], n), vals,
                              range1, range2, freeAxis, dir)
    xy1 <- .LexisCoordsToXY(rep(range1[2], n), vals,
                              range1, range2, freeAxis, dir)
    segments(xy0[1, ], xy0[2, ], xy1[1, ], xy1[2, ],
             col = col, lty = lty, lwd = lwd)
  } else { # diagonal
    r1Span <- diff(range1); r2Span <- diff(range2)
    for (d in vals) {
      pts <- .LexisDiagEndpoints(d, r1Span, r2Span)
      if (nrow(pts) >= 2L) {
        xy <- .LexisCoordsToXY(pts[, 1] + range1[1],
                                pts[, 2] + range2[1],
                                range1, range2, freeAxis, dir)
        lines(xy[1, ], xy[2, ], col = col, lty = lty, lwd = lwd)
      }
    }
  }
  invisible(NULL)
}


# Compute tick directions for each axis, oriented outward from their edge.
# Ticks are parallel to their own grid-line family, not perpendicular to the
# edge, so it is clear which of the two arriving grid families a label belongs
# to.
#
# Returns: list(axis1, axis2, free23, free34) — each a unit 2-vector.
.LexisTickDirs <- function(lex) {
  corners <- lex$corners
  span1 <- diff(lex$range1)
  span2 <- diff(lex$range2)

  # Constant-axis1 grid lines run in the coord2 direction (C4 − C1)
  d_a1 <- (corners[, 4] - corners[, 1]) / span2
  d_a1 <- d_a1 / sqrt(sum(d_a1^2))

  # Constant-axis2 grid lines run in the coord1 direction (C2 − C1)
  d_a2 <- (corners[, 2] - corners[, 1]) / span1
  d_a2 <- d_a2 / sqrt(sum(d_a2^2))

  # Diagonal grid lines: direction along c1'↑ c2'↓ (or the reverse)
  d_diag <- d_a1 - d_a2
  d_diag <- d_diag / sqrt(sum(d_diag^2))

  # Orient each outward from its edge
  .out <- function(d, edgeIdx) {
    if (sum(d * lex$edgeNormals[, edgeIdx]) < 0) -d else d
  }

  list(
    axis1  = .out(d_a1, 1L),  # edge 1->2
    axis2  = .out(d_a2, 4L),  # edge 4->1
    free23 = .out(d_diag, 2L), # edge 2->3
    free34 = .out(d_diag, 3L)  # edge 3->4
  )
}

# Axis ticks on edge 1->2 (axis1) and edge 4->1 (axis2).
.LexisAxisTicks <- function(lex, axis1Vals, axis2Vals) {
  am <- lex$axisMap
  td <- .LexisTickDirs(lex)

  # Axis1 ticks on edge 1->2
  d <- td$axis1
  len <- lex$ticks.length[am["axis1"]]
  xy <- .LexisCoordsToXY(axis1Vals, rep(lex$range2[1], length(axis1Vals)),
                          lex$range1, lex$range2, lex$freeAxis, lex$direction)
  segments(xy[1, ], xy[2, ],
           xy[1, ] + d[1] * len, xy[2, ] + d[2] * len,
           col = lex$ticks.col[am["axis1"]],
           lwd = lex$ticks.lwd[am["axis1"]])

  # Axis2 ticks on edge 4->1
  d <- td$axis2
  len <- lex$ticks.length[am["axis2"]]
  xy <- .LexisCoordsToXY(rep(lex$range1[1], length(axis2Vals)), axis2Vals,
                          lex$range1, lex$range2, lex$freeAxis, lex$direction)
  segments(xy[1, ], xy[2, ],
           xy[1, ] + d[1] * len, xy[2, ] + d[2] * len,
           col = lex$ticks.col[am["axis2"]],
           lwd = lex$ticks.lwd[am["axis2"]])
}

# Format numeric grid values as compact label strings.
.FormatGridLabels <- function(vals) {
  if (all(abs(vals - round(vals)) < 1e-10)) {
    as.character(round(vals))
  } else {
    format(round(vals, 10), drop0trailing = TRUE, trim = TRUE)
  }
}

# Axis labels at tick positions.
.LexisAxisLabels <- function(lex, axis1Vals, axis2Vals) {
  am <- lex$axisMap
  td <- .LexisTickDirs(lex)
  mult <- 2.5

  # Axis1 labels on edge 1->2
  d <- td$axis1
  off <- lex$ticks.length[am["axis1"]] * mult
  xy <- .LexisCoordsToXY(axis1Vals, rep(lex$range2[1], length(axis1Vals)),
                          lex$range1, lex$range2, lex$freeAxis, lex$direction)
  lab1 <- if (isTRUE(lex$axis.labels)) {
    .FormatGridLabels(axis1Vals)
  } else if (is.list(lex$axis.labels)) {
    lex$axis.labels[[am["axis1"]]]
  } else {
    lex$axis.labels
  }
  text(xy[1, ] + d[1] * off, xy[2, ] + d[2] * off, lab1,
       cex = lex$axis.cex[am["axis1"]], col = lex$lab.col[am["axis1"]],
       font = lex$axis.font[am["axis1"]])

  # Axis2 labels on edge 4->1
  d <- td$axis2
  off <- lex$ticks.length[am["axis2"]] * mult
  xy <- .LexisCoordsToXY(rep(lex$range1[1], length(axis2Vals)), axis2Vals,
                          lex$range1, lex$range2, lex$freeAxis, lex$direction)
  lab2 <- if (isTRUE(lex$axis.labels)) {
    .FormatGridLabels(axis2Vals)
  } else if (is.list(lex$axis.labels)) {
    lex$axis.labels[[am["axis2"]]]
  } else {
    lex$axis.labels
  }
  text(xy[1, ] + d[1] * off, xy[2, ] + d[2] * off, lab2,
       cex = lex$axis.cex[am["axis2"]], col = lex$lab.col[am["axis2"]],
       font = lex$axis.font[am["axis2"]])
}

# Free-axis ticks on edges 2->3 and 3->4, where diagonal grid lines cross.
.LexisAxisTicksFree <- function(lex, dVals) {
  am <- lex$axisMap
  td <- .LexisTickDirs(lex)
  span1 <- diff(lex$range1)
  span2 <- diff(lex$range2)
  k <- span1 + span2
  eps <- sqrt(.Machine$double.eps)
  len <- lex$ticks.length[am["free"]]
  tcol <- lex$ticks.col[am["free"]]
  tlwd <- lex$ticks.lwd[am["free"]]

  # Edge 2->3: coord1 = range1[2], coord2 varies
  d23 <- dVals[dVals >= span1 - eps & dVals <= k + eps]
  if (length(d23) > 0L) {
    coord2 <- d23 - span1 + lex$range2[1]
    coord1 <- rep(lex$range1[2], length(coord2))
    xy <- .LexisCoordsToXY(coord1, coord2,
                            lex$range1, lex$range2, lex$freeAxis, lex$direction)
    d <- td$free23
    segments(xy[1, ], xy[2, ],
             xy[1, ] + d[1] * len, xy[2, ] + d[2] * len,
             col = tcol, lwd = tlwd)
  }

  # Edge 3->4: coord2 = range2[2], coord1 varies
  d34 <- dVals[dVals >= span2 - eps & dVals <= k + eps]
  if (length(d34) > 0L) {
    coord1 <- d34 - span2 + lex$range1[1]
    coord2 <- rep(lex$range2[2], length(coord1))
    xy <- .LexisCoordsToXY(coord1, coord2,
                            lex$range1, lex$range2, lex$freeAxis, lex$direction)
    d <- td$free34
    segments(xy[1, ], xy[2, ],
             xy[1, ] + d[1] * len, xy[2, ] + d[2] * len,
             col = tcol, lwd = tlwd)
  }
}

# Compute free-axis label values at given edge positions.
# Uses free.fun if available; otherwise falls back to ternary complement.
.FreeLabelVals <- function(lex, coord1, coord2, dVals) {
  if (!is.null(lex$free.fun)) {
    args <- setNames(list(coord1, coord2), lex$boundedAxes)
    vals <- do.call(lex$free.fun, args)
    .FormatGridLabels(vals)
  } else if (isTRUE(lex$axis.labels)) {
    k <- diff(lex$range1) + diff(lex$range2)
    .FormatGridLabels(k - dVals)
  } else if (is.list(lex$axis.labels)) {
    lex$axis.labels[[lex$axisMap["free"]]]
  } else {
    lex$axis.labels
  }
}

# Free-axis labels on edges 2->3 and 3->4.
.LexisAxisLabelsFree <- function(lex, dVals) {
  am <- lex$axisMap
  td <- .LexisTickDirs(lex)
  span1 <- diff(lex$range1)
  span2 <- diff(lex$range2)
  k <- span1 + span2
  eps <- sqrt(.Machine$double.eps)
  mult <- 2.5

  acex <- lex$axis.cex[am["free"]]
  acol <- lex$lab.col[am["free"]]
  afont <- lex$axis.font[am["free"]]

  # Edge 2->3: coord1 = range1[2], coord2 varies
  d23 <- dVals[dVals >= span1 - eps & dVals <= k + eps]
  if (length(d23) > 0L) {
    coord2 <- d23 - span1 + lex$range2[1]
    coord1 <- rep(lex$range1[2], length(coord2))
    xy <- .LexisCoordsToXY(coord1, coord2,
                            lex$range1, lex$range2, lex$freeAxis, lex$direction)
    d <- td$free23
    off <- lex$ticks.length[am["free"]] * mult

    labs <- .FreeLabelVals(lex, coord1, coord2, d23)
    text(xy[1, ] + d[1] * off, xy[2, ] + d[2] * off, labs,
         cex = acex, col = acol, font = afont)
  }

  # Edge 3->4: coord2 = range2[2], coord1 varies
  d34 <- dVals[dVals >= span2 - eps & dVals <= k + eps]
  if (length(d34) > 0L) {
    coord1 <- d34 - span2 + lex$range1[1]
    coord2 <- rep(lex$range2[2], length(coord1))
    xy <- .LexisCoordsToXY(coord1, coord2,
                            lex$range1, lex$range2, lex$freeAxis, lex$direction)
    d <- td$free34
    off <- lex$ticks.length[am["free"]] * mult

    labs <- .FreeLabelVals(lex, coord1, coord2, d34)
    text(xy[1, ] + d[1] * off, xy[2, ] + d[2] * off, labs,
         cex = acex, col = acol, font = afont)
  }
}

# Axis titles at edge midpoints.
.LexisAxisTitles <- function(lex) {
  corners <- lex$corners
  am <- lex$axisMap
  labs <- list(lex$alab, lex$blab, lex$clab)

  # Axis1 title on edge 1->2
  lab <- labs[[am["axis1"]]]
  if (!is.null(lab)) {
    mid <- (corners[, 1] + corners[, 2]) / 2
    n <- lex$edgeNormals[, 1]
    off <- lex$lab.offset[am["axis1"]]
    text(mid[1] + n[1] * off, mid[2] + n[2] * off, lab,
         cex = lex$lab.cex[am["axis1"]], font = lex$lab.font[am["axis1"]],
         col = lex$lab.col[am["axis1"]])
  }

  # Axis2 title on edge 4->1
  lab <- labs[[am["axis2"]]]
  if (!is.null(lab)) {
    mid <- (corners[, 4] + corners[, 1]) / 2
    n <- lex$edgeNormals[, 4]
    off <- lex$lab.offset[am["axis2"]]
    text(mid[1] + n[1] * off, mid[2] + n[2] * off, lab,
         cex = lex$lab.cex[am["axis2"]], font = lex$lab.font[am["axis2"]],
         col = lex$lab.col[am["axis2"]])
  }

  # Free axis title on edge 2->3
  lab <- labs[[am["free"]]]
  if (!is.null(lab)) {
    mid <- (corners[, 2] + corners[, 3]) / 2
    n <- lex$edgeNormals[, 2]
    off <- lex$lab.offset[am["free"]]
    text(mid[1] + n[1] * off, mid[2] + n[2] * off, lab,
         cex = lex$lab.cex[am["free"]], font = lex$lab.font[am["free"]],
         col = lex$lab.col[am["free"]])

    # Repeat on edge 3->4
    mid <- (corners[, 3] + corners[, 4]) / 2
    n <- lex$edgeNormals[, 3]
    text(mid[1] + n[1] * off, mid[2] + n[2] * off, lab,
         cex = lex$lab.cex[am["free"]], font = lex$lab.font[am["free"]],
         col = lex$lab.col[am["free"]])
  }
}

# Corner labels at 3 of the 4 parallelogram vertices.
.LexisCornerLabels <- function(lex) {
  am <- lex$axisMap
  tips <- list(lex$atip, lex$btip, lex$ctip)

  .DrawTip <- function(cornerIdx, label, styleIdx) {
    if (is.null(label)) return()
    prevEdge <- if (cornerIdx == 1L) 4L else cornerIdx - 1L
    n <- lex$edgeNormals[, prevEdge] + lex$edgeNormals[, cornerIdx]
    n <- n / sqrt(sum(n * n))
    off <- lex$ticks.length[styleIdx] * 4
    xy <- lex$corners[, cornerIdx]
    text(xy[1] + n[1] * off, xy[2] + n[2] * off, label,
         cex = lex$tip.cex[styleIdx], font = lex$tip.font[styleIdx],
         col = lex$tip.col[styleIdx])
  }

  # corner1 (origin) -> free axis tip
  .DrawTip(1L, tips[[am["free"]]], am["free"])
  # corner2 (axis1 at max) -> axis1 tip
  .DrawTip(2L, tips[[am["axis1"]]], am["axis1"])
  # corner4 (axis2 at max) -> axis2 tip
  .DrawTip(4L, tips[[am["axis2"]]], am["axis2"])
}


# ---- LexisPlot --------------------------------------------------------------

#' Create a Lexis diagram
#'
#' `LexisPlot()` creates a blank Lexis diagram (parallelogram-shaped ternary
#' plot) onto which data can be plotted using the [`AddToLexis()`] family of
#' functions.
#'
#' A Lexis diagram has three axes subject to the constraint
#' \eqn{a + b + c = \textrm{constant}}{a + b + c = constant}.  The user
#' specifies ranges for two of the three axes; the third is derived.
#'
#' @param aRange,bRange,cRange Numeric vectors of length 2 specifying the
#'   range of each axis, or `NULL`.  Exactly two must be non-`NULL`;
#'   the remaining axis is the "free" (derived) axis.  If all three are
#'   `NULL`, the default is `bRange = c(0, 1)`, `cRange = c(0, 1)`
#'   (i.e., axis `a` is free).
#' @param free.fun Optional function used to compute labels for the free
#'   (derived) axis.
#'   Called with named arguments matching the two bounded axes.
#'   For example, if `aRange` and `bRange` are specified (so `c` is free),
#'   pass `free.fun = function(a, b) b - a` to label the free axis with
#'   \eqn{b - a}.  If `NULL` (the default), labels show the ternary
#'   complement \eqn{k - a' - b'} where primes denote shifted coordinates
#'   and \eqn{k = \Delta a + \Delta b}.
#' @inheritParams TernaryPlot
#'
#' @return A list of class `"LexisPlot"` containing plot parameters, returned
#'   invisibly.
#'
#' @examples
#' LexisPlot()
#' LexisPoints(b = c(0.2, 0.4), c = c(0.3, 0.6))
#'
#' # Age-Period-Cohort example (c free, with Cohort labels)
#' LexisPlot(aRange = c(0, 100), bRange = c(1900, 2000),
#'           alab = "Age", blab = "Period", clab = "Cohort",
#'           free.fun = function(a, b) b - a)
#' LexisPoints(a = c(20, 50), b = c(1950, 1980))
#'
#' @family Lexis plotting functions
#' @template MRS
#' @importFrom graphics par plot polygon segments text lines
#' @export
LexisPlot <- function(
    aRange = NULL,
    bRange = NULL,
    cRange = NULL,
    alab = NULL, blab = NULL, clab = NULL,
    atip = NULL, btip = NULL, ctip = NULL,
    point = "up",
    clockwise = TRUE,
    xlim = NULL, ylim = NULL,
    col = NA,
    padding = 0.08,
    isometric = TRUE,
    grid.lines = 10,
    grid.col = "darkgrey",
    grid.lty = "solid",
    grid.lwd = par("lwd"),
    grid.minor.lines = 4,
    grid.minor.col = "lightgrey",
    grid.minor.lty = "solid",
    grid.minor.lwd = par("lwd"),
    axis.labels = TRUE,
    axis.cex = 0.8,
    axis.col = "black",
    axis.font = par("font"),
    axis.lty = "solid",
    axis.lwd = 1,
    axis.rotate = TRUE,
    axis.tick = TRUE,
    ticks.lwd = axis.lwd,
    ticks.length = 0.025,
    ticks.col = grid.col,
    lab.offset = 0.16,
    lab.cex = 1.0,
    lab.font = 0,
    lab.col = NULL,
    tip.cex = lab.cex,
    tip.font = 2,
    tip.col = "black",
    free.fun = NULL,
    panel.first = NULL,
    panel.last = NULL,
    ...) {

  # ---- Direction ------------------------------------------------------------
  direction <- 1L + (pmatch(tolower(point[[1]]),
                            c("right", "down", "left", "up",
                              "east", "south", "west", "north",
                              "2", "3", "4", "1")) %% 4L)
  if (is.na(direction)) {
    stop("`point` must be one of up, down, left or right")
  }
  options("ternDirection" = direction)
  .SetRegion(ternRegionDefault)

  # ---- Resolve axes ---------------------------------------------------------
  axes <- .ResolveLexisAxes(aRange, bRange, cRange)
  freeAxis <- axes$freeAxis
  range1 <- axes$range1
  range2 <- axes$range2
  axisMap <- axes$axisMap

  # ---- Parallelogram corners ------------------------------------------------
  corner1 <- range1[c(1, 2, 2, 1)]
  corner2 <- range2[c(1, 1, 2, 2)]
  corners <- .LexisCoordsToXY(corner1, corner2,
                               range1, range2, freeAxis, direction)

  # ---- Plot limits ----------------------------------------------------------
  if (is.null(xlim)) xlim <- range(corners[1, ])
  if (is.null(ylim)) ylim <- range(corners[2, ])
  if (isometric) {
    xR <- diff(xlim); yR <- diff(ylim)
    if (abs(xR - yR) > 1e-7) {
      if (xR < yR) {
        xlim <- mean(xlim) + c(-1, 1) * yR / 2
      } else {
        ylim <- mean(ylim) + c(-1, 1) * xR / 2
      }
    }
  }

  # ---- Edge geometry --------------------------------------------------------
  edgeNormals <- .LexisEdgeNormals(corners)

  # ---- Build state object ---------------------------------------------------
  lex <- structure(list(
    freeAxis = freeAxis,
    boundedAxes = axes$boundedAxes,
    range1 = range1, range2 = range2,
    aRange = axes$aRange, bRange = axes$bRange, cRange = axes$cRange,
    axisMap = axisMap,
    direction = direction,
    corners = corners,
    axesX = corners[1, c(1:4, 1)],
    axesY = corners[2, c(1:4, 1)],
    edgeNormals = edgeNormals,

    alab = alab, blab = blab, clab = clab,
    atip = atip, btip = btip, ctip = ctip,

    col = col, padding = padding,
    grid.lines = .ValidateGridLines(grid.lines),
    grid.col = .Triplicate(grid.col),
    grid.lty = .Triplicate(grid.lty),
    grid.lwd = .Triplicate(grid.lwd),
    grid.minor.lines = .ValidateGridLines(grid.minor.lines),
    grid.minor.col = .Triplicate(grid.minor.col),
    grid.minor.lty = .Triplicate(grid.minor.lty),
    grid.minor.lwd = .Triplicate(grid.minor.lwd),

    axis.labels = axis.labels,
    axis.cex = .Triplicate(axis.cex),
    axis.col = .Triplicate(axis.col),
    axis.font = .Triplicate(axis.font),
    axis.lty = .Triplicate(axis.lty),
    axis.lwd = .Triplicate(axis.lwd),
    axis.rotate = axis.rotate,
    axis.tick = axis.tick,

    ticks.lwd = .Triplicate(ticks.lwd),
    ticks.length = .Triplicate(ticks.length),
    ticks.col = .Triplicate(ticks.col),

    lab.offset = .Triplicate(lab.offset),
    lab.cex = .Triplicate(lab.cex),
    lab.font = .Triplicate(lab.font),
    lab.col = .Triplicate(if (is.null(lab.col)) "black" else lab.col),

    tip.cex = .Triplicate(tip.cex),
    tip.font = .Triplicate(tip.font),
    tip.col = .Triplicate(tip.col),

    free.fun = free.fun,

    xlim = xlim, ylim = ylim
  ), class = "LexisPlot")

  # ---- Graphical parameters -------------------------------------------------
  mc <- match.call(expand.dots = FALSE)
  graphicalParams <- names(mc$...) %in% names(par())
  new_par <- mc$...[graphicalParams]
  if (isometric) new_par$pty <- "s"
  original_par <- par(new_par)
  on.exit(par(original_par), add = TRUE)

  # ---- Draw -----------------------------------------------------------------
  .StartPlot(lex, ...)
  options(".Last.triangle" = lex)

  # Background fill
  polygon(lex$axesX, lex$axesY, col = lex$col, border = NA)

  panel.first

  # Grid lines
  gl <- lex$grid.lines
  if (.GridExists(gl)) {
    spacing <- max(diff(range1), diff(range2)) / gl
    axis1Major <- seq(range1[1], range1[2], by = spacing)
    axis2Major <- seq(range2[1], range2[2], by = spacing)
    dMajor <- seq(0, diff(range1) + diff(range2), by = spacing)

    # Minor grid
    ml <- lex$grid.minor.lines
    if (ml > 0L) {
      axis1Minor <- .MinorVals(axis1Major, ml)
      axis2Minor <- .MinorVals(axis2Major, ml)
      dMinor <- .MinorVals(dMajor, ml)
      .LexisGridFamily(lex, axis1Minor, "1",
                        lex$grid.minor.col[axisMap["axis1"]],
                        lex$grid.minor.lty[axisMap["axis1"]],
                        lex$grid.minor.lwd[axisMap["axis1"]])
      .LexisGridFamily(lex, axis2Minor, "2",
                        lex$grid.minor.col[axisMap["axis2"]],
                        lex$grid.minor.lty[axisMap["axis2"]],
                        lex$grid.minor.lwd[axisMap["axis2"]])
      .LexisGridFamily(lex, dMinor, "d",
                        lex$grid.minor.col[axisMap["free"]],
                        lex$grid.minor.lty[axisMap["free"]],
                        lex$grid.minor.lwd[axisMap["free"]])
    }

    # Major grid
    .LexisGridFamily(lex, axis1Major, "1",
                      lex$grid.col[axisMap["axis1"]],
                      lex$grid.lty[axisMap["axis1"]],
                      lex$grid.lwd[axisMap["axis1"]])
    .LexisGridFamily(lex, axis2Major, "2",
                      lex$grid.col[axisMap["axis2"]],
                      lex$grid.lty[axisMap["axis2"]],
                      lex$grid.lwd[axisMap["axis2"]])
    .LexisGridFamily(lex, dMajor, "d",
                      lex$grid.col[axisMap["free"]],
                      lex$grid.lty[axisMap["free"]],
                      lex$grid.lwd[axisMap["free"]])
  }

  panel.last

  # Axis lines (4 sides)
  for (i in 1:4) {
    j <- if (i < 4L) i + 1L else 1L
    # Edges 1,3 are axis1 direction; edges 2,4 are axis2 direction
    ai <- if (i %in% c(1L, 3L)) axisMap["axis1"] else axisMap["axis2"]
    lines(corners[1, c(i, j)], corners[2, c(i, j)],
          col = lex$axis.col[ai], lty = lex$axis.lty[ai],
          lwd = lex$axis.lwd[ai])
  }

  # Axis ticks and labels
  if (.GridExists(gl)) {
    .LexisAxisTicks(lex, axis1Major, axis2Major)
    .LexisAxisTicksFree(lex, dMajor)
    if (!isFALSE(lex$axis.labels)) {
      .LexisAxisLabels(lex, axis1Major, axis2Major)
      .LexisAxisLabelsFree(lex, dMajor)
    }
  }

  # Axis titles
  .LexisAxisTitles(lex)

  # Corner labels
  .LexisCornerLabels(lex)

  invisible(lex)
}


# ---- Tile centres for the parallelogram ------------------------------------

# Generate triangle centres that tile a Lexis parallelogram.
# Returns a matrix with rows x, y, triDown, plus an
# "effectiveResolution" attribute.
.LexisTriangleCentres <- function(resolution, lex) {
  range1 <- lex$range1; range2 <- lex$range2
  r1Span <- diff(range1); r2Span <- diff(range2)
  k <- r1Span + r2Span
  spacing <- max(r1Span, r2Span) / resolution
  nR1 <- max(1L, as.integer(ceiling(r1Span / spacing - 1e-9)))
  nR2 <- max(1L, as.integer(ceiling(r2Span / spacing - 1e-9)))

  ij <- expand.grid(j = seq_len(nR1) - 1L, i = seq_len(nR2) - 1L)

  # Up-triangle centres
  upC1 <- (ij$j + 1 / 3) * spacing + range1[1]
  upC2 <- (ij$i + 1 / 3) * spacing + range2[1]

  # Down-triangle centres
  dnC1 <- (ij$j + 2 / 3) * spacing + range1[1]
  dnC2 <- (ij$i + 2 / 3) * spacing + range2[1]

  allC1 <- c(upC1, dnC1)
  allC2 <- c(upC2, dnC2)
  triDown <- rep(c(0L, 1L), each = nrow(ij))

  xy <- .LexisCoordsToXY(allC1, allC2, range1, range2,
                          lex$freeAxis, lex$direction)

  ret <- rbind(x = xy[1, ], y = xy[2, ], triDown = triDown)
  attr(ret, "effectiveResolution") <- k / spacing
  ret
}


# ---- LexisPointValues --------------------------------------------------------

#' Evaluate a function over a Lexis grid
#'
#' `LexisPointValues()` evaluates a function at the centres of triangular
#' tiles that cover a Lexis parallelogram, producing a matrix suitable for
#' [`ColourLexis()`].
#'
#' @param Func Function that takes two named arguments corresponding to the
#'   two bounded axes of the active Lexis plot.  For example, if the plot was
#'   created with `bRange` and `cRange`, `Func` should accept arguments
#'   `b` and `c`.
#' @param resolution Integer specifying the number of tile divisions along the
#'   longer axis of the parallelogram.
#' @param \dots Additional arguments passed to `Func()`.
#'
#' @return A matrix with four rows: `x`, `y` (Cartesian coordinates of tile
#'   centres), `z` (the value of `Func`), and `down` (0 for
#'   upward-pointing triangles, 1 for downward-pointing).  An
#'   `"effectiveResolution"` attribute is attached for use by
#'   [`ColourLexis()`].
#'
#' @examples
#' LexisPlot(blab = "b", clab = "c")
#' vals <- LexisPointValues(function(b, c) b + c, resolution = 10)
#' ColourLexis(vals, legend = TRUE, bty = "n")
#'
#' @family Lexis plotting functions
#' @template MRS
#' @importFrom stats setNames
#' @export
LexisPointValues <- function(Func, resolution = 48L, ...) {
  tri <- getOption(".Last.triangle")
  centres <- .LexisTriangleCentres(resolution, tri)
  coords <- .XYToLexisCoords(centres["x", ], centres["y", ],
                              tri$range1, tri$range2,
                              tri$freeAxis, tri$direction)

  # Build named args matching the bounded axes
  args <- setNames(list(coords[1, ], coords[2, ]), tri$boundedAxes)
  z <- do.call(Func, c(args, list(...)))

  if (length(z) == 1L && ncol(centres) > 1L) {
    warning("`Func` should return a vector, but returned a single value.")
  }

  ret <- rbind(x = centres["x", ], y = centres["y", ],
               z = z, down = centres["triDown", ])
  attr(ret, "effectiveResolution") <- attr(centres, "effectiveResolution")
  ret
}


# ---- ColourLexis -------------------------------------------------------------

#' Colour a Lexis plot
#'
#' Fill the tiles of a Lexis plot with colours determined by a matrix of
#' values, typically produced by [`LexisPointValues()`].
#'
#' @param values Numeric matrix with four named rows: `x`, `y` (Cartesian
#'   coordinates of tile centres), `z` (value for colour mapping), and `down`
#'   (triangle direction: 0 = up, 1 = down).
#' @inheritParams ColourTernary
#'
#' @return Called for its side effect (colouring a Lexis plot).  Invisibly
#'   returns `NULL`.
#'
#' @examples
#' LexisPlot(blab = "b", clab = "c")
#' vals <- LexisPointValues(function(b, c) b - c, resolution = 12)
#' ColourLexis(vals, legend = TRUE, bty = "n")
#'
#' @family Lexis plotting functions
#' @template MRS
#' @importFrom grDevices hcl.colors
#' @export
ColourLexis <- function(values,
                        spectrum = hcl.colors(256L, palette = "viridis",
                                              alpha = 0.6),
                        resolution = attr(values, "effectiveResolution"),
                        direction = getOption("ternDirection", 1L),
                        legend,
                        ...) {
  if (is.null(resolution)) {
    tri <- getOption(".Last.triangle")
    k <- diff(tri$range1) + diff(tri$range2)
    spacing <- max(diff(tri$range1), diff(tri$range2)) /
      sqrt(ncol(values) / 2)
    resolution <- k / spacing
  }
  ColourTernary(values, spectrum = spectrum, resolution = resolution,
                direction = direction, legend = legend, ...)
}

#' @rdname ColourLexis
#' @export
ColorLexis <- ColourLexis


# ---- LexisContour ------------------------------------------------------------

#' Add contours to a Lexis plot
#'
#' Draw contour lines depicting the value of a function across a Lexis
#' parallelogram.
#'
#' @inheritParams LexisPointValues
#' @param filled Logical: if `TRUE`, draw filled contours.
#' @param nlevels Integer giving the approximate number of contour levels.
#' @param levels Numeric vector of contour levels.
#' @param zlim Numeric vector of length 2 giving the z range.
#' @param color.palette Function that takes an integer and returns that many
#'   colours.
#' @param fill.col Colours for filled contours.
#' @param legend Character or numeric vector of legend labels.  Specify `TRUE`
#'   to auto-generate, or a single integer for that many labels.
#' @param legend... List of additional arguments to
#'   [`SpectrumLegend()`][PlotTools::SpectrumLegend].
#' @param func... List of additional arguments passed to `Func()`.
#' @param \dots Further arguments passed to [graphics::contour()].
#'
#' @return Invisibly returns a list with elements `x`, `y`, and `z` (the
#'   evaluation grid).
#'
#' @examples
#' LexisPlot(blab = "b", clab = "c")
#' LexisContour(function(b, c) b * c, resolution = 36)
#'
#' @family Lexis plotting functions
#' @template MRS
#' @importFrom graphics contour .filled.contour
#' @importFrom grDevices hcl.colors
#' @importFrom PlotTools SpectrumLegend
#' @export
LexisContour <- function(
    Func, resolution = 96L,
    filled = FALSE, legend, legend... = list(),
    nlevels = 10, levels = pretty(zlim, nlevels), zlim,
    color.palette = function(n) {
      hcl.colors(n, palette = "viridis", alpha = 0.6)
    },
    fill.col = color.palette(length(levels) - 1),
    func... = list(), ...) {

  tri <- getOption(".Last.triangle")
  range1 <- tri$range1; range2 <- tri$range2
  freeAxis <- tri$freeAxis; direction <- tri$direction
  bounded <- tri$boundedAxes

  # XY grid spanning the parallelogram
  corners <- tri$corners
  xGrid <- seq(min(corners[1, ]), max(corners[1, ]), length.out = resolution)
  yGrid <- seq(min(corners[2, ]), max(corners[2, ]), length.out = resolution)

  eps <- sqrt(.Machine$double.eps)

  FunctionWrapper <- function(x, y) {
    coords <- .XYToLexisCoords(x, y, range1, range2, freeAxis, direction)
    c1 <- coords[1, ]
    c2 <- coords[2, ]
    inPlot <- (c1 >= range1[1] - eps) & (c1 <= range1[2] + eps) &
              (c2 >= range2[1] - eps) & (c2 <= range2[2] + eps)

    ret <- rep(NA_real_, length(x))
    args <- setNames(list(c1[inPlot], c2[inPlot]), bounded)
    ret[inPlot] <- do.call(Func, c(args, func...))
    ret
  }

  z <- outer(X = xGrid, Y = yGrid, FUN = FunctionWrapper)
  if (missing(zlim)) zlim <- range(z, finite = TRUE)

  if (filled) {
    .filled.contour(xGrid, yGrid, z, levels, fill.col)
  }
  contour(xGrid, yGrid, z, add = TRUE, nlevels = nlevels,
          levels = levels, zlim = zlim, ...)

  if (!missing(legend)) {
    if (isTRUE(legend)) legend <- 4
    if (is.numeric(legend) && length(legend) == 1) {
      legend <- signif(seq(zlim[2], zlim[1], length.out = legend))
    }
    do.call(SpectrumLegend, c(list(legend = legend, palette = fill.col),
                              legend...))
  }

  invisible(list(x = xGrid, y = yGrid, z = z))
}
