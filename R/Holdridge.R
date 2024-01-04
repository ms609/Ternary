#' Plot life zones on a Holdridge plot
#'
#' `HoldridgePlot()` creates a blank triangular plot, as proposed by
#' Holdridge (1947, 1967), onto which potential evapotranspiration
#' (\acronym{PET}) ratio and annual precipitation data can be plotted
#' (using the [`AddToHoldridge()`] family of functions) in order to interpret
#' climatic life zones.
#'
#' [`HoldridgePoints()`], [`HoldridgeText()`] and related functions allow data
#' points to be added to an existing plot; [`AddToHoldridge()`] allows plotting
#' using any of the standard plotting functions.
#'
#' [`HoldridgeBelts()`] and [`HoldridgeHexagons()`] plot interpretative lines
#' and hexagons allowing plotted data to be linked to interpreted climate
#' settings.
#' 
#' Please cite Tsakalos _et al._ (2023) when using this function.
#'
#' @inheritParams TernaryPlot
#'
#' @param hex.border,hex.lty,hex.lwd Parameters to pass to
#' `HoldridgeHexagons()`.  Set to `NA` to suppress hexagons.
#' @param hex.labels 38-element character vector specifying label for
#' each hexagonal class, from top left to bottom right.
#' @param hex.cex,hex.font,hex.text.col Parameters passed to
#' \code{\link[graphics:text]{text()}} to plot `hex.labels`.
#' @references
#' Holdridge (1947),
#' "Determination of world plant formations from simple climatic data",
#' _Science_ 105:367&ndash;368. \doi{10.1126/science.105.2727.367}
#'
#' Holdridge (1967), _[Life zone ecology]_.
#' Tropical Science Center, San Jos&eacute;
#' 
#' Tsakalos, Smith, Luebert & Mucina (2023).
#' "climenv: Download, extract and visualise climatic and elevation data.",
#' _Journal of Vegetation Science_ 6:e13215. \doi{10.1111/jvs.13215}
#'
#' [Life zone ecology]: https://reddcr.go.cr/sites/default/files/centro-de-documentacion/holdridge_1966_-_life_zone_ecology.pdf
#'
#' @encoding UTF-8
#' @examples
#' data(holdridgeLifeZonesUp, package = "Ternary")
#' HoldridgePlot(hex.labels = holdridgeLifeZonesUp)
#' HoldridgeBelts()
#' @template MRS
#' @family Holdridge plotting functions
#' @export
HoldridgePlot <- function(atip = NULL, btip = NULL, ctip = NULL,
                          alab = "Potential evapotranspiration ratio",
                          blab = "Annual precipitation / mm",
                          clab = "Humidity province",
                          lab.offset = 0.22,
                          lab.col = c("#D81B60", "#1E88E5", "#111111"),
                          xlim = NULL, ylim = NULL, region = NULL,
                          lab.cex = 1.0,
                          lab.font = 0,
                          tip.cex = lab.cex,
                          tip.font = 2,
                          tip.col = "black",
                          isometric = TRUE,
                          atip.rotate = NULL,
                          btip.rotate = NULL, ctip.rotate = NULL,
                          atip.pos = NULL, btip.pos = NULL, ctip.pos = NULL,
                          padding = 0.16,
                          col = NA,
                          panel.first = NULL, panel.last = NULL,
                          grid.lines = 8,
                          grid.col = c(NA, "#1E88E5", "#D81B60"),
                          grid.lty = "solid",
                          grid.lwd = par("lwd"),
                          grid.minor.lines = 0,
                          grid.minor.col = "lightgrey",
                          grid.minor.lty = "solid",
                          grid.minor.lwd = par("lwd"),
                          hex.border = "#888888",
                          hex.col = HoldridgeHypsometricCol,
                          hex.lty = "solid",
                          hex.lwd = par("lwd"),
                          hex.cex = 0.5,
                          hex.labels = NULL,
                          hex.font = NULL,
                          hex.text.col = "black",
                          axis.cex = 0.8,
                          axis.col = c(grid.col[2], grid.col[3], NA),
                          axis.font = par("font"),
                          axis.labels = TRUE,
                          axis.lty = "solid",
                          axis.lwd = 1,
                          axis.rotate = TRUE,
                          axis.pos = NULL,
                          axis.tick = TRUE,
                          ticks.lwd = axis.lwd,
                          ticks.length = 0.025,
                          ticks.col = grid.col,
                          ...) {
  tri <- .TrianglePlot(
    atip = atip, btip = btip, ctip = ctip,
    alab = alab, blab = blab, clab = clab,
    atip.pos = atip.pos,
    btip.pos = btip.pos,
    ctip.pos = ctip.pos,
    atip.rotate = atip.rotate,
    btip.rotate = btip.rotate,
    ctip.rotate = ctip.rotate,
    padding = padding,
    point = 1L,
    lab.col = lab.col,
    lab.cex = lab.cex,
    lab.font = lab.font,
    lab.offset = lab.offset,
    axis.col = axis.col,
    axis.cex = axis.cex,
    axis.labels = list(
      2^(5:-3),
      1000 * 2^(-4:4),
      rev(c(
        "semi-\nparched", "superarid", "perarid", "arid", "semiarid",
        "subhumid", "humid", "perhumid", "superhumid"
      ))
    ),
    axis.lty = axis.lty,
    axis.font = axis.font,
    axis.lwd = axis.lwd,
    axis.rotate = axis.rotate,
    axis.tick = axis.tick,
    axis.pos = axis.pos,
    grid.lines = grid.lines,
    grid.col = grid.col,
    grid.lwd = grid.lwd,
    grid.lty = grid.lty,
    grid.minor.lines = grid.minor.lines,
    grid.minor.col = grid.minor.col,
    grid.minor.lty = grid.minor.lty,
    grid.minor.lwd = grid.minor.lwd,
    isometric = isometric,
    ticks.col = ticks.col,
    ticks.incline = c(FALSE, TRUE, TRUE),
    ticks.length = ticks.length,
    ticks.lwd = ticks.lwd,
    tip.col = tip.col,
    tip.cex = tip.cex,
    tip.font = tip.font,
    xlim = xlim,
    ylim = ylim,
    col = col
  )

  # Set graphical parameters
  mc <- match.call(expand.dots = FALSE)
  graphicalParams <- names(mc$...) %in% names(par())
  new_par <- mc$...[graphicalParams]
  if (isometric) {
    new_par$pty <- "s"
  }

  original_par <- par(new_par)
  on.exit(par(original_par), add = TRUE)

  .StartPlot(tri, ...)
  options(".Last.triangle" = tri)

  .PlotBackground(tri)

  panel.first

  HoldridgeHexagons(
    border = hex.border, hex.col = hex.col, lty = hex.lty,
    lwd = hex.lwd, labels = hex.labels, font = hex.font,
    cex = hex.cex, text.col = hex.text.col
  )

  .PlotMinorGridLines(tri$grid.lines, tri$grid.minor.lines,
    col = tri$grid.minor.col,
    lty = tri$grid.minor.lty,
    lwd = tri$grid.minor.lwd
  )

  .PlotMajorGridLines(tri$grid.lines,
    col = tri$grid.col,
    lty = tri$grid.lty,
    lwd = tri$grid.lwd
  )

  panel.last

  .PlotAxisTicks(tri)

  .HoldridgeAxisLabels(tri)

  lapply(1:3, .AxisLines)
  lapply(1:3, .TitleAxis)
  .TitleCorners()

  # Return:
  invisible(tri)
}

.HoldridgeAxisLabels <- function(tri) {
  if (tri$gridExists) {
    lab <- tri$axis.labels

    lapply(seq_along(tri$gridPoints), function(i) {
      p <- tri$gridPoints[i]
      q <- 1 - p
      lineEnds <- vapply(
        list(c(p, 0, q), c(q, p, 0), c(0, q, p)),
        TernaryCoords, double(2)
      )

      .AxisLabel(1, lineEnds, lab = lab[[1]][i])
      .AxisLabel(2, lineEnds, lab = lab[[2]][i])
    })

    offset <- 7 * tri$gridPoints[2] / 8
    lapply(seq_along(tri$gridPoints), function(i) {
      p <- tri$gridPoints[i] + offset
      q <- 1 - p
      lineEnds <- vapply(
        list(c(p, 0, q), c(q, p, 0), c(0, q, p)),
        TernaryCoords, double(2)
      )

      .AxisLabel(3, lineEnds, lab = lab[[3]][i])
    })
  }
}

#' @rdname HoldridgePlot
#' @export
HoldridgeBelts <- function(grid.col = "#004D40", grid.lty = "dotted",
                           grid.lwd = par("lwd")) {
  direction <- 1L # May support other values in future?
  linePoints <- c(1, 2, 3, 5, 7, 9) / 16
  tern_height <- switch(direction,
    sqrt(3 / 4),
    1,
    sqrt(3 / 4),
    1
  )
  tern_width <- switch(direction,
    1,
    sqrt(3 / 4),
    1,
    sqrt(3 / 4),
    1
  )

  lapply(linePoints, function(p) {
    x <- tern_width * switch(direction,
      c(-1, 1) * (1 - p) / 2,
      c(0, 0.5 - abs(0.5 - p)) * 2,
      c(-1, 1) * p / 2,
      c(0, -(0.5 - abs(0.5 - p))) * 2
    )
    y <- rep(tern_height, 2) * (p - switch(direction,
      0,
      0.5,
      1,
      0.5
    ))
    lines(x, y, col = grid.col, lty = grid.lty, lwd = grid.lwd)
  })

  # Return:
  invisible(NULL)
}


#' Convert a point in evapotranspiration-precipitation space to an appropriate
#' cross-blended hypsometric colour
#'
#' Used to colour `HoldridgeHexagons()`, and may also be used to aid the
#' interpretation of PET + precipitation data in any graphical context.
#'
#' @inheritParams HoldridgeToXY
#' @param opacity Opacity level to be converted to the final two characters
#' of an \acronym{RGBA} hexadecimal colour definition, e.g. `#000000FF`.
#' Specify a character string, which will be interpreted as a hexadecimal
#' alpha value and appended to the six \acronym{RGB} hexadecimal digits;
#' a numeric in the range 0 (transparent) to 1 (opaque);
#' or `NA`, to return only the six \acronym{RGB} digits.
#' @return Character vector listing \acronym{RGB} or (if `opacity != NA`)
#' \acronym{RGBA} values corresponding to each PET-precipitation value pair.
#' @template MRS
#' @references
#' Palette derived from the hypsometric colour scheme presented at
#' [Shaded Relief](https://www.shadedrelief.com/hypso/hypso.html).
#' @examples
#' HoldridgePlot(hex.col = HoldridgeHypsometricCol)
#' VeryTransparent <- function(...) HoldridgeHypsometricCol(..., opacity = 0.3)
#' HoldridgePlot(hex.col = VeryTransparent)
#' pet <- holdridge$PET
#' prec <- holdridge$Precipitation
#' ptCol <- HoldridgeHypsometricCol(pet, prec)
#' HoldridgePoints(pet, prec, pch = 21, bg = ptCol)
#' @template MRS
#' @importFrom grDevices colorRampPalette
#' @family Holdridge plotting functions
#' @export
HoldridgeHypsometricCol <- function(pet, prec, opacity = NA) {
  .Within257 <- function(x) pmax(1, pmin(257, x))
  xy <- HoldridgeToXY(pet, prec)
  aridity <- colorRampPalette(c(arid = "#efd8c6", humid = "#9fc4b3"),
    space = "Lab"
  )(257)[.Within257(xy[1, ] * 256 + 129)]
  ret <- vapply(seq_along(aridity), function(i) {
    colorRampPalette(c(aridity[i], "#ffffff"),
      space = "Lab"
    )(257)[.Within257(xy[2, i] / 0.541 * 256 + 1)]
  }, character(1))
  if (is.numeric(opacity)) {
    paste0(ret, as.hexmode(ceiling(opacity * 255)))
  } else if (is.character(opacity)) {
    nChar <- nchar(opacity)
    if (!all(nChar <= 2)) {
      warning("`opacity` could not be interpreted as hexadecimal value < 256")
    }
    opacity <- paste0(vapply(
      2L - nChar,
      function(pad) paste0(rep("0", pad), collapse = ""),
      character(1)
    ), opacity)
    paste0(ret, opacity)
  } else {
    ret
  }
}

#' @rdname HoldridgePlot
#' @param border Colour to use for hexagon borders.
#' @param hex.col Fill colour for hexagons.  Provide a vector specifying a
#' colour for each hexagon in turn, reading from left to right and top to
#' bottom, or a function that accepts two arguments, numerics `pet` and `prec`,
#' and returns a colour in a format accepted by
#' \code{\link[graphics:polygon]{polygon()}}.
#' @param labels Vector specifying labels for life zone hexagons to be plotted.
#' Suggested values: [`holdridgeClassesUp`], [`holdridgeLifeZonesUp`].
#' @param lty,lwd,cex,font \link[graphics:par]{Graphical parameters} specifying
#' properties of hexagons to be plotted.
#' @param text.col Colour of text to be printed in hexagons.
HoldridgeHexagons <- function(border = "#004D40",
                              hex.col = HoldridgeHypsometricCol,
                              lty = "dotted",
                              lwd = par("lwd"),
                              labels = NULL,
                              cex = 1.0,
                              text.col = NULL,
                              font = NULL) {
  hexIndex <- matrix(c(
    26, 19, 13, 8, 4, 1,
    27, 20, 14, 9, 5, 2,
    28, 21, 15, 10, 6, 3,
    29, 22, 16, 11, 7, NA,
    30, 23, 17, 12, NA, NA,
    31, 24, 18, rep(NA, 3),
    32, 25, rep(NA, 4),
    33, rep(NA, 5)
  ), 6)

  .FillCol <- function(i, j, x, y) {
    if (is.function(hex.col)) {
      pp <- XYToHoldridge(x, y)
      hex.col(pp[1, ], pp[2, ])
    } else if (length(hex.col) == 1) {
      hex.col
    } else {
      hex.col[hexIndex[j + 1, i]]
    }
  }

  starts <- TernaryToXY(rbind(
    rep(0, 8), # biot
    seq(0, 28, by = 4), # prec
    seq(32, 4, by = -4)
  )) # pet
  e <- 1 / 16
  n <- 2 * e / sqrt(3)

  hexX <- c(0, 0, e, e + e, e + e, e)
  hexY <- c(0, n, 3 * n / 2, n, 0, -n / 2)

  hexTopX <- c(0, 0, e, e + e, e + e)
  hexTopY <- c(0, n / 2, n, n / 2, 0)

  for (i in 1:8) {
    start <- starts[, i]
    midX <- start[1] + hexTopX[3]
    midY <- start[2]
    polygon(start[1] + hexTopX, start[2] + hexTopY,
      col = .FillCol(i, 0, midX, midY), border = NA
    )
    lines(start[1] + hexTopX, start[2] + hexTopY,
      col = border, lty = lty, lwd = lwd
    )
    text(midX, midY + (e / 2), labels[hexIndex[1, i]],
      cex = cex, col = text.col,
      font = font
    )
    start <- start + c(hexTopX[3], hexTopY[3])
    for (j in seq_len(min(5, 8 - i)) - 1L) {
      turtleX <- start[1] + (j * hexX[3])
      turtleY <- start[2] + (j * hexY[3])
      midX <- turtleX + e
      midY <- turtleY + (n / 2)
      polygon(turtleX + hexX, turtleY + hexY,
        col = .FillCol(i, j, midX, midY),
        lty = lty, border = border, lwd = lwd
      )
      text(midX, midY, labels[hexIndex[1 + j, i]],
        cex = cex, col = text.col,
        font = font
      )
    }
  }
}

#' @describeIn CoordinatesToXY Convert from Holdridge coordinates
#' @param pet,prec  Numeric vectors giving *p*otential *e*vapo*t*ranspiration
#'  ratio and annual *prec*ipitation (in mm).
#' @export
#' @keywords internal
HoldridgeToXY <- function(pet, prec) {
  pet08 <- log2(pet) + 3
  prec08 <- log2(prec / 1000) + 4

  plottable <- is.finite(pet08) & is.finite(prec08)

  TernaryCoords(
    rbind(8 - pet08 - prec08, prec08, pet08)[, plottable, drop = FALSE]
  )
}


#' @rdname AddToTernary
#' @inheritParams HoldridgeToXY
#' @family Holdridge plotting functions
#' @export
AddToHoldridge <- function(PlottingFunction, pet, prec, ...) {
  xy <- HoldridgeToXY(pet, prec)
  PlottingFunction(xy[1, ], xy[2, ], ...)
}

#' @describeIn AddToTernary Add  \link[graphics]{arrows} to Holdridge plot
#' @importFrom graphics arrows
#' @export
HoldridgeArrows <- function(fromCoordinates, toCoordinates = fromCoordinates,
                            ...) {
  fromXY <- CoordinatesToXY(fromCoordinates)
  toXY <- CoordinatesToXY(toCoordinates)

  # Return:
  arrows(fromXY[1L, ], fromXY[2L, ], toXY[1L, ], toXY[2L, ], ...)
}

#' @describeIn AddToTernary Add \link[graphics]{lines} to Holdridge plot
#' @importFrom graphics lines
#' @export
HoldridgeLines <- function(pet, prec, ...) {
  AddToHoldridge(lines, pet, prec, ...)
}

#' @describeIn AddToTernary Add \link[graphics]{points} to Holdridge plot
#' @importFrom graphics points
#' @export
HoldridgePoints <- function(pet, prec, ...) {
  AddToHoldridge(points, pet, prec, ...)
}

#' @describeIn AddToTernary Add \link[graphics:polygon]{polygons} to Holdridge
#' plot
#' @importFrom graphics polygon
#' @export
HoldridgePolygon <- function(pet, prec, ...) {
  AddToHoldridge(polygon, pet, prec, ...)
}

#' @describeIn AddToTernary Add \link[graphics]{text} to Holdridge plot
#' @importFrom graphics text
#' @export
HoldridgeText <- function(pet, prec, ...) {
  AddToHoldridge(text, pet, prec, ...)
}
