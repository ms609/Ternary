#' Create Holdridge plot
#' 
#' @inheritParams TernaryPlot
#' 
#' @param hex.border,hex.col,hex.lty,hex.lwd Parameters to pass to 
#' `HoldridgeHexagons()`.  Set to `NA` to suppress hexagons
#' 
HoldridgePlot <- function (atip = NULL, btip = NULL, ctip = NULL,
                           alab = 'Potential evaoptranspiration ratio',
                           blab = 'Annual precipitation / mm',
                           clab = 'Humidity province',
                           lab.offset = 0.22, 
                           lab.col = c('#D81B60', '#1E88E5', '#111111'),
                           xlim = NULL, ylim = NULL,
                           lab.cex = 1.0, lab.font = 0, tip.cex = lab.cex,
                           tip.font = 2, tip.col = 'black',
                           isometric = TRUE, atip.rotate = NULL,
                           btip.rotate = NULL, ctip.rotate = NULL,
                           atip.pos = NULL, btip.pos = NULL, ctip.pos = NULL,
                           padding = 0.16,
                           col = NA,
                           
                           grid.lines = 8,
                           grid.col = c(NA, '#1E88E5', '#D81B60'),
                           grid.lty = 'solid', grid.lwd = par('lwd'),
                           grid.minor.lines = 0, grid.minor.col = 'lightgrey',
                           grid.minor.lty = 'solid', grid.minor.lwd = par('lwd'),
                           
                           hex.border = '#00000066',
                           hex.col = HoldridgeHypsometricCol,
                           hex.lty = 'solid',
                           hex.lwd = par('lwd'),
                           
                           axis.lty = 'solid',
                           axis.labels = TRUE, axis.cex = 0.8,
                           axis.font = par('font'),
                           axis.rotate = TRUE,
                           axis.pos = NULL,
                           axis.tick = TRUE,
                           axis.lwd = 1,
                           ticks.lwd = axis.lwd, ticks.length = 0.025,
                           axis.col = 'black', ticks.col = grid.col,
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
    axis.labels = list(2 ^ (5:-3),
                       1000 * 2 ^ (-4:4),
                       rev(c('superarid', 'perarid', 'arid', 'semiarid',
                         'subhumid', 'humid', 'perhumid', 'superhumid'))
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
    col = col)
  
  if (isometric) {
    original_par <- par(pty = 's')
    on.exit(par(original_par), add = TRUE)
  }
  
  .StartPlot(tri, ...)
  options('.Last.triangle' = tri)
  
  .PlotBackground(tri)
  
  HoldridgeHexagons(border = hex.border, col = hex.col, lty = hex.lty,
                    lwd = hex.lwd)
  
  .PlotMinorGridLines(tri$grid.lines, tri$grid.minor.lines, 
                      col = tri$grid.minor.col,
                      lty = tri$grid.minor.lty,
                      lwd = tri$grid.minor.lwd)
  
  .PlotMajorGridLines(tri$grid.lines,
                      col = tri$grid.col,
                      lty = tri$grid.lty,
                      lwd = tri$grid.lwd)
  
  .PlotAxisTicks(tri)
  
  .HoldridgeAxisLabels(tri)
  
  lapply(1:3, .AxisLines)
  lapply(1:3, .TitleAxis)
  .TitleCorners()
  
  # Return:
  return <- tri
}
  
.HoldridgeAxisLabels <- function (tri) {
  if (tri$gridExists) {
    lab <- tri$axis.labels
    
    lapply(seq_along(tri$gridPoints), function (i) {
      p <- tri$gridPoints[i]
      q <- 1 - p
      lineEnds <- vapply(list(c(p, 0, q), c(q, p, 0), c(0, q, p)),
                         TernaryCoords, double(2))

      .AxisLabel(1, lineEnds, lab = lab[[1]][i])
      .AxisLabel(2, lineEnds, lab = lab[[2]][i])
    })
    
    gridGap <- tri$gridPoints[2] / 8
    lapply(seq_along(tri$gridPoints)[-1], function (i) {
      p <- tri$gridPoints[i] - gridGap
      q <- 1 - p
      lineEnds <- vapply(list(c(p, 0, q), c(q, p, 0), c(0, q, p)),
                         TernaryCoords, double(2))

      .AxisLabel(3, lineEnds, lab = lab[[3]][i - 1])
    })

  }
}

#' @describeIn HoldridgePlot Plot interpretative horizontals.
HoldridgeBelts <- function (grid.col = '#004D40', grid.lty = 'dotted',
                            grid.lwd = par('lwd'),
                            direction = getOption('ternDirection')) {
  
  if (!(direction %in% 1:4)) {
    stop("Parameter `direction` must be an integer from 1 to 4")
  }
  linePoints <- c(1, 2, 3, 5, 7, 9) / 16
  tern_height <- switch(direction, sqrt(3/4), 1, sqrt(3/4), 1)
  tern_width <- switch(direction, 1, sqrt(3/4), 1, sqrt(3/4), 1)
  
  lapply(linePoints, function (p) {
    x <- tern_width * switch(direction,
                             c(-1, 1) * (1 - p) / 2,
                             c(0, 0.5 - abs(0.5 - p)) * 2,
                             c(-1, 1) * p / 2,
                             c(0, -(0.5 - abs(0.5 - p))) * 2)
    y <- rep(tern_height, 2) * (p - switch(direction, 0, 0.5, 1, 0.5))
    lines(x, y, col = grid.col, lty = grid.lty, lwd = grid.lwd)
  })
  
  # Return:
  return <- NULL
}


#' Convert a point in evapotranspiration-precipitation space to an appropriate
#' cross-blended hypsometric colour
#' 
#' Palette basis: https://www.shadedrelief.com/hypso/hypso.html
#' 
#' @inheritParams HoldridgeToXY
#' 
#' @return Character vector listing RGBA values corresponding to each pet-prec
#' value.
#' @template MRS
#' @importFrom grDevices colorRamp
#' @export
HoldridgeHypsometricCol <- function (pet, prec, opacity = NA) {
  .Within257 <- function (x) pmax(1, pmin(257, x))
  xy <- HoldridgeToXY(pet, prec)
  aridity <- colorRampPalette(c(arid = "#efd8c6", humid = "#9fc4b3"), 
                              space = 'Lab')(257)[.Within257(xy[1, ] * 256 + 129)]
  ret <- vapply(seq_along(aridity), function (i) {
    colorRampPalette(c(aridity[i], "#ffffff"),
                     space = 'Lab')(257)[.Within257(xy[2, i] / 0.541 * 256 + 1)]
  }, character(1))
  if (is.numeric(opacity)) {
    paste0(ret, as.hexmode(opacity * 255))
  } else if (is.character(opacity)) {
    paste0(ret, opacity)
  } else {
    ret
  }
}

#' @describeIn HoldridgePlot Plot interpretative hexagons.
#' @param col Fill colour for hexagons.  Provide a vector specifying a colour
#' for each hexagon in turn, reading from left to right and top to bottom,
#' or a function that accepts two arguments, numerics `pet` and `prec`,
#' and returns a colour in a format accepted by [graphics:polygon][`polygon()`].
HoldridgeHexagons <- function (border = '#004D40',
                               col = HoldridgeHypsometricCol,
                               lty = 'dotted',
                               lwd = par('lwd')) {
  
  hexIndex <- matrix(c(26, 19, 13, 8, 4, 1,
                       27, 20, 14, 9, 5, 2,
                       28, 21, 15, 10, 6, 3,
                       29, 22, 16, 11, 7, NA,
                       30, 23, 17, 12, NA, NA,
                       31, 24, 18, rep(NA, 3),
                       32, 25, rep(NA, 4),
                       33, rep(NA, 5)), 6)
  
  .FillCol <- function(i, j, x, y) {
    if (is.function(col)) {
      pp <- XYToHoldridge(x, y)
      col(pp[1, ], pp[2, ])
    } else if (length(col) == 1) {
      col
    } else {
      col[hexIndex[j + 1, i]]
    }
  }
  
  starts <- TernaryToXY(rbind(rep(0, 8),          # biot
                              seq(0, 28, by = 4), # prec
                              seq(32, 4, by = -4))) # pet
  e <- 1 / 16
  n <- 2 * e / sqrt(3)
  
  hexX <- c(0, 0, e, e + e, e + e, e, 0)
  hexY <- c(0, n, 3 * n / 2, n, 0, -n / 2, 0)
  
  hexTopX <- c(0, 0, e, e + e, e + e, 0)
  hexTopY <- c(0, n / 2, n, n / 2, 0, 0)
  
  for (i in 1:8) {
    start <- starts[, i]
    polygon(start[1] + hexTopX, start[2] + hexTopY,
            col = .FillCol(i, 0, start[1] + hexTopX[3], start[2]),
            lty = lty, border = border, lwd = lwd)
    start <- start + c(hexTopX[3], hexTopY[3])
    for (j in seq_len(min(5, 8 - i)) - 1L) {
      turtleX <- start[1] + (j * hexX[3])
      turtleY <- start[2] + (j * hexY[3])
      polygon(turtleX + hexX, turtleY + hexY,
              col = .FillCol(i, j, turtleX + e, turtleY + (n / 2)),
              lty = lty, border = border, lwd = lwd)
    }
  }
}

#' @describeIn CoordinatesToXY Convert from Holdridge coordinates
#' @param pet,prec  Numeric vectors giving *p*otential *e*vapo*t*ranspiration
#'  ratio and annual *prec*ipitation (in mm).
#' @export
#' @keywords internal
HoldridgeToXY <- function (pet, prec) {
  pet08 <- log2(pet) + 3
  prec08 <- log2(prec / 1000) + 4
  
  plottable <- is.finite(pet08) & is.finite(prec08)
  
  TernaryCoords(rbind(8 - pet08 - prec08, prec08, pet08)[, plottable, drop = FALSE])
}


#' @rdname AddToTernary
#' @inheritParams HoldridgeToXY
#' @export
AddToHoldridge <- function (PlottingFunction, pet, prec, ...) {
  xy <- HoldridgeToXY(pet, prec)
  PlottingFunction(xy[1, ], xy[2, ], ...)
}

#' @describeIn AddToTernary Add  \link[graphics]{arrows} to Holdridge plot
#' @importFrom graphics arrows
#' @export
HoldridgeArrows <- function (fromCoordinates, toCoordinates = fromCoordinates,
                             ...) {
  fromXY <- CoordinatesToXY(fromCoordinates)
  toXY <- CoordinatesToXY(toCoordinates)
  
  # Return:
  arrows(fromXY[1L, ], fromXY[2L, ], toXY[1L, ], toXY[2L, ], ...)
}

#' @describeIn AddToTernary Add \link[graphics]{lines} to Holdridge plot
#' @importFrom graphics lines
#' @export
HoldridgeLines <- function (pet, prec, ...) {
  AddToHoldridge(lines, pet, prec, ...)
}

#' @describeIn AddToTernary Add \link[graphics]{points} to Holdridge plot
#' @importFrom graphics points
#' @export
HoldridgePoints <- function (pet, prec, ...) {
  AddToHoldridge(points, pet, prec, ...)
}

#' @describeIn AddToTernary Add \link[graphics:polygon]{polygons} to Holdridge 
#' plot
#' @importFrom graphics polygon
#' @export
HoldridgePolygon <- function (pet, prec, ...) {
  AddToHoldridge(polygon, pet, prec, ...)
}

#' @describeIn AddToTernary Add \link[graphics]{text} to Holdridge plot
#' @importFrom graphics text
#' @export
HoldridgeText <- function (pet, prec, ...) {
  AddToHoldridge(text, pet, prec, ...)
}
