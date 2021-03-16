#' Create Holdridge plot
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

#' @describeIn CoordinatesToXY Convert from Holdridge coordinates
#' @param pet,prec  Numeric vectors giving *p*otential *e*vapo*t*ranspiration
#'  ratio and annual *prec*ipitation (in mm).
#' @export
#' @keywords internal
HoldridgeToXY <- function (pet, prec) {
  pet08 <- log2(pet) + 3
  prec08 <- log2(prec / 1000) + 4
  
  TernaryCoords(rbind(8 - pet08 - prec08, prec08, pet08))
}


#' @rdname AddToTernary
#' @inheritParams HoldridgeToXY
#' @export
AddToHoldridge <- function (PlottingFunction, pet, prec, ...) {
  xy <- CoordinatesToXY(pet, prec)
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
