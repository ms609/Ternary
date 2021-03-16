#' Create Holdridge plot
#' 
#' @param pet,precipitation  Numeric vectors giving potential evapotranspiration
#'  ratio and annual precipitation (in mm).
#' @param add Logical specifying whether to add points to an existing plot.
Holdridge <- function (pet, precipitation, add = FALSE,
                       atip = NULL, btip = NULL, ctip = NULL,
                       alab = 'Potential evaoptranspiration ratio',
                       blab = 'Annual precipitation / mm',
                       clab = 'Humidity province',
                       lab.offset = 0.16, lab.col = NULL,
                       xlim = NULL, ylim = NULL,
                       lab.cex = 1.0, lab.font = 0, tip.cex = lab.cex,
                       tip.font = 2, tip.col = 'black',
                       isometric = TRUE, atip.rotate = NULL,
                       btip.rotate = NULL, ctip.rotate = NULL,
                       atip.pos = NULL, btip.pos = NULL, ctip.pos = NULL,
                       padding = 0.04,
                       col = NA,
                       grid.lines = 8, grid.col = c( '#004D40', '#1E88E5', '#D81B60'),
                       grid.lty = 'solid', grid.lwd = par('lwd'),
                       grid.minor.lines = 1, grid.minor.col = 'lightgrey',
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
      point = point,
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

  #' Add elements to ternary plot
  #' 
  #' Plot shapes onto a ternary diagram created with [`TernaryPlot()`].
  #' 
  #' @param PlottingFunction Function to add data to a plot; perhaps one of
  #'        \code{\link[graphics]{points}},
  #'        \code{\link[graphics]{lines}} or
  #'        \code{\link[graphics]{text}}.
  #' @template coordinatesParam
  #' @param fromCoordinates,toCoordinates For `TernaryArrows()`, coordinates at 
  #' which arrows should begin and end; _cf._ `x0`, `y0`, `x1` and `y1` in 
  #' \link[graphics]{arrows}.  Recycled as necessary.
  #' @param \dots Additional parameters to pass to `PlottingFunction()`.  
  #' If using `TernaryText()`, this will likely include the parameter `labels`,
  #' to specify the text to plot.
  #' 
  #' @examples 
  #' coords <- list(
  #'   A = c(1, 0, 2),
  #'   B = c(1, 1, 1),
  #'   C = c(1.5, 1.5, 0),
  #'   D = c(0.5, 1.5, 1)
  #' )
  #' TernaryPlot()
  #' AddToTernary(lines, coords, col='darkgreen', lty='dotted', lwd=3)
  #' TernaryLines(coords, col='darkgreen')
  #' TernaryArrows(coords[1], coords[2:4], col='orange', length=0.2, lwd=1)
  #' TernaryText(coords, cex=0.8, col='red', font=2)
  #' TernaryPoints(coords, pch=1, cex=2, col='blue')
  #' AddToTernary(points, coords, pch=1, cex=3)
  #' 
  #' 
  #' @template MRS
  #' @export
  AddToHoldridge <- function (PlottingFunction, coordinates, ...) {
    xy <- CoordinatesToXY(coordinates)
    PlottingFunction(xy[1, ], xy[2, ], ...)
  }
  
  #' @describeIn AddToTernary Add  \link[graphics]{arrows}
  #' @importFrom graphics arrows
  #' @export
  HoldridgeArrows <- function (fromCoordinates, toCoordinates=fromCoordinates, ...) {
    fromXY <- CoordinatesToXY(fromCoordinates)
    toXY <- CoordinatesToXY(toCoordinates)
    
    # Return:
    arrows(fromXY[1L, ], fromXY[2L, ], toXY[1L, ], toXY[2L, ], ...)
  }
  
  #' @describeIn AddToTernary Add \link[graphics]{lines}
  #' @importFrom graphics lines
  #' @export
  HoldridgeLines <- function (coordinates, ...) AddToTernary(lines, coordinates, ...)
  
  #' @describeIn AddToTernary Add \link[graphics]{points}
  #' @importFrom graphics points
  #' @export
  HoldridgePoints <- function (coordinates, ...) AddToTernary(points, coordinates, ...)
  
  #' @describeIn AddToTernary Add \link[graphics:polygon]{polygons}
  #' @importFrom graphics polygon
  #' @export
  HoldridgePolygon <- function (coordinates, ...) AddToTernary(polygon, coordinates, ...)
  
  #' @describeIn AddToTernary Add \link[graphics]{text}
  #' @importFrom graphics text
  #' @export
  HoldridgeText <- function (coordinates, ...) AddToTernary(text, coordinates, ...)
  