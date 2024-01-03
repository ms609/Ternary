#' Create a ternary plot
#'
#' Create and style a blank ternary plot.
#'
#' The plot will be generated using the standard 'graphics' plot functions, on
#' which additional elements can be added using cartesian coordinates, perhaps
#' using functions such as \code{\link[graphics]{arrows}},
#' \code{\link[graphics]{legend}} or \code{\link[graphics]{text}}.
#'
#' @param atip,btip,ctip Character string specifying text to title corners,
#'  proceeding clockwise from the corner specified in `point` (default: top).
#' @param alab,blab,clab Character string specifying text with which to label
#'  the corresponding sides of the triangle.
#'  Left or right-pointing arrows are produced by
#'  typing `\\U2190` or `\\U2192`, or using `expression('value' %->% '')`.
#' @param lab.offset Numeric specifying distance between midpoint of axis label
#'  and the axis.
#'  Increase `padding` if labels are being clipped.
#'  Use a vector of length three to specify a different offset for each label.
#'
#' @param point Character string specifying the orientation of the ternary plot:
#'  should the triangle point `"up"`, `"right"`, `"down"` or `"left"`?
#'  The integers 1 to 4 can be used in place of the character strings.
#' @param clockwise Logical specifying the direction of axes.  If `TRUE` (the
#'  default), each axis runs from zero to its maximum value in a clockwise
#'  direction around the plot.
#'
#' @param xlim,ylim Numeric vectors of length two specifying the minimum and
#'  maximum _x_ and _y_ limits of the plotted area, to which `padding` will be
#'  added.
#'  The default is to display the complete height or width of the plot.
#'  Allows cropping to magnified region of the plot. (See vignette for diagram.)
#'  May be overridden if `isometric = TRUE`; see documentation of
#' `isometric` parameter.
#'
#' @param lab.cex,tip.cex Numeric specifying character expansion (font size)
#'  for axis labels.
#'  Use a vector of length three to specify a different value for each direction.
#' @param lab.font,tip.font Numeric specifying font style (Roman, bold, italic,
#'  bold-italic) for axis titles.
#'  Use a vector of length three to set a different font for each direction.
#' @param lab.col Character vector specifying colours for axis labels. Use a
#'  vector of length three to specify a different colour for each label.
#' @param atip.rotate,btip.rotate,ctip.rotate Integer specifying number of
#'  degrees to rotate label of rightmost apex.
#' @param atip.pos,btip.pos,ctip.pos Integer specifying positioning of labels,
#'  iff the corresponding `xlab.rotate` parameter is set.
#'
#' @param isometric Logical specifying whether to enforce an equilateral shape
#'  for the ternary plot.
#'  If only one of `xlim` and `ylim` is set, the other will be calculated to
#'  maintain an equilateral plot.
#'  If both `xlim` and `ylim` are set, but have different ranges, then the limit
#'  with the smaller range will be scaled until its range matches that of the
#'  other limit.
#' @param padding Numeric specifying size of internal margin of the plot;
#'  increase if axis labels are being clipped.
#' @param col The colour for filling the plot; see
#' \code{\link[graphics]{polygon}}.
#'
#' @param panel.first An expression to be evaluated after the plot axes are
#'  set up but before any plotting takes place.
#'  This can be useful for drawing backgrounds, e.g. with [`ColourTernary()`]
#'  or [`HorizontalGrid()`].
#'  Note that this works by lazy evaluation: passing this argument from other
#'  plot methods may well not work since it may be evaluated too early.
#' @param panel.last An expression to be evaluated after plotting has taken
#'  place but before the axes and box are added.  See the comments about
#'  `panel.first`.
#'
#' @param grid.lines Integer specifying the number of grid lines to plot.
#' @param grid.minor.lines Integer specifying the number of minor (unlabelled)
#'  grid lines to plot between each major pair.
#' @param grid.col,grid.minor.col Colours to draw the grid lines. Use a vector
#'  of length three to set different values for each direction.
#' @param grid.lty,grid.minor.lty Character or integer vector; line type of
#'  the grid lines. Use a vector of length three to set different values for
#'  each direction.
#' @param grid.lwd,grid.minor.lwd Non-negative numeric giving line width of the
#'  grid lines.  Use a vector of length three to set different values for each
#'  direction.
#'
#' @param axis.lty  Line type for both the axis line and tick marks.
#'  Use a vector of length three to set a different value for each direction.
#' @param axis.labels This can either be a logical value specifying whether
#'  (numerical) annotations are to be made at the tickmarks, or a character or
#'  expression vector of labels to be placed at the tick points,
#'  or a list of length three, with each entry specifying labels to be placed
#'  on each axis in turn.
#' @param axis.cex Numeric specifying character expansion (font size)
#'  for axis labels.
#'  Use a vector of length three to set a different value for each direction.
#' @param axis.font Font for text. Defaults to \code{par('font')}.
#' @param axis.rotate Logical specifying whether to rotate axis labels
#'  to parallel grid lines, or numeric specifying custom rotation for each axis,
#'  to be passed as `srt` parameter to `text()`.  Expand margins or set
#'  `par(xpd = NA)` if labels are clipped.
#' @param axis.pos Vector of length one or three specifying position of axis
#'  labels, to be passed as `pos` parameter to `text()`; populated automatically
#'  if `NULL` (the default).
#' @param axis.tick Logical specifying whether to mark the axes with tick marks.
#' @param axis.lwd,ticks.lwd Line width for the axis line and tick marks.
#'  Zero or negative values will suppress the line or ticks.
#'  Use a vector of length three to set different values for each axis.
#' @param ticks.length Numeric specifying distance that ticks should extend
#'  beyond the plot margin.  Also affects position of axis labels, which are
#'  plotted at the end of each tick.
#'  Use a vector of length three to set a different length for each direction.
#' @param axis.col,ticks.col,tip.col Colours for the axis line,
#'  tick marks and tip labels respectively.
#'  Use a vector of length three to set a different value for each direction.
#'  \code{axis.col = NULL} means to use \code{par('fg')}, possibly specified
#'  inline, and \code{ticks.col = NULL} means to use whatever colour
#'  \code{axis.col} resolved to.
#'
#'
#' @param \dots Additional parameters to \code{\link[graphics]{plot}}.
#'
#' @seealso
#'  - [`AddToTernary()`]: Add elements to a ternary plot
#'  - [`TernaryCoords()`]: Convert ternary coordinates to Cartesian
#'    (_x_ and _y_) coordinates
#'  - [`TernaryXRange()`], [`TernaryYRange()`]: What are the _x_ and _y_ limits
#'      of the plotted region?
#'
#'
#' @examples
#' TernaryPlot(
#'   atip = "Top", btip = "Bottom", ctip = "Right", axis.col = "red",
#'   col = rgb(0.8, 0.8, 0.8)
#' )
#' HorizontalGrid(grid.lines = 2, grid.col = "blue", grid.lty = 1)
#' # the second line corresponds to the base of the triangle, and is not drawn
#' @template MRS
#'
#' @importFrom graphics par plot polygon
#' @export
TernaryPlot <- function(atip = NULL, btip = NULL, ctip = NULL,
                        alab = NULL, blab = NULL, clab = NULL,
                        lab.offset = 0.16, lab.col = NULL,
                        point = "up", clockwise = TRUE,
                        xlim = NULL, ylim = NULL,
                        lab.cex = 1.0, lab.font = 0, tip.cex = lab.cex,
                        tip.font = 2, tip.col = "black",
                        isometric = TRUE, atip.rotate = NULL,
                        btip.rotate = NULL, ctip.rotate = NULL,
                        atip.pos = NULL, btip.pos = NULL, ctip.pos = NULL,
                        padding = 0.08,
                        col = NA,
                        panel.first = NULL, panel.last = NULL,
                        grid.lines = 10, grid.col = "darkgrey",
                        grid.lty = "solid", grid.lwd = par("lwd"),
                        grid.minor.lines = 4, grid.minor.col = "lightgrey",
                        grid.minor.lty = "solid", grid.minor.lwd = par("lwd"),
                        axis.lty = "solid",
                        axis.labels = TRUE, axis.cex = 0.8,
                        axis.font = par("font"),
                        axis.rotate = TRUE,
                        axis.pos = NULL,
                        axis.tick = TRUE,
                        axis.lwd = 1,
                        ticks.lwd = axis.lwd, ticks.length = 0.025,
                        axis.col = "black", ticks.col = grid.col,
                        ...) {
  tern <- .TrianglePlot(
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
    axis.labels = axis.labels,
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
    sideOrder = if (clockwise) 1:3 else c(3, 1, 2),
    ticks.col = ticks.col,
    ticks.incline = clockwise,
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


  .StartPlot(tern, ...)
  options(".Last.triangle" = tern)

  .PlotBackground(tern)

  panel.first

  .PlotMinorGridLines(tern$grid.lines, tern$grid.minor.lines,
    col = tern$grid.minor.col,
    lty = tern$grid.minor.lty,
    lwd = tern$grid.minor.lwd
  )

  .PlotMajorGridLines(tern$grid.lines,
    col = tern$grid.col,
    lty = tern$grid.lty,
    lwd = tern$grid.lwd
  )

  panel.last

  .PlotAxisTicks(tern)

  .PlotAxisLabels(tern)

  lapply(1:3, .AxisLines)
  lapply(1:3, .TitleAxis)
  .TitleCorners()

  # Return:
  invisible(tern)
}

#' @describeIn TernaryPlot Add `grid.lines` horizontal lines to the ternary plot
#' @template directionParam
#'
#' @importFrom graphics par
#' @export
HorizontalGrid <- function(grid.lines = 10, grid.col = "grey",
                           grid.lty = "dotted", grid.lwd = par("lwd"),
                           direction = getOption("ternDirection", 1L)) {
  if (!(direction %in% 1:4)) {
    stop("Parameter `direction` must be an integer from 1 to 4")
  }
  linePoints <- seq(from = 0, to = 1, length.out = grid.lines + 1L)
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


  lapply(linePoints[-c(1, grid.lines + 1L)], function(p) {
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

#' Add elements to ternary or Holdridge plot
#'
#' Plot shapes onto a ternary diagram created with [`TernaryPlot()`],
#' or a Holdridge plot created with [`HoldridgePlot()`].
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
#' to specify the text to plot. Other useful
#' \link[graphics:par]{graphical parameters} include `srt` to rotate text.
#'
#' @examples
#' # Data to plot
#' coords <- list(
#'   A = c(1, 0, 2),
#'   B = c(1, 1, 1),
#'   C = c(1.5, 1.5, 0),
#'   D = c(0.5, 1.5, 1)
#' )
#' 
#' # Set up plot
#' oPar <- par(mar = rep(0, 4), xpd = NA) # reduce margins and write in them
#' TernaryPlot()
#' 
#' # Add elements to ternary diagram
#' AddToTernary(lines, coords, col = "darkgreen", lty = "dotted", lwd = 3)
#' TernaryLines(coords, col = "darkgreen")
#' TernaryArrows(coords[1], coords[2:4], col = "orange", length = 0.2, lwd = 1)
#' TernaryText(coords, cex = 0.8, col = "red", font = 2)
#' TernaryPoints(coords, pch = 1, cex = 2, col = "blue")
#' AddToTernary(graphics::points, coords, pch = 1, cex = 3)
#'
#' # An equivalent syntax applies to Holdridge plots:
#' HoldridgePlot()
#' pet <- c(0.8, 2, 0.42)
#' prec <- c(250, 400, 1337)
#' HoldridgeText(pet, prec, c("A", "B", "C"))
#' AddToHoldridge(graphics::points, pet, prec, cex = 3)
#' 
#' # Restore original plotting parameters
#' par(oPar)
#' @template MRS
#' @export
AddToTernary <- function(PlottingFunction, coordinates, ...) {
  xy <- CoordinatesToXY(coordinates)
  PlottingFunction(xy[1, ], xy[2, ], ...)
}

#' Convert user-specified ternary coordinates into X and Y coordinates
#'
#' Accepts various formats of input data; extracts ternary coordinates and
#' converts to X and Y coordinates.
#'
#' @template coordinatesParam
#'
#' @return `CoordinatesToXY()` returns an array of two rows, corresponding to
#' the X and Y coordinates of `coordinates`.
#'
#' @template MRS
#' @export
#' @keywords internal
CoordinatesToXY <- function(coordinates) {
  dims <- dim(coordinates)

  # Return:
  if (is.null(dims)) {
    if (is.list(coordinates)) {
      vapply(coordinates, TernaryCoords, double(2))
    } else if (is.numeric(coordinates)) {
      matrix(TernaryCoords(coordinates), nrow = 2)
    }
  } else if (length(dims) == 2L) {
    which_dim <- if (dims[2] == 3L) {
      1L
    } else if (dims[1] == 3L) {
      2L
    } else {
      stop("Coordinates must be ternary points")
    }
    apply(coordinates, which_dim, TernaryCoords)
  } else {
    stop("Unrecognized format for coordinates parameter.")
  }
}

#' @describeIn AddToTernary Add \link[graphics]{segments}
#' @importFrom graphics segments
#' @export
TernarySegments <- function(fromCoordinates, toCoordinates = fromCoordinates,
                            ...) {
  fromXY <- CoordinatesToXY(fromCoordinates)
  toXY <- CoordinatesToXY(toCoordinates)

  # Return:
  segments(fromXY[1L, ], fromXY[2L, ], toXY[1L, ], toXY[2L, ], ...)
}

#' @describeIn AddToTernary Add  \link[graphics]{arrows}
#' @importFrom graphics arrows
#' @export
TernaryArrows <- function(fromCoordinates, toCoordinates = fromCoordinates,
                          ...) {
  fromXY <- CoordinatesToXY(fromCoordinates)
  toXY <- CoordinatesToXY(toCoordinates)

  # Return:
  arrows(fromXY[1L, ], fromXY[2L, ], toXY[1L, ], toXY[2L, ], ...)
}

#' @describeIn AddToTernary Add \link[graphics]{lines}
#' @importFrom graphics lines
#' @export
TernaryLines <- function(coordinates, ...) {
  AddToTernary(lines, coordinates, ...)
}

#' @describeIn AddToTernary Add \link[graphics]{points}
#' @importFrom graphics points
#' @export
TernaryPoints <- function(coordinates, ...) {
  AddToTernary(points, coordinates, ...)
}

#' @describeIn AddToTernary Add \link[graphics:polygon]{polygons}
#' @importFrom graphics polygon
#' @export
TernaryPolygon <- function(coordinates, ...) {
  AddToTernary(polygon, coordinates, ...)
}

#' @describeIn AddToTernary Add \link[graphics]{text}
#' @importFrom graphics text
#' @export
TernaryText <- function(coordinates, ...) {
  AddToTernary(text, coordinates, ...)
}

#' @describeIn AddToTernary Add points, joined by lines
#' @importFrom graphics lines points
#' @export
JoinTheDots <- function(coordinates, ...) {
  AddToTernary(points, coordinates, ...)
  AddToTernary(lines, coordinates, ...)
}
