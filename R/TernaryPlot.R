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
#' proceeding clockwise from the corner specified in `point` (default: top).
#' @param alab,blab,clab Character string specifying text with which to label 
#' the corresponding sides of the triangle.  
#' Left or right-pointing arrows are produced by
#'  typing `\\U2190` or `\\U2192`, or using `expression('value' %->% '')`.
#' @param lab.offset Numeric specifying distance between midpoint of axis label 
#' and the axis.
#'  Increase `padding` if labels are being clipped.
#'  Use a vector of length three to specify a different offset for each label.
#'                      
#' @param point Character string specifying the orientation of the ternary plot: 
#' should the triangle point `"up"`, `"right"`, `"down"` or `"left"`? 
#' The integers 1 to 4 can be used in place of the character strings.
#' @param clockwise Logical specifying the direction of axes.  If `TRUE` (the 
#' default), each axis runs from zero to its maximum value in a clockwise
#' direction around the plot.
#' 
#' @param xlim,ylim Numeric vectors of length 2 specifying the minimum and maximum
#'  _x_ and _y_ limits of the plotted area, to which \code{padding} will be added.
#'  The default is to display the complete height or width of the plot.  
#'  Allows cropping to magnified region of the plot. (See vignette for diagram.)
#'  May be overridden if `isometric=TRUE`; see documentation of
#'  `isometric` parameter.
#'  
#' @param lab.cex,tip.cex Numeric specifying character expansion (font size)
#' for axis labels. 
#' Use a vector of length three to specify a different value for each direction.
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
#' @param padding Numeric specifying size of internal margin of the plot; increase
#'  if axis labels are being clipped.
#' @param col The colour for filling the plot; see \code{\link[graphics]{polygon}}.
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
#'  expression vector of labels to be placed at the tick points.
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
#'  - [`TernaryCoords()`]: Convert ternary coordinates to Cartesian (_x_ and _y_) 
#'      coordinates
#'  - [`TernaryXRange()`], [`TernaryYRange()`]: What are the _x_ and _y_ limits
#'      of the plotted region?
#' 
#' 
#' @examples 
#' TernaryPlot(atip = "Top", btip = "Bottom", ctip = "Right", axis.col = "red", 
#'             col = rgb(0.8, 0.8, 0.8))
#' HorizontalGrid(grid.lines = 2, grid.col = 'blue', grid.lty = 1) 
#' # the second line corresponds to the base of the triangle, and is not drawn
#' @template MRS
#' 
#' @importFrom graphics par plot polygon
#' @export
TernaryPlot <- function (atip = NULL, btip = NULL, ctip = NULL,
                         alab = NULL, blab = NULL, clab = NULL, 
                         lab.offset = 0.16, lab.col = NULL,
                         point = 'up', clockwise = TRUE,
                         xlim = NULL, ylim = NULL,
                         lab.cex = 1.0, lab.font = 0, tip.cex = lab.cex,
                         tip.font = 2, tip.col = 'black',
                         isometric = TRUE, atip.rotate = NULL,
                         btip.rotate = NULL, ctip.rotate = NULL,
                         atip.pos = NULL, btip.pos = NULL, ctip.pos = NULL,
                         padding = 0.08,
                         col = NA,
                         grid.lines = 10, grid.col = 'darkgrey',
                         grid.lty = 'solid', grid.lwd = par('lwd'),
                         grid.minor.lines = 4, grid.minor.col = 'lightgrey',
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
  direction <- 1L + (pmatch(tolower(point), c('right', 'down', 'left', 'up',
                                              'east', 'south', 'west', 'north',
                                              2L, 3L, 4L, 1L)) %% 4L)
  if (is.na(direction)) {
    stop("Point must be one of up, down, left or right")
  } else {
    options('ternDirection' = direction)
  }
  
  # Prepare parameters
  Triplicate <- function (x) if (length(x) == 1) rep(x, 3) else x
  lab.col <- Triplicate(lab.col)
  lab.cex <- Triplicate(lab.cex)
  lab.font <- Triplicate(lab.font)
  lab.offset <- Triplicate(lab.offset)
  axis.col <- Triplicate(axis.col)
  axis.cex <- Triplicate(axis.cex)
  axis.lty <- Triplicate(axis.lty)
  axis.font <- Triplicate(axis.font)
  ticks.col <- Triplicate(ticks.col)
  grid.col <- Triplicate(grid.col)
  grid.lwd <- Triplicate(grid.lwd)
  grid.lty <- Triplicate(grid.lty)
  grid.minor.col <- Triplicate(grid.minor.col)
  grid.minor.lty <- Triplicate(grid.minor.lty)
  grid.minor.lwd <- Triplicate(grid.minor.lwd)
  axis.lwd <- Triplicate(axis.lwd)
  axis.rotate <- Triplicate(axis.rotate)
  axis.pos <- Triplicate(axis.pos)
  ticks.length <- Triplicate(ticks.length)
  ticks.lwd <- Triplicate(ticks.lwd)
  tip.col <- Triplicate(tip.col)
  tip.cex <- Triplicate(tip.cex)
  tip.font <- Triplicate(tip.font)
  sides <- if(clockwise) 1:3 else c(3, 1, 2)
  
  if (isometric) {
    original_par <- par(pty = 's')
    on.exit(par(original_par))
    
    if (is.null(xlim) && !is.null(ylim)) {
      xlim <- TernaryXRange(direction) * (ylim[2] - ylim[1])
    }
    xRange <- xlim[2] - xlim[1]
    if (is.null(ylim) && !is.null(xlim)) {
      ylim <- TernaryYRange(direction) * xRange
    }
    yRange <- ylim[2] - ylim[1]
    
    if (length(xlim) > 0 && abs(xRange - yRange) > 1e-07) {
      if (abs(xRange) < abs(yRange)) {
        xlim <- xlim * (yRange / xRange)
        warning("x range < y range, but isometric = TRUE; setting xlim = c(", 
                xlim[1], ', ', xlim[2], ")")
      } else {
        ylim <- ylim * (xRange / yRange)
        warning("x range > y range, but isometric = TRUE; setting ylim = c(", 
                ylim[1], ', ', ylim[2], ")")
      }
    }
  }
  if (is.null(xlim)) xlim <- TernaryXRange(direction)
  if (is.null(ylim)) ylim <- TernaryYRange(direction)
  padVec <- c(-1, 1) * padding
  
  
  plot(0, type = 'n', axes = FALSE, xlab = '', ylab = '',
       xlim = xlim + padVec, ylim = ylim + padVec, ...)
  axes <- vapply(list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 0, 0)),
                 TernaryCoords, double(2))
  polygon(axes[1, ], axes[2, ], col=col, border=NA)
  
  if (!is.integer(grid.lines)) grid.lines <- ceiling(grid.lines)
  if (!is.integer(grid.minor.lines)) grid.minor.lines <- ceiling(grid.minor.lines)
  if (!is.null(grid.lines) && !is.na(grid.lines) && grid.lines > 1L) {
    # Plot minor grid lines
    if (grid.minor.lines > 0L) {
      n_minor_lines <- grid.lines * (grid.minor.lines + 1L)  + 1L
      minor_line_points <- seq(from = 0, to = 1, length.out = 
                                 n_minor_lines)[-seq(from = 1, to = n_minor_lines, 
                                                     by = grid.minor.lines + 1L)]
      lapply(minor_line_points, function (p) {
        q <- 1 - p
        line_ends <- vapply(list(c(p, q, 0), c(p, 0, q),
                                 c(0, p, q), c(q, p, 0),
                                 c(q, 0, p), c(0, q, p)),
                            TernaryCoords, double(2))
        lapply(list(c(1, 2), c(3, 4), c(5, 6)), function (i) 
          lines(line_ends[1, i], line_ends[2, i], col = grid.minor.col[i[2]/2],
                lty = grid.minor.lty[i[2]/2], lwd = grid.minor.lwd[i[2]/2]))
        NULL
      })
      
    }
    
    # Plot grid
    line_points <- seq(from = 0, to = 1, length.out = grid.lines + 1L)
    
    lapply(line_points[-c(1, grid.lines + 1L)], function (p) {
      q <- 1 - p
      line_ends <- vapply(list(c(p, q, 0), c(p, 0, q),
                               c(0, p, q), c(q, p, 0),
                               c(q, 0, p), c(0, q, p)),
                          TernaryCoords, double(2))
      lapply(list(c(1, 2), c(3, 4), c(5, 6)), function (i) 
      lines(line_ends[1, i], line_ends[2, i], col = grid.col[i[2]/2], 
            lty = grid.lty[i[2]/2], lwd = grid.lwd[i[2]/2]))
      NULL
    })
    
    if (clockwise) {
      axis_degrees <- (c(180, 300, 60) + (direction * 90)) %% 360
      
      rot <- c(c(  0, 270,   0,  90)[direction],
               c( 60, -30,  60, -30)[direction],
               c(-60,  30, -60,  30)[direction])
      
      pos <- c(c(2, 2, 4, 2)[direction],
               c(4, 4, 2, 2)[direction],
               c(4, 2, 2, 4)[direction])
      
      mult <- c(c(5 , 16, 10, 12)[direction],
                c(5 ,  9,  8,  9)[direction],
                c(16,  8, 16,  8)[direction]) / 10
    } else {
      axis_degrees <- (c(240, 0, 120) + (direction * 90)) %% 360
      
      rot <- c(c(-60,  30, -60,  30)[direction],
               c(  0, -90,  00, -90)[direction],
               c( 60, -30,  60, -30)[direction])
      
      pos <- c(c(2, 4, 4, 2)[direction],
               c(4, 4, 2, 2)[direction],
               c(2, 2, 4, 4)[direction])
      
      mult <- c(c(5, 4, 4, 5)[direction],
                c(4, 6, 4, 7)[direction],
                c(8, 4, 7, 3)[direction]) / 5
      
    }
    
    if (is.logical(axis.rotate)) {
      rot <- ifelse(axis.rotate, rot, 0)
      pos.unrotated <- matrix(c(2, 4, 1,
                                3, 1, 2, 
                                4, 2, 3, 
                                1, 3, 4), 3)[, direction]
      pos <- ifelse(axis.rotate, pos, pos.unrotated)
    } else {
      rot <- axis.rotate
    }
    if (!is.null(axis.pos)) pos <- axis.pos
    
  
    # Plot and annotate axes
    lapply(seq_along(line_points), function (i) {
      p <- line_points[i]
      q <- 1 - p
      line_ends <- vapply(list(c(p, 0, q),
                               c(q, p, 0),
                               c(0, q, p)),
                          TernaryCoords, double(2))
                         
      if (axis.tick) {
        AxisTick <- function (side) {
          lines(line_ends[1, side] + c(0, sin(axis_degrees[side] * pi / 180) *
                                         ticks.length[side]),
                line_ends[2, side] + c(0, cos(axis_degrees[side] * pi / 180) *
                                         ticks.length[side]),
                col = ticks.col[sides[side]], lwd = ticks.lwd[sides[side]])
        }
      
        AxisTick(1)
        AxisTick(2)
        AxisTick(3)
      }
      
      if (length(axis.labels) > 1 || axis.labels != FALSE) {
        if (length(axis.labels) == 1) axis.labels <- round(line_points * 100, 1)
        if (length(axis.labels) == grid.lines) axis.labels <- c('', axis.labels)
        if (!clockwise) axis.labels <- rev(axis.labels)
       
        AxisLabel <- function (side) {
          text(line_ends[1, side] + sin(axis_degrees[side] * pi / 180) * 
                 ticks.length[side] * mult[side],
               line_ends[2, side] + cos(axis_degrees[side] * pi / 180) * 
                 ticks.length[side] * mult[side],
               axis.labels[i], srt = rot[side],
               pos = pos[side], font = axis.font[sides[side]],
               cex = axis.cex[sides[side]],
               col = lab.col[sides[side]])
        }
        
        # Annotate axes
        AxisLabel(1)
        AxisLabel(2)
        AxisLabel(3)
      }
    })
  }
  
  # Draw axis lines
  lines(axes[1, 3:4], axes[2, 3:4], col = axis.col[sides[1]], 
        lty = axis.lty[sides[1]], lwd = axis.lwd[sides[1]])
  lines(axes[1, 1:2], axes[2, 1:2], col = axis.col[sides[2]], 
        lty = axis.lty[sides[2]], lwd = axis.lwd[sides[2]])
  lines(axes[1, 2:3], axes[2, 2:3], col = axis.col[sides[3]], 
        lty = axis.lty[sides[3]], lwd = axis.lwd[sides[3]])

  DirectionalOffset <- function (degrees) {
    c(sin(degrees * pi / 180), cos(degrees * pi/ 180))
  }
  
  TitleAxis <- function (xy, lab, side, rot) {
    text(xy[1], xy[2], lab, cex = lab.cex[side], font = lab.font[side],
         srt = rot[direction], col = lab.col[side])
  }
  alab_xy <- TernaryCoords(c(1, 0, 1)) + 
    (lab.offset[1] * DirectionalOffset(c(300,  60, 120, 210)[direction]))
  blab_xy <- TernaryCoords(c(1, 1, 0)) + 
    (lab.offset[2] * DirectionalOffset(c( 60, 120, 210, 330)[direction]))
  clab_xy <- TernaryCoords(c(0, 1, 1)) + 
    (lab.offset[3] * DirectionalOffset(c(180, 270,   0,  90)[direction]))
    
  TitleAxis(alab_xy, if (clockwise) alab else clab, if (clockwise) 1 else 3, 
            c( 60, 330,  60, 330))
  TitleAxis(blab_xy, if (clockwise) blab else alab, if (clockwise) 2 else 1, 
            c(300,  30, 300,  30))
  TitleAxis(clab_xy, if (clockwise) clab else blab, if (clockwise) 3 else 2, 
            c(  0,  90,   0, 270))
  
  axRaw <- if (clockwise) c(-4, 4,  1, -3) else c(4, 4, -1, -3)
  ayRaw <- if (clockwise) c(1, -4, -2, -4) else c(1, -4, -2, 4)
  ax <- axRaw[direction] * ticks.length[1]
  ay <- ayRaw[direction] * ticks.length[1]
  if (is.null(atip.rotate)) {
    atip.rotate <- if (clockwise) c(0, 30, 0, 330)[direction] else c(0, 30, 0, 30)[direction]
    atip.pos <- if (clockwise) c(2, 2, 4, 4)[direction] else c(4, 2, 2, 4)[direction]
  }
  bx <- c(4, 4, -2, -3)[direction] * ticks.length[2]
  by <- c(-4, -2, 4, 2.4)[direction] * ticks.length[2]
  if (is.null(btip.rotate)) {
    btip.rotate <- c(0, 0, 0, 0)[direction]
    btip.pos <- c(2, 4, 4, 2)[direction]
  }
  cx <- c(-3, 0, 2, -3)[direction] * ticks.length[3]
  cy <- c(-4, 2, 4, -2)[direction] * ticks.length[3]
  if (is.null(ctip.rotate)) {
    ctip.rotate <- c(0, 0, 0, 0)[direction]
    ctip.pos <- c(4, 4, 2, 2)[direction]
  }
  
  # Title corners
  text(axes[1, 1] + ax, axes[2, 1] + ay, atip, pos = atip.pos, cex = tip.cex[1],
       font = tip.font[1], col = tip.col[1], srt = atip.rotate)
  text(axes[1, 2] + bx, axes[2, 2] + by, btip, pos = btip.pos, cex = tip.cex[2],
       font = tip.font[2], col = tip.col[2], srt = btip.rotate)
  text(axes[1, 3] + cx, axes[2, 3] + cy, ctip, pos = ctip.pos, cex = tip.cex[3],
       font = tip.font[3], col = tip.col[3], srt = ctip.rotate)
  
  # Return:
  return <- NULL
}

#' @describeIn TernaryPlot Add `grid.lines` horizontal lines to the ternary plot
#' @template directionParam
#' 
#' @importFrom graphics par
#' @export
HorizontalGrid <- function (grid.lines = 10, grid.col='grey',
                            grid.lty='dotted', grid.lwd=par('lwd'),
                            direction=getOption('ternDirection')) {
  
  if (!(direction %in% 1:4)) stop("Parameter direction must be an integer from 1 to 4")
  line_points <- seq(from=0, to=1, length.out=grid.lines + 1L)
  tern_height <- c(sqrt(3/4), 1, sqrt(3/4), 1)[direction]
  tern_width <- c(1, sqrt(3/4), 1, sqrt(3/4), 1)[direction]
  
  
  lapply(line_points[-c(1, grid.lines + 1L)], function (p) {
    x <- tern_width * if (direction == 1) {
      c(-1, 1) * (1 - p) / 2
    } else if (direction == 2) {
      c(0, 0.5 - abs(0.5 - p)) * 2
    } else if (direction == 3) {
      c(-1, 1) * p / 2
    } else if (direction == 4) {
      c(0, -(0.5 - abs(0.5 - p))) * 2
    }
    y <- rep(tern_height, 2) * (p - c(0, 0.5, 1, 0.5)[direction])
    lines(x, y, col=grid.col, lty=grid.lty, lwd=grid.lwd)
  })
  
  # Return:
  return <- NULL
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
AddToTernary <- function (PlottingFunction, coordinates, ...) {
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
CoordinatesToXY <- function (coordinates) {
  dims <- dim(coordinates)
  
  # Return:
  if (is.null(dims)) {
    if (is.list(coordinates)) {
      vapply(coordinates, TernaryCoords, double(2))
    } else if (is.numeric(coordinates)) {
      matrix(TernaryCoords(coordinates), nrow=2)
    }
  } else if (length(dims) == 2) {
    which_dim <- if(dims[2] == 3) 1 else if (dims[1] == 3) 2 else stop("Coordinates must be ternary points")
    apply(coordinates, which_dim, TernaryCoords)
  } else {
    stop("Unrecognized format for coordinates parameter.")
  }
}

#' @describeIn AddToTernary Add  \link[graphics]{arrows}
#' @importFrom graphics arrows
#' @export
TernaryArrows <- function (fromCoordinates, toCoordinates=fromCoordinates, ...) {
  fromXY <- CoordinatesToXY(fromCoordinates)
  toXY <- CoordinatesToXY(toCoordinates)
  
  # Return:
  arrows(fromXY[1L, ], fromXY[2L, ], toXY[1L, ], toXY[2L, ], ...)
}

#' @describeIn AddToTernary Add \link[graphics]{lines}
#' @importFrom graphics lines
#' @export
TernaryLines <- function (coordinates, ...) AddToTernary(lines, coordinates, ...)

#' @describeIn AddToTernary Add \link[graphics]{points}
#' @importFrom graphics points
#' @export
TernaryPoints <- function (coordinates, ...) AddToTernary(points, coordinates, ...)

#' @describeIn AddToTernary Add \link[graphics:polygon]{polygons}
#' @importFrom graphics polygon
#' @export
TernaryPolygon <- function (coordinates, ...) AddToTernary(polygon, coordinates, ...)

#' @describeIn AddToTernary Add \link[graphics]{text}
#' @importFrom graphics text
#' @export
TernaryText <- function (coordinates, ...) AddToTernary(text, coordinates, ...)

#' @describeIn AddToTernary Add points, joined by lines
#' @importFrom graphics lines points
#' @export
JoinTheDots <- function(coordinates, ...) {
  AddToTernary(points, coordinates, ...)
  AddToTernary(lines, coordinates, ...)
}
