#' Create a ternary plot
#' 
#' Create and style a blank ternary plot.
#' 
#' The plot will be generated using the standard graphics plot functions, on which
#' additional elements can be added using cartesian coordinates, perhaps using
#' functions such as \code{\link[graphics]{arrows}}, 
#' \code{\link[graphics]{legend}} or \code{\link[graphics]{text}}.
#' 
#' @param atip,btip,ctip Character string specifying text to title corners, 
#' proceeding clockwise from the corner specified in `point` (default: top).
#' @param alab,blab,clab Character string specifying text with which to label 
#' the corresponding sides of the triangle.  
#' Left or right-pointing arrows are produced by
#'  typing `\\U2190` or `\\U2192`, or using `expression('value' %->% '')`.
#' @param lab.offset Numeric specifying distance between midpoint of axis label and the axis.
#'  Increase `padding` if labels are being clipped.
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
#' @param lab.cex,tip.cex Numeric specifying character expansion for axis titles.
#' @param lab.font,tip.font Numeric specifying font (roman, bold, italic, bold-italic) for axis titles.
#' @param atip.rotate,btip.rotate,ctip.rotate Integer specifying number of
#'  degrees to rotate label of rightmost apex.
#' @param atip.pos,btip.pos,ctip.pos Integer specifying positioning of labels,
#'  iff the corresponding `xlab.rotate` parameter is set.
#' 
#' @param isometric Logical specifying whether to enforce an equilateral shape
#'  for the ternary plot.
#'  If only one of `xlim` and `ylim` is set, the other will be calculated to maintain
#'  an equilateral plot.
#'  If both `xlim` and `ylim` are set, but have different ranges, then the limit with the
#'  smaller range will be scaled until its range matches that of the other limit.
#' @param padding Numeric specifying size of internal margin of the plot; increase
#'  if axis labels are being clipped.
#' @param col The colour for filling the plot; see \code{\link[graphics]{polygon}}.
#' 
#' @param grid.lines Integer specifying the number of grid lines to plot.
#' @param grid.minor.lines Integer specifying the number of minor (unlabelled) 
#'  grid lines to plot between each major pair.
#' @param grid.col,grid.minor.col The colour to draw the grid lines.
#' @param grid.lty,grid.minor.lty Character or integer; line type of 
#'  the grid lines.
#' @param grid.lwd,grid.minor.lwd Non-negative numeric giving line width of the
#'  grid lines.
#' 
#' @param axis.lty  Line type for both the axis line and tick marks.
#' @param axis.labels This can either be a logical value specifying whether 
#'  (numerical) annotations are to be made at the tickmarks, or a character or
#'  expression vector of labels to be placed at the tick points.
#' @param axis.cex Numeric specifying character expansion for axis labels.
#' @param axis.font Font for text. Defaults to \code{par('font')}.
#' @param axis.tick Logical specifying whether to mark the axes with tick marks.
#' @param axis.lwd,ticks.lwd Line width for the axis line and tick marks. 
#'  Zero or negative values will suppress the line or ticks.
#' @param ticks.length Numeric specifying distance that ticks should extend
#'  beyond the plot margin.  Also affects position of axis labels, which are
#'  plotted at the end of each tick.
#' @param axis.col,ticks.col,axis.labels.col Colours for the axis line, tick
#'  marks and labels, respectively. 
#'  \code{axis.col = NULL} means to use \code{par('fg')}, possibly specified 
#'  inline, and \code{ticks.col = NULL} means to use whatever colour
#'  \code{axis.col} resolved to.
#' 
#' 
#' @param \dots Additional parameters to \code{\link[graphics]{plot}}.
#' 
#' @seealso {
#'  - [AddToTernary]: Add elements to a ternary plot
#'  - [TernaryCoords]: Convert ternary coordinates to Cartesian (_x_ and _y_) coordinates
#'  - [TernaryXRange], [TernaryYRange]: What are the _x_ and _y_ limits of the plotted region?
#' }
#' 
#' @examples {
#' TernaryPlot(atip="Top", btip="Bottom", ctip="Right", axis.col="red", col=rgb(0.8, 0.8, 0.8))
#' HorizontalGrid(grid.lines=2, grid.col='blue', grid.lty=1) # the second line corresponds to
#'                                               # the base of the triangle, and is not drawn
#' }
#' 
#' @author Martin R. Smith
#' 
#' @importFrom graphics par plot polygon
#' 
#' @export
TernaryPlot <- function (atip=NULL, btip=NULL, ctip=NULL,
                         alab=NULL, blab=NULL, clab=NULL, lab.offset=0.16,
                         point='up', clockwise=TRUE, xlim=NULL, ylim=NULL,
                         lab.cex=1.0, lab.font=0, tip.cex=lab.cex, tip.font=2,
                         isometric=TRUE, 
                         atip.rotate = NULL, btip.rotate = NULL, ctip.rotate = NULL,
                         atip.pos = NULL, btip.pos = NULL, ctip.pos = NULL,
                         padding = 0.08,
                         col=NA, 
                         grid.lines=10, grid.col='darkgrey',
                         grid.lty='solid', grid.lwd=par('lwd'),
                         grid.minor.lines=4, grid.minor.col='lightgrey',
                         grid.minor.lty='solid', grid.minor.lwd=par('lwd'),
                         axis.lty='solid',
                         axis.labels=TRUE, axis.cex=0.8, 
                         axis.font=par('font'),
                         axis.tick=TRUE,
                         axis.lwd=1, ticks.lwd=axis.lwd, ticks.length=0.025,
                         axis.col='black', ticks.col=grid.col,
                         axis.labels.col=axis.col,
                         ...) {
  direction <- 1L + (pmatch(tolower(point), c('right', 'down', 'left', 'up', 'east', 'south', 'west', 'north', 2L, 3L, 4L, 1L)) %% 4L)
  if (is.na(direction)) {
    warning("Point must be one of up, down, left or right")
  } else {
    options('ternDirection' = direction)
  }
  
  if (isometric) {
    original_par <- par(pty='s')
    on.exit(par(original_par))
    
    if (is.null(xlim) && !is.null(ylim)) xlim <- TernaryXRange(direction) * (ylim[2] - ylim[1])
    xRange <- xlim[2] - xlim[1]
    if (is.null(ylim) && !is.null(xlim)) ylim <- TernaryYRange(direction) * xRange
    yRange <- ylim[2] - ylim[1]
    
    if (length(xlim) > 0 && abs(xRange - yRange) > 1e-07) {
      if (abs(xRange) < abs(yRange)) {
        xlim <- xlim * (yRange / xRange)
        warning("x range < y range, but isometric = TRUE; setting xlim = c(", xlim[1], ', ', xlim[2], ")")
      } else {
        ylim <- ylim * (xRange / yRange)
        warning("x range > y range, but isometric = TRUE; setting ylim = c(", ylim[1], ', ', ylim[2], ")")
      }
    }
  }
  if (is.null(xlim)) xlim <- TernaryXRange(direction)
  if (is.null(ylim)) ylim <- TernaryYRange(direction)
  padVec <- c(-1, 1) * padding
  
  
  plot(-999, -999, axes=FALSE, xlab='', ylab='',
       xlim=xlim + padVec, ylim=ylim + padVec, ...)
  axes <- vapply(list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 0, 0)),
                 TernaryCoords, double(2))
  polygon(axes[1, ], axes[2, ], col=col, border=NA)
  
  if (!is.integer(grid.lines)) grid.lines <- ceiling(grid.lines)
  if (!is.integer(grid.minor.lines)) grid.minor.lines <- ceiling(grid.minor.lines)
  if (!is.null(grid.lines) && !is.na(grid.lines) && grid.lines > 1L) {
    # Plot minor grid lines
    if (grid.minor.lines > 0L) {
      n_minor_lines <- grid.lines * (grid.minor.lines + 1L)  + 1L
      minor_line_points <- seq(from=0, to=1, length.out=n_minor_lines)[-seq(from=1, to=n_minor_lines, by=grid.minor.lines + 1L)]
      lapply(minor_line_points, function (p) {
        q <- 1 - p
        line_ends <- vapply(list(c(p, q, 0), c(p, 0, q),
                                 c(0, p, q), c(q, p, 0),
                                 c(q, 0, p), c(0, q, p)),
                            TernaryCoords, double(2))
        lapply(list(c(1, 2), c(3, 4), c(5, 6)), function (i) 
          lines(line_ends[1, i], line_ends[2, i], col=grid.minor.col,
                lty=grid.minor.lty, lwd=grid.minor.lwd))
        NULL
      })
      
    }
    
    # Plot grid
    line_points <- seq(from=0, to=1, length.out=grid.lines + 1L)
    
    lapply(line_points[-c(1, grid.lines + 1L)], function (p) {
      q <- 1 - p
      line_ends <- vapply(list(c(p, q, 0), c(p, 0, q),
                               c(0, p, q), c(q, p, 0),
                               c(q, 0, p), c(0, q, p)),
                          TernaryCoords, double(2))
      lapply(list(c(1, 2), c(3, 4), c(5, 6)), function (i) 
      lines(line_ends[1, i], line_ends[2, i], col=grid.col, lty=grid.lty, lwd=grid.lwd))
      NULL
    })
    
    axis1_degrees <- (180 + (direction * 90)) %% 360
    axis2_degrees <- (300 + (direction * 90)) %% 360
    axis3_degrees <- ( 60 + (direction * 90)) %% 360
    
    rot1 <- c(  0, 270,   0,  90)[direction]
    rot2 <- c( 60, -30,  60, -30)[direction]
    rot3 <- c(-60,  30, -60,  30)[direction]
    
    pos1 <- c(2, 2, 4, 2)[direction]
    pos2 <- c(4, 4, 2, 2)[direction]
    pos3 <- c(4, 2, 2, 4)[direction]
    
    mult1 <- c(5 , 16, 10, 12)[direction] / 10
    mult2 <- c(5 ,  9,  8,  9)[direction] / 10
    mult3 <- c(16,  8, 16,  8)[direction] / 10
    
    # Plot and annotate axes
    lapply(seq_along(line_points), function (i) {
      p <- line_points[i]
      q <- 1 - p
      line_ends <- vapply(list(c(p, 0, q),
                               c(q, p, 0),
                               c(0, q, p)),
                          TernaryCoords, double(2))
                         
      if (axis.tick) {
        lines(line_ends[1, 1] + c(0, sin(axis1_degrees * pi / 180) * ticks.length),
              line_ends[2, 1] + c(0, cos(axis1_degrees * pi / 180) * ticks.length),
              col=ticks.col, lwd=ticks.lwd)
    
        lines(line_ends[1, 2] + c(0, sin(axis2_degrees * pi / 180) * ticks.length),
              line_ends[2, 2] + c(0, cos(axis2_degrees * pi / 180) * ticks.length),
              col=ticks.col, lwd=ticks.lwd)
       
        lines(line_ends[1, 3] + c(0, sin(axis3_degrees * pi / 180) * ticks.length),
              line_ends[2, 3] + c(0, cos(axis3_degrees * pi / 180) * ticks.length),
              col=ticks.col, lwd=ticks.lwd)
      }
      
      if (length(axis.labels) > 1 || axis.labels != FALSE) {
        if (length(axis.labels) == 1) axis.labels <- round(line_points * 100, 1)
        if (length(axis.labels) == grid.lines) axis.labels <- c('', axis.labels)
        
        # Annotate axes
        text(line_ends[1, 1] + sin(axis1_degrees * pi / 180) * ticks.length * mult1,
             line_ends[2, 1] + cos(axis1_degrees * pi / 180) * ticks.length * mult1,
             axis.labels[i], srt=rot1, pos=pos1, font=axis.font, cex=axis.cex,
             col=axis.labels.col)
        text(line_ends[1, 2] + sin(axis2_degrees * pi / 180) * ticks.length * mult2,
             line_ends[2, 2] + cos(axis2_degrees * pi / 180) * ticks.length * mult2,
             axis.labels[i], srt=rot2, pos=pos2, font=axis.font, cex=axis.cex,
             col=axis.labels.col)
        text(line_ends[1, 3] + sin(axis3_degrees * pi / 180) * ticks.length * mult3,
             line_ends[2, 3] + cos(axis3_degrees * pi / 180) * ticks.length * mult3,
             axis.labels[i], srt=rot3, pos=pos3, font=axis.font, cex=axis.cex,
             col=axis.labels.col)
      }
    })
  }
  
  # Draw axis lines
  lines(axes[1, ], axes[2, ], col=axis.col, lty=axis.lty, lwd=axis.lwd)

  DirectionalOffset <- function (degrees) {
    c(sin(degrees * pi / 180), cos(degrees * pi/ 180))
  }
  alab_xy <- TernaryCoords(c(1, 0, 1)) + (lab.offset * DirectionalOffset(c(300,  60, 120, 210)[direction]))
  blab_xy <- TernaryCoords(c(1, 1, 0)) + (lab.offset * DirectionalOffset(c( 60, 120, 210, 330)[direction]))
  clab_xy <- TernaryCoords(c(0, 1, 1)) + (lab.offset * DirectionalOffset(c(180, 270,   0,  90)[direction]))
    
  # Title axes
  text(alab_xy[1], alab_xy[2], alab, cex=lab.cex, font=lab.font, srt=c( 60, 330,  60, 330)[direction])
  text(blab_xy[1], blab_xy[2], blab, cex=lab.cex, font=lab.font, srt=c(300,  30, 300,  30)[direction])
  text(clab_xy[1], clab_xy[2], clab, cex=lab.cex, font=lab.font, srt=c(  0,  90,   0, 270)[direction])
  
  
  if (is.null(atip.rotate)) {
    ax <- c(-4, 4,  1, -3)[direction] * ticks.length
    ay <- c(1, -4, -2, -4)[direction] * ticks.length
    atip.rotate = c(0, 30, 0, 330)[direction]
    atip.pos = c(2, 2, 4, 4)[direction]
  }
  if (is.null(btip.rotate)) {
    bx <- c(4, 4, -2, -3)[direction] * ticks.length
    by <- c(-4, -2, 4, 2.4)[direction] * ticks.length
    btip.rotate = c(0, 0, 0, 0)[direction]
    btip.pos = c(2, 4, 4, 2)[direction]
  }
  if (is.null(ctip.rotate)) {
    cx <- c(-3, 0, 2, -3)[direction] * ticks.length
    cy <- c(-4, 2, 4, -2)[direction] * ticks.length
    ctip.rotate = c(0, 0, 0, 0)[direction]
    ctip.pos = c(4, 4, 2, 2)[direction]
  }
  
  # Title corners
  text(axes[1, 1] + ax, axes[2, 1] + ay, atip, pos=atip.pos, cex=tip.cex, font=tip.font, srt=atip.rotate)
  text(axes[1, 2] + bx, axes[2, 2] + by, btip, pos=btip.pos, cex=tip.cex, font=tip.font, srt=btip.rotate)
  text(axes[1, 3] + cx, axes[2, 3] + cy, ctip, pos=ctip.pos, cex=tip.cex, font=tip.font, srt=ctip.rotate)
  
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
#' Plot shapes onto a ternary diagram created with \code{\link{TernaryPlot}}.
#' 
#' @param PlottingFunction Function to add data to a plot; perhaps one of
#'        \code{\link[graphics]{points}},
#'        \code{\link[graphics]{lines}} or
#'        \code{\link[graphics]{text}}.
#' @template coordinatesParam
#' @param fromCoordinates,toCoordinates For `TernaryArrows`, coordinates at 
#' which arrows should begin and end; cf. `x0`, `y0`, `x1` and `y1` in 
#' \link[graphics]{arrows}.  Recycled as necessary.
#' @param \dots Additional parameters to pass to \code{PlottingFunction}.  
#' If using `TernaryText`, this will likely include the parameter `labels`,
#' to specify the text to plot.
#' 
#' @examples {
#'   coords <- list(
#'     A = c(1, 0, 2),
#'     B = c(1, 1, 1),
#'     C = c(1.5, 1.5, 0),
#'     D = c(0.5, 1.5, 1)
#'   )
#'   TernaryPlot()
#'   AddToTernary(lines, coords, col='darkgreen', lty='dotted', lwd=3)
#'   TernaryLines(coords, col='darkgreen')
#'   TernaryArrows(coords[1], coords[2:4], col='orange', length=0.2, lwd=1)
#'   TernaryText(coords, cex=0.8, col='red', font=2)
#'   TernaryPoints(coords, pch=1, cex=2, col='blue')
#'   AddToTernary(points, coords, pch=1, cex=3)
#' }
#' 
#' @author Martin R. Smith
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
#' @return An array of two rows, corresponding to the X and Y coordinates of 
#' `coordinates`.
#' 
#' @export
#' @keywords internal
#' @author Martin R. Smith
CoordinatesToXY <- function (coordinates) {
  dims <- dim(coordinates)
  
  # Return:
  if (is.null(dims)) {
    if (mode(coordinates) == 'list') {
      vapply(coordinates, TernaryCoords, double(2))
    } else if (mode(coordinates) == 'numeric') {
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
