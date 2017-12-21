#' XY to Ternary
#' 
#' @param abc A vector of length three giving the position on a ternary plot that points
#'            in the direction specified by `direction` (1 = up, 2 = right, 3 = down, 4 = left).
#'            \code{c(100, 0, 0)} will plot in the `direction`-most corner; \code{c(0, 100, 0)} 
#'            will plot in the corner clockwise of `direction`; \code{c(0, 0, 100)} will plot in
#'            the corner anti-clockwise of `direction`.
#'            Alternatively, the a coordinate can be specified as the first parameter,
#'            in which case the b and c coordinates must be specified via \code{b_coord}
#'            and \code{c_coord}.
#' @param b_coord The b coordinate, if \code{abc} is a single number.
#' @param c_coord The c coordinate, if \code{abc} is a single number.
#' @template directionParam
#'            
#' @return A vector of length two that converts the coordinates given in \code{abc}
#'         into cartesian (x, y) coordinates corresponding to the plot created by
#'         the last call of \code{\link{TernaryPlot}}
#'
#' @author Martin R. Smith
#' @export
TernaryCoords <- function (abc, b_coord=NULL, c_coord=NULL, direction=getOption('ternDirection')) {
  if (!is.null(b_coord) && !is.null(c_coord)) {
    abc <- c(abc, b_coord, c_coord)
  }
  if (length(abc) != 3) stop("Parameter abc must be a vector of length three.")
  if (mode(abc) != 'numeric') stop("Parameter abc must be numeric.")
  if (!(direction %in% 1:4)) stop  ("Parameter direction must be 1, 2, 3 or 4")
  
  abc <- abc[if (direction == 1L) c(2, 3, 1) else
             if (direction == 2L) c(3, 2, 1) else 
             if (direction == 3L) c(3, 2, 1) else
             if (direction == 4L) c(2, 3, 1)]
               
  x_deviation <- abc[3] / sum(abc)
  if (x_deviation == 1) {
      x <- cos(pi/6)
      y <- 0
  } else {
    y_deviation <- (abc[1] - abc[2]) / sum(abc[1:2])
    x <- x_deviation * cos(pi/6)
    y <- y_deviation * (1 - x_deviation) / 2
  }
  ret <- if (direction == 1L) c(y, x) else 
         if (direction == 2L) c(x, y) else 
         if (direction == 3L) c(y, -x) else
         if (direction == 4L) c(-x, y)
  
  # Return:
  ret
}

#' X and Y coordinates of ternary plotting area
#'
#' @template directionParam
#'
#' @return Returns the minimum and maximum X coordinate for a ternary plot, 
#' oriented in the specified direction.
#' 
#' @author Martin R. Smith
#' @export
TernaryXRange <- function (direction = getOption('ternDirection')) {
  if (is.na(direction) || !(direction %in% 1:4)) stop("Invalid ternary orientation")
  if (direction == 2L) {
    c(0, 1) - ((1 - sqrt(0.75)) / 2) # Range should equal Y range. Centre plot.
  } else if (direction == 4L) {
    c(-1, 0) + ((1 - sqrt(0.75)) / 2) # Range should equal Y range. Centre plot.
  } else {
    c(-0.5, 0.5)
  }
}

#' @describeIn TernaryXRange Returns the minimum and maximum Y coordinate for a ternary plot in the specified direction.
#' @export
TernaryYRange <- function (direction = getOption('ternDirection')) {
  if (is.na(direction) || !(direction %in% 1:4)) stop("Invalid ternary orientation")
  if (direction == 1L) {
    c(0, 1) - ((1 - sqrt(0.75)) / 2) # Range should equal X range. Centre plot.
  } else if (direction == 3L) {
    c(-1, 0) + ((1 - sqrt(0.75)) / 2) # Range should equal X range. Centre plot.
  } else {
    c(-0.5, +0.5)
  } 
}

#' Create a ternary plot
#' 
#' Create and style a blank ternary plot.
#' 
#' The plot will be generated using the standard graphics plot functions, on which
#' additional elements can be added using cartesian coordinates, perhaps using
#' functions such as [arrows](arrows), [legend] or [text].
#' 
#' @param atip,btip,ctip Character specifying text to title corners, proceeding clockwise
#'                       from the corner specified in `point` (default: top).
#' @param alab,blab,clab Character specifying text with which to label the corresponding 
#'                       sides of the triangle.  Left or right-pointing arrows are produced by
#'                       typing `\\U2190` or `\\U2192`.
#' @param lab.offset Numeric specifying distance between midpoint of axis label and the axis.
#'                   Increase `padding` if labels are being clipped.
#'                      
#' @param point Character specifying the orientation of the ternary plot: should the
#'              triangle point up, left, right or down?
#' @param xlim,ylim Numeric vectors of length 2 specifying the minimum and maximum
#'                  _x_ and _y_ limits of the plotted area, to which \code{padding}
#'                  will be added.
#' @param lab.cex,tip.cex Numeric specifying character expansion for axis titles.
#' @param lab.font,tip.font Numeric specifying font (roman, bold, italic, bold-italic) for axis titles.
#' @param alab.rotate,blab.rotate,clab.rotate Integer specifying number of
#'          degrees to rotate label of rightmost apex.
#' @param alab.pos,blab.pos,clab.pos Integer specifying positioning of labels,
#'          iff corresponding `xlab.rotate` parameter is set.
#' 
#' @param isometric Logical specifying whether to enforce an equilateral 
#'                  shape for the ternary plot.
#' @param padding Numeric specifying size of internal margin of the plot; increase
#'                if axis labels are being clipped.
#' @param col The colour for filling the plot; see \code{[graphics:polygon]}.
#' 
#' @param grid.lines The number of grid lines to plot.
#' @param grid.col The colour to draw the grid lines.
#' @param grid.lty Character or (integer) numeric; line type of the grid lines.
#' @param grid.lwd Non-negative numeric giving line width of the grid lines.
#' 
#' @param axis.lty  Line type for both the axis line and tick marks
#' @param axis.labels This can either be a logical value specifying whether 
#'                    (numerical) annotations are to be made at the tickmarks,
#'                     or a character or expression vector of labels to be
#'                     placed at the tickpoints.
#' @param axis.cex Numeric specifying character expansion for axis labels.
#' @param axis.font Font for text. Defaults to \code{par('font')}.
#' @param axis.tick Logical specifying whether to mark the axes with tick marks.
#' @param axis.lwd,ticks.lwd Line width for the axis line and tick marks. 
#'                 Zero or negative values will suppress the line or ticks.
#' @param axis.col,ticks.col Colours for the axis line and tick marks respectively. 
#'        \code{axis.col = NULL} means to use \code{par('fg')}, possibly specified 
#'        inline, and \code{ticks.col = NULL} means to use whatever colour
#'        \code{axis.col} resolved to.
#' 
#' 
#' @param \dots Additional parameters to \code{[graphics:plot]}.
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
                         point='up', xlim=NULL, ylim=NULL,
                         lab.cex=1.0, lab.font=0, tip.cex=lab.cex, tip.font=2,
                         isometric=TRUE, 
                         atip.rotate = NULL, btip.rotate = NULL, ctip.rotate = NULL,
                         atip.pos = NULL, btip.pos = NULL, ctip.pos = NULL,
                         padding = 0.08,
                         col=NA, 
                         grid.lines=10, grid.col='grey',
                         grid.lty='dotted', grid.lwd=par('lwd'),
                         axis.lty='solid', 
                         axis.labels=TRUE, axis.cex=0.8, 
                         axis.font=par('font'),
                         axis.tick=TRUE,
                         axis.lwd=1, ticks.lwd=axis.lwd,
                         axis.col='black', ticks.col=grid.col,
                         ...) {
  direction <- 1 + (pmatch(tolower(point), c('right', 'down', 'left', 'up', 'east', 'south', 'west', 'north', 2, 3, 4, 1)) %% 4)
  if (is.na(direction)) {
    warning("Point must be one of up, down, left or right")
  } else {
    options('ternDirection' = direction)
  }
  
  tick_length <- 0.025
  
  if (isometric) {
    original_par <- par(pty='s')
    on.exit(par(original_par))
  }
  padVec <- c(-1, 1) * padding
  if (is.null(xlim)) xlim <- TernaryXRange(direction)
  if (is.null(ylim)) ylim <- TernaryYRange(direction)
  
  
  plot(-999, -999, axes=FALSE, xlab='', ylab='',
       xlim=xlim + padVec, ylim=ylim + padVec, ...)
  axes <- vapply(list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 0, 0)),
                 TernaryCoords, double(2))
  polygon(axes[1, ], axes[2, ], col=col, border=NA)
  
  if (!is.integer(grid.lines)) grid.lines <- ceiling(grid.lines)
  if (!is.null(grid.lines) && !is.na(grid.lines) && grid.lines > 1L) {
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
        lines(line_ends[1, 1] + c(0, sin(axis1_degrees * pi / 180) * tick_length),
              line_ends[2, 1] + c(0, cos(axis1_degrees * pi / 180) * tick_length),
              col=ticks.col, lwd=ticks.lwd)
    
        lines(line_ends[1, 2] + c(0, sin(axis2_degrees * pi / 180) * tick_length),
              line_ends[2, 2] + c(0, cos(axis2_degrees * pi / 180) * tick_length),
              col=ticks.col, lwd=ticks.lwd)
       
        lines(line_ends[1, 3] + c(0, sin(axis3_degrees * pi / 180) * tick_length),
              line_ends[2, 3] + c(0, cos(axis3_degrees * pi / 180) * tick_length),
              col=ticks.col, lwd=ticks.lwd)
      }
      
      if (length(axis.labels) > 1 || axis.labels != FALSE) {
        if (length(axis.labels) == 1) axis.labels <- round(line_points * 100, 1)
        if (length(axis.labels) == grid.lines) axis.labels <- c('', axis.labels)
        
        # Annotate axes
        text(line_ends[1, 1] + sin(axis1_degrees * pi / 180) * tick_length * mult1,
             line_ends[2, 1] + cos(axis1_degrees * pi / 180) * tick_length * mult1,
             axis.labels[i], srt=rot1, pos=pos1, font=axis.font, cex=axis.cex)
        text(line_ends[1, 2] + sin(axis2_degrees * pi / 180) * tick_length * mult2,
             line_ends[2, 2] + cos(axis2_degrees * pi / 180) * tick_length * mult2,
             axis.labels[i], srt=rot2, pos=pos2, font=axis.font, cex=axis.cex)
        text(line_ends[1, 3] + sin(axis3_degrees * pi / 180) * tick_length * mult3,
             line_ends[2, 3] + cos(axis3_degrees * pi / 180) * tick_length * mult3,
             axis.labels[i], srt=rot3, pos=pos3, font=axis.font, cex=axis.cex)
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
    ax <- c(-4, 4,  1, -3)[direction] * tick_length
    ay <- c(1, -4, -2, -4)[direction] * tick_length
    atip.rotate = c(0, 30, 0, 330)[direction]
    atip.pos = c(2, 2, 4, 4)[direction]
  }
  if (is.null(btip.rotate)) {
    bx <- c(4, 4, -2, -3)[direction] * tick_length
    by <- c(-4, -2, 4, 2.4)[direction] * tick_length
    btip.rotate = c(0, 0, 0, 0)[direction]
    btip.pos = c(2, 4, 4, 2)[direction]
  }
  if (is.null(ctip.rotate)) {
    cx <- c(-3, 0, 2, -3)[direction] * tick_length
    cy <- c(-4, 2, 4, -2)[direction] * tick_length
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
#' @keywords internal
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
#' Plot points onto a ternary diagram created with \code{\link{TernaryPlot}}.
#' 
#' @param PlottingFunction Function to add data to a plot; perhaps one of
#'        \code{\link[graphics]{points}},
#'        \code{\link[graphics]{lines}} or
#'        \code{\link[graphics]{text}}.
#' @param coordinates A list, matrix, data.frame or vector in which each
#'                    element (or row) specifies
#'                    the three coordinates of a point in ternary space.
#' @param \dots Additional parameters to pass to \code{PlottingFunction}.
#' 
#' @examples {
#'   coords <- list(
#'     A = c(1, 0, 2),
#'     B = c(1, 1, 1),
#'     C = c(1.5, 1.5, 0),
#'     D = c(0.5, 1.5, 1)
#'   )
#'   TernaryPlot()
#'   AddToTernary(lines, coords, col='green', lwd=2)
#'   TernaryLines(coords, col='red', lty='dotted')
#'   TernaryText(coords, cex=0.7, col='red')
#'   TernaryPoints(coords, pch=1, cex=2, col='blue')
#'   AddToTernary(points, coords, pch=1, cex=3)
#' }
#' 
#' @author Martin R. Smith
#' @export
AddToTernary <- function (PlottingFunction, coordinates, ...) {
  dims <- dim(coordinates)
  if (is.null(dims)) {
    if (mode(coordinates) == 'list') {
      xy <- vapply(coordinates, TernaryCoords, double(2))
      return(PlottingFunction(xy[1, ], xy[2, ], ...))
    } else if (mode(coordinates) == 'numeric') {
      xy <- TernaryCoords(coordinates)
      return(PlottingFunction(xy[1], xy[2], ...))
    }
  } else if (length(dims) == 2) {
    which_dim <- if(dims[2] == 3) 1 else if (dims[1] == 3) 2 else stop("Coordinates must be ternary points")
    xy <- apply(coordinates, which_dim, TernaryCoords)
    return(PlottingFunction(xy[1, ], xy[2, ], ...))
  } else {
    stop("Unrecognized format for coordinates parameter.") 
  }
}

#' @describeIn AddToTernary Add points
#' @importFrom graphics points
#' @export
TernaryPoints <- function (coordinates, ...) AddToTernary(points, coordinates, ...)

#' @describeIn AddToTernary Add points
#' @importFrom graphics text
#' @export
TernaryText <- function (coordinates, ...) AddToTernary(text, coordinates, ...)

#' @describeIn AddToTernary Add points
#' @importFrom graphics lines
#' @export
TernaryLines <- function (coordinates, ...) AddToTernary(lines, coordinates, ...)

#' @describeIn AddToTernary Add points
#' @importFrom graphics polygon
#' @export
TernaryPolygon <- function (coordinates, ...) AddToTernary(polygon, coordinates, ...)

#' @describeIn AddToTernary Add points, joined by lines
#' @importFrom graphics lines points
#' @export
JoinTheDots <- function(coordinates, ...) {
  AddToTernary(points, coordinates, ...)
  AddToTernary(lines, coordinates, ...)
}
