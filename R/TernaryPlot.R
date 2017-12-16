#' XY to Ternary
#' 
#' @param abc A vector of length three giving the position on a ternary plot.
#'            \code{c(100, 0, 0)} will plot in the top corner; \code{c(0, 100, 0)} 
#'            will plot in the bottom corner; \code{c(0, 0, 1)} will plot in
#'            the right-hand corner.
#'            Alternatively, the a coordinate can be specified as the first parameter,
#'            in which case the b and c coordinates must be specified via \code{b_coord}
#'            and \code{c_coord}
#' @param b_coord The b coordinate, if \code{abc} is a single number.
#' @param c_coord The c coordinate, if \code{abc} is a single number.
#'            
#' @return A vector of length two that converts the coordinates given in \code{abc}
#'         into cartesian (x, y) coordinates, to be overlaid on a ternary plot
#'         with corners at (0, -1/2), (0, 1/2), (sin(pi/3), 0)
#'
#' @author Martin R. Smith
#' @export
TernaryCoords <- function (abc, b_coord=NULL, c_coord=NULL) {
  if (!is.null(b_coord) && !is.null(c_coord)) {
    abc <- c(abc, b_coord, c_coord)
  }
  if (length(abc) != 3) stop("Parameter abc must be a vector of length three.")
  if (mode(abc) != 'numeric') stop("Parameter abc must be numeric.")
  
  x_deviation <- abc[3] / sum(abc)
  if (x_deviation == 1) return(c(cos(pi/6), 0))
  y_deviation <- (abc[1] - abc[2]) / sum(abc[1:2])
  
  x <- x_deviation * cos(pi/6)
  y <- y_deviation * (1 - x_deviation) / 2
  
  # Return:
  c(x, y)
}

#' Ternary Plot
#' 
#' Create a blank ternary plot, rotated so that its left edge is vertical.
#' 
#' @param alab,blab,clab Character specifying the title for the topmost,
#'                       bottommost and leftmost corners respectively.
#' @param xlim,ylim Numeric vectors of length 2 specifying the minimum and maximum
#'                  _x_ and _y_ limits of the plotted area, to which \code{padding}
#'                  will be added.
#' @param lab.cex Numeric specifying character expansion for axis titles.
#' @param lab.font Numeric specifying font (roman, bold, italic, bold-italic) for axis titles.
#' @param clab.rotate Numeric specifying number of degrees to rotate label of rightmost 
#'                    apex.  Set to 270 if \code{clab.rotate = TRUE}.
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
#' TernaryPlot(alab="Top", blab="Bottom", clab="Right", border="red", col=rgb(0.8, 0.8, 0.8))
#' }
#' 
#' @author Martin R. Smith
#' 
#' @importFrom graphics par plot polygon
#' 
#' @export
TernaryPlot <- function (alab=NULL, blab=NULL, clab=NULL,
                         xlim=NULL, ylim=NULL,
                         lab.cex=1.0, lab.font=2, isometric=TRUE, 
                         clab.rotate = FALSE,
                         padding = 0.04,
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
  tick_length <- 0.025
  
  if (isometric) {
    original_par <- par(pty='s')
    on.exit(par(original_par))
  }
  padVec <- c(-1, 1) * padding
  if (is.null(xlim)) xlim <- c(0, 1) # Not sqrt(0.75) as we want the triangle equilateral
  if (is.null(ylim)) ylim <- c(-0.5, +0.5)
  
  
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
    
    # Plot and annotate axes
    lapply(seq_along(line_points), function (i) {
      p <- line_points[i]
      q <- 1 - p
      line_ends <- vapply(list(c(p, 0, q),
                               c(q, p, 0),
                               c(0, q, p)),
                          TernaryCoords, double(2))
      
      
      if (axis.tick) {
        lines(line_ends[1, 1] + c(0, sin(pi/3) * tick_length),
              line_ends[2, 1] + c(0, cos(pi/3) * tick_length),
              col=ticks.col, lwd=ticks.lwd)
      
        lines(line_ends[1, 2] - c(0, sin(pi/3) * tick_length),
              line_ends[2, 2] + c(0, cos(pi/3) * tick_length),
              col=ticks.col, lwd=ticks.lwd)
       
        lines(line_ends[1, 3] + c(0, 0),
              line_ends[2, 3] - c(0, tick_length),
              col=ticks.col, lwd=ticks.lwd)
      }
      
      if (length(axis.labels) > 1 || axis.labels != FALSE) {
        if (length(axis.labels) == 1) axis.labels <- round(line_points * 100, 1)
        if (length(axis.labels) == grid.lines) axis.labels <- c('', axis.labels)
        # Annotate axes
        text(line_ends[1, 1] + sin(pi/3) * tick_length - 0.06,
             line_ends[2, 1] + cos(pi/3) * tick_length + 0.015,
             axis.labels[i], srt=30, pos=4, font=axis.font, cex=axis.cex)
        text(line_ends[1, 2] - sin(pi/3) * tick_length + 0.03,
             line_ends[2, 2] + cos(pi/3) * tick_length - 0.03,
             axis.labels[i], srt=330, pos=2, font=axis.font, cex=axis.cex)
        text(line_ends[1, 3],
             line_ends[2, 3] - tick_length + 0.019,
             axis.labels[i], srt=270, pos=4, font=axis.font, cex=axis.cex)
      }
      
    })
    
  }
  
  # Draw axis lines
  lines(axes[1, ], axes[2, ], col=axis.col, lty=axis.lty, lwd=axis.lwd)
  
  # Title corners
  text(0 + tick_length, 0.5 + (tick_length * 2), alab, pos=4, cex=lab.cex, font=lab.font)
  text(0 + tick_length, -(0.5 + (tick_length * 2)), blab, pos=4, cex=lab.cex, font=lab.font)
  if (!is.null(clab)) {
    if (clab.rotate) {
      if (clab.rotate == TRUE) clab.rotate <- 270
      text(sqrt(3/4), -0.085, clab, srt=270, pos=4, cex=lab.cex, font=lab.font)
    } else {
      text(sqrt(3/4) + 0.1, -0.15, clab, pos=2, cex=lab.cex, font=lab.font)
    }
  }
  
  # Return:
  return <- NULL
}

#' @describeIn TernaryPlot Add horizontal lines to the ternary plot
#' 
#' @importFrom graphics par
#' @keywords internal
#' @export
HorizontalGrid <- function (grid.lines = 10, grid.col='grey',
                            grid.lty='dotted', grid.lwd=par('lwd')) {
  
  line_points <- seq(from=0, to=1, length.out=grid.lines + 1L)
  
  lapply(line_points[-c(1, grid.lines + 1L)], function (p) {
    line_ends <- if (p <= 0.5) {
      apply(matrix(c(1-p, 1-(2*p),  p, 0,  0, p*2), nrow=2), 1, TernaryCoords)
    } else {
      p <- 1 - p
      -apply(matrix(c(1-p, 1-(2*p),  p, 0,  0, p*2), nrow=2), 1, TernaryCoords)
    }
    lines(abs(line_ends[1, ]), line_ends[2, ], col=grid.col, lty=grid.lty, lwd=grid.lwd)
    
  })
  
  # Return:
  return <- NULL
}

#' Add to Ternary Plot
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
