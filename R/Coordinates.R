#' Convert ternary coordinates to Cartesian space
#' 
#' Converts coordinates of a point in ternary space, in the format (_a_, _b_, _c_), to
#' _x_ and _y_ coordinates of Cartesian space, which can be sent to standard functions
#' in the graphics package.
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
#'         into Cartesian (_x_, _y_) coordinates corresponding to the plot created by
#'         the last call of \code{\link{TernaryPlot}}.
#'
#' @seealso [TernaryPlot]
#' @concept Coordinate translation
#' @author Martin R. Smith
#' @export
TernaryCoords <- function (abc, b_coord=NULL, c_coord=NULL, direction=getOption('ternDirection')) {
  if (!is.null(b_coord) && !is.null(c_coord)) {
    abc <- c(abc, b_coord, c_coord)
  }
  if (length(abc) != 3) stop("Parameter abc must be a vector of length three.")
  if (mode(abc) != 'numeric') stop("Parameter abc must be numeric.")
  if (!(direction %in% 1:4)) stop  ("Parameter direction must be 1, 2, 3 or 4")
  names(abc) <- NULL # or they may be inherited by x and y, confusingly
  
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


#' Cartesian coordinates to ternary point
#' 
#' @param x,y Numeric values giving the _x_ and _y_ coordinates of a point or points.
#' @template directionParam
#' 
#' @return `XYToTernary` Returns the ternary point(s) corresponding to the specified _x_ and _y_ 
#' coordinates, where a + b + c = 1.
#' 
#' 
#' @author Martin R. Smith
#' 
#' @concept Coordinate translation
#' @export
XYToTernary <- function (x, y, direction=getOption('ternDirection')) {
  if (mode(x) != 'numeric') stop("Parameter x must be numeric.")
  if (mode(y) != 'numeric') stop("Parameter y must be numeric.")
  if (!(direction %in% 1:4)) stop  ("Parameter direction must be 1, 2, 3 or 4")
  
  if (direction == 1L) {
    a <- y / sqrt(0.75)
    bcRange <- 1 - a
    b <- x + (bcRange / 2)
    c <- bcRange - b
  } else if (direction == 2L) {
    a <- x / sqrt(0.75)
    bcRange <- 1 - a
    c <- y + (bcRange / 2)
    b <- bcRange - c
  } else if (direction == 3L) {
    a <- -y / sqrt(0.75)
    bcRange <- 1 - a
    b <- -x + (bcRange / 2)
    c <- bcRange - b
  } else { # direction == 4L
    a <- -x / sqrt(0.75)
    bcRange <- 1 - a
    b <- y + (bcRange / 2)
    c <- bcRange - b
  }
  # Return:
  rbind(a, b, c)
}

#' X and Y coordinates of ternary plotting area
#'
#' @template directionParam
#'
#' @return Returns the minimum and maximum X or Y coordinate of the area 
#' in which a ternary plot is drawn, oriented in the specified direction.
#' Because the plotting area is a square, the triangle of the ternary plot
#' will not occupy the full range in one direction.
#' Assumes that the defaults have not been overwritten by specifying `xlim` or `ylim`.
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

#' Is a point in the plotting area?
#' 
#' @template xyParams
#' @param tolerance Consider points this close to the edge of the plot to be 
#' inside.  Set to negative values to count points that are just outside the 
#' plot as inside, and to positive values to count points that are just inside
#' the margins as outside. Maximum positive value: 1/3.
#' 
#' @return Logical vector specifying whether each pair of _x_ and _y_ coordinates
#' corresponds to a point outside the plotted ternary diagram.
#' 
#' @author Martin R. Smith
#' @export
OutsidePlot <- function (x, y, tolerance = 0) {
  abc <- XYToTernary(x, y)
  apply(abc < tolerance, 2, any)
}

#' Reflected equivalents of points outside the ternary plot
#' 
#' To avoid edge effects, it may be desirable to add the value of a point
#' within a ternary plot with the value of its 'reflection' across the nearest 
#' axis or corner.
#' 
#' @template xyParams
#' @template directionParam
#' 
#' @return A list of the _x_, _y_ coordinates of the points produced if 
#' the given point is reflected across each of the edges or corners.
#' 
ReflectedEquivalents <- function (x, y, direction = getOption('ternDirection')) {
  switch(direction, {
    # 1L
    corners <- matrix(c(0, cos(pi/6), 0.5, 0, -0.5, 0), nrow=2)
    edgeM <- tan(pi/3) * rep(c(1, -1, 0), 2)
    edgeC <- cos(pi/6) * c(1, 1, 0, -1, -1, 1)
  }, {
    # 2L
    corners <- matrix(c(cos(pi/6), 0, 0, -0.5, 0, 0.5), nrow=2)
    edgeM <- tan(pi/6) * rep(c(-1, 1, Inf), 2)
    edgeC <- 0.5 * c(1, 1, 0, -1, -1, 2 * cos(pi/6))
  }, {
    # 3L
    corners <- matrix(c(0, -cos(pi/6), -0.5, 0, 0.5, 0), nrow=2)
    edgeM <- tan(pi/3) * rep(c(1, -1, 0), 2)
    edgeC <- cos(pi/6) * c(-1, -1, 0, 1, 1, -1)
  }, {
    # 4L
    corners <- matrix(c(-cos(pi/6), 0, 0, 0.5, 0, -0.5), nrow=2)
    edgeM <- tan(pi/6) * rep(c(1, -1, 0), 2)
    edgeC <- 0.5 * c(1, 1, 0, -1, -1, 1)
  })
  xx <- lapply((1:6)[is.finite(edgeM)], function (i) abline(edgeC[i], edgeM[i], col=i))
  xx <- lapply((1:6)[!is.finite(edgeM)], function (i) abline(v=edgeC[i], col=i))

  # If m = Inf, we have a vertical line, and c specifies its x intercept.
  ReflectAcrossLine <- function (xi, yi, m, c) {
    d <- (xi + (yi - c) * m) / (1 + (m * m))
    ret <- cbind(d + d - xi, 2 * d * m - yi + c + c)
    infiniteM <- !is.finite(m)
    if (any(infiniteM)) {
      altRet <- cbind(c + c - xi, rep(y, length.out=dim(ret)[1]))
      ret[infiniteM, ] <- altRet[infiniteM, ]
    }
    ret
  }
  
  mirror1 <- c(5L, 6L, 4L)
  mirror2 <- c(6L, 4L, 5L)
  lapply(seq_along(x), function (i) {
    reflectOnce <- ReflectAcrossLine(x[i], y[i], edgeM, edgeC)
    rbind(
      reflectOnce,
      ReflectAcrossLine(reflectOnce[1:3, 1L], reflectOnce[1:3, 2L],
                        edgeM[mirror1], edgeC[mirror1]),
      ReflectAcrossLine(reflectOnce[1:3, 1L], reflectOnce[1:3, 2L],
                        edgeM[mirror2], edgeC[mirror2])
    )
  })
  
}
