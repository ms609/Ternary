#' Convert ternary coordinates to Cartesian space
#'
#' Convert coordinates of a point in ternary space, in the format
#' (_a_, _b_, _c_), to _x_ and _y_ coordinates of Cartesian space, which can be
#' sent to standard functions in the 'graphics' package.
#'
#' @param abc A vector of length three giving the position on a ternary plot
#' that points in the direction specified by `direction` (1 = up, 2 = right,
#' 3 = down, 4 = left). \code{c(100, 0, 0)} will plot in the `direction`-most
#' corner; \code{c(0, 100, 0)} will plot in the corner clockwise of `direction`;
#' \code{c(0, 0, 100)} will plot in the corner anti-clockwise of `direction`.
#' Alternatively, the a coordinate can be specified as the first parameter,
#' in which case the b and c coordinates must be specified via \code{b_coord}
#' and \code{c_coord}.
#' Or, a matrix with three rows, representing in turn the `a`, `b` and `c`
#' coordinates of points.
#' @param b_coord The b coordinate, if \code{abc} is a single number.
#' @param c_coord The c coordinate, if \code{abc} is a single number.
#' @template directionParam
#' @inheritParams TernaryPlot
#'
#' @return `TernaryCoords()` returns a vector of length two that converts
#' the coordinates given in `abc` into Cartesian (_x_, _y_) coordinates
#' corresponding to the plot created by the last call of [`TernaryPlot()`].
#'
#' @seealso
#' - [`TernaryPlot()`]
#'
#' @examples
#' TernaryCoords(100, 0, 0)
#' TernaryCoords(c(0, 100, 0))
#'
#' coords <- matrix(1:12, nrow = 3)
#' TernaryToXY(coords)
#' @family coordinate translation functions
#' @template MRS
#' @export
TernaryCoords <- function(
    abc, b_coord = NULL, c_coord = NULL,
    direction = getOption("ternDirection", 1L),
    region = getOption("ternRegion", ternRegionDefault)
  ) {
  UseMethod("TernaryToXY")
}

#' @rdname TernaryCoords
#' @export
TernaryToXY.matrix <- function(
    abc, b_coord = NULL, c_coord = NULL,
    direction = getOption("ternDirection", 1L),
    region = getOption("ternRegion", ternRegionDefault)
  ) {
  ret <- apply(abc, 2, TernaryToXY, direction = direction, region = region)
  rownames(ret) <- c("x", "y")

  # Return:
  ret
}

#' @rdname TernaryCoords
#' @export
TernaryToXY.numeric <- function(
    abc, b_coord = NULL, c_coord = NULL,
    direction = getOption("ternDirection", 1L),
    region = getOption("ternRegion", ternRegionDefault)
  ) {
  if (!is.null(b_coord) && !is.null(c_coord)) {
    abc <- c(abc, b_coord, c_coord)
  }
  if (length(abc) != 3) {
    stop("Parameter `abc` must be a vector of length three.")
  }
  if (!is.numeric(abc)) {
    stop("Parameter `abc` must be numeric.")
  }
  if (!(direction %in% 1:4)) {
    stop("Parameter `direction` must be 1, 2, 3 or 4")
  }
  # unname to avoid x and y inheriting names
  abc <- unname(abc)[switch(direction,
    c(2, 3, 1),
    c(3, 2, 1),
    c(3, 2, 1),
    c(2, 3, 1)
  )]
  if (!any(as.logical(abc))) {
    # abc == c(0, 0, 0)
    abc <- c(1, 1, 1)
  }

  x_deviation <- abc[[3]] / sum(abc)
  if (x_deviation == 1) {
    x <- cos(pi / 6)
    y <- 0
  } else {
    y_deviation <- (abc[[1]] - abc[[2]]) / sum(abc[1:2])
    x <- x_deviation * cos(pi / 6)
    y <- y_deviation * (1 - x_deviation) / 2
  }
  
  # Return:
  .NormalizeToRegion(
    switch(direction,
      c(y, x),
      c(x, y),
      c(y, -x),
      c(-x, y)
    ),
    region = region
  )
}

#' @rdname TernaryCoords
#' @export
TernaryToXY <- TernaryCoords

#' Cartesian coordinates to ternary point
#'
#' Convert cartesian (_x_, _y_) coordinates to a point in ternary space.
#'
#' @param x,y Numeric values giving the _x_ and _y_ coordinates of a point or
#' points.
#' @template directionParam
#' @inheritParams TernaryPlot
#'
#' @return `XYToTernary()` Returns the ternary point(s) corresponding to the
#' specified _x_ and _y_ coordinates, where a + b + c = 1.
#'
#' @examples
#' XYToTernary(c(0.1, 0.2), 0.5)
#' @template MRS
#'
#' @family coordinate translation functions
#' @export
XYToTernary <- function(
    x, y,
    direction = getOption("ternDirection", 1L),
    region = getOption("ternRegion", ternRegionDefault)
  ) {
  if (!is.numeric(x)) {
    stop("Parameter `x` must be numeric.")
  }
  if (!is.numeric(y)) {
    stop("Parameter `y` must be numeric.")
  }
  if (!(direction %in% 1:4)) {
    stop("Parameter direction must be 1, 2, 3 or 4")
  }
  
  xy <- .UnnormalizeXY(x, y)
  x <- xy[[1]]
  y <- xy[[2]]

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

#' @rdname XYToTernary
#' @export
XYToHoldridge <- function(x, y) {
  tern <- XYToTernary(x, y, 1L)

  # Return:
  rbind(
    pet = 2^(tern[3, ] * 8 - 3),
    prec = 1000 * 2^(tern[2, ] * 8 - 4)
  )
}

#' @rdname XYToTernary
#' @export
XYToPetPrec <- XYToHoldridge

#' X and Y coordinates of ternary plotting area
#'
#' @template directionParam
#'
#' @return `TernaryXRange()` and `TernaryYRange()` return the minimum and
#' maximum X or Y coordinate of the area in which a ternary plot is drawn,
#' oriented in the specified direction.
#' Because the plotting area is a square, the triangle of the ternary plot
#' will not occupy the full range in one direction.
#' Assumes that the defaults have not been overwritten by specifying `xlim` or
#' `ylim`.
#'
#' @template MRS
#' @family plot limits
#' @export
TernaryXRange <- function(direction = getOption("ternDirection", 1L)) {
  if (is.na(direction) || !(direction %in% 1:4)) {
    stop("Invalid ternary orientation")
  }
  if (direction == 2L) {
    c(0, 1) - ((1 - sqrt(0.75)) / 2) # Range should equal Y range. Centre plot.
  } else if (direction == 4L) {
    c(-1, 0) + ((1 - sqrt(0.75)) / 2) # Range should equal Y range. Centre plot.
  } else {
    c(-0.5, 0.5)
  }
}

#' @describeIn TernaryXRange Returns the minimum and maximum Y coordinate for a
#' ternary plot in the specified direction.
#' @export
TernaryYRange <- function(direction = getOption("ternDirection", 1L)) {
  if (is.na(direction) || !(direction %in% 1:4)) {
    stop("Invalid ternary orientation")
  }
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
#' Evaluate whether a given set of coordinates lie outwith the boundaries of
#' a plotted ternary diagram.
#'
#' @template xyParams
#' @param tolerance Consider points this close to the edge of the plot to be
#' inside.  Set to negative values to count points that are just outside the
#' plot as inside, and to positive values to count points that are just inside
#' the margins as outside. Maximum positive value: 1/3.
#'
#' @return `OutsidePlot()` returns a logical vector specifying whether each
#' pair of _x_ and _y_ coordinates corresponds to a point outside the plotted
#' ternary diagram.
#'
#' @examples
#'
#' TernaryPlot()
#' points(0.5, 0.5, col = "darkgreen")
#' OutsidePlot(0.5, 0.5)
#'
#' points(0.1, 0.5, col = "red")
#' OutsidePlot(0.1, 0.5)
#'
#' OutsidePlot(c(0.5, 0.1), 0.5)
#' @template MRS
#' @family plot limits
#' @export
OutsidePlot <- function(x, y, tolerance = 0) {
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
#' @return `ReflectedEquivalents()` returns a list of the _x_, _y_ coordinates
#' of the points produced if the given point is reflected across each of the
#' edges or corners.
#'
#' @examples
#' TernaryPlot(axis.labels = FALSE, point = 4)
#'
#' xy <- cbind(
#'   TernaryCoords(0.9, 0.08, 0.02),
#'   TernaryCoords(0.15, 0.8, 0.05),
#'   TernaryCoords(0.05, 0.1, 0.85)
#' )
#' x <- xy[1, ]
#' y <- xy[2, ]
#'
#' points(x, y, col = "red", pch = 1:3)
#' ref <- ReflectedEquivalents(x, y)
#' points(ref[[1]][, 1], ref[[1]][, 2], col = "blue", pch = 1)
#' points(ref[[2]][, 1], ref[[2]][, 2], col = "green", pch = 2)
#' points(ref[[3]][, 1], ref[[3]][, 2], col = "orange", pch = 3)
#' @family coordinate translation functions
#' @export
ReflectedEquivalents <- function(
    x, y,
    direction = getOption("ternDirection", 1L)
  ) {
  switch(direction,
    {
      # 1L
      edgeM <- tan(pi / 3) * rep(c(1, -1, 0), 2)
      edgeC <- cos(pi / 6) * c(1, 1, 0, -1, -1, 1)
    },
    {
      # 2L
      edgeM <- tan(pi / 6) * rep(c(-1, 1, Inf), 2)
      edgeC <- 0.5 * c(1, -1, 0, -1, 1, 2 * cos(pi / 6))
    },
    {
      # 3L
      edgeM <- tan(pi / 3) * rep(c(1, -1, 0), 2)
      edgeC <- cos(pi / 6) * c(-1, -1, 0, 1, 1, -1)
    },
    {
      # 4L
      edgeM <- tan(pi / 6) * rep(c(1, -1, Inf), 2)
      edgeC <- 0.5 * c(1, -1, 0, -1, 1, -2 * cos(pi / 6))
    }
  )

  # If m = Inf, we have a vertical line, and c specifies its x intercept.
  ReflectAcrossLine <- function(xi, yi, m, c) {
    d <- (xi + (yi - c) * m) / (1 + (m * m))
    ret <- cbind(d + d - xi, 2 * d * m - yi + c + c)
    infiniteM <- !is.finite(m)
    if (any(infiniteM)) {
      altRet <- cbind(c + c - xi,
                      rep(yi, length.out = max(length(c), length(xi))))
      ret[infiniteM, ] <- altRet[infiniteM, ]
    }
    ret
  }

  mirror1 <- c(5L, 6L, 4L)
  mirror2 <- c(6L, 4L, 5L)
  lapply(seq_along(x), function(i) {
    reflectOnce <- ReflectAcrossLine(x[i], y[i], edgeM, edgeC)
    rbind(
      reflectOnce,
      ReflectAcrossLine(
        reflectOnce[1:3, 1L], reflectOnce[1:3, 2L],
        edgeM[mirror1], edgeC[mirror1]
      ),
      ReflectAcrossLine(
        reflectOnce[1:3, 1L], reflectOnce[1:3, 2L],
        edgeM[mirror2], edgeC[mirror2]
      )
    )
  })
}
