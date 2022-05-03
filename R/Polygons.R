#' Polygon geometry
#' 
#' Geometry functions for irregular polygons
#'
#' @param x,y Vectors containing the coordinates of the vertices of the polygon.
#' @examples 
#' x <- c(-3, -1, 6, 3, -4)
#' y <- c(-2, 4, 1, 10, 9)
#' plot(x, y, frame.plot = FALSE)
#' polygon(x, y)
#' @template MRS
#' @family tiling functions
#' @name Polygon-Geometry
NULL

#' @describeIn Polygon-Geometry Calculate the area of an irregular polygon
#' @importFrom grDevices xy.coords
#' @param positive If vertices are specified in an anticlockwise direction,
#' the polygon will be treated as a hole, with a negative area, unless
#' `positive` is set to TRUE.
#' Vertices specified in a clockwise sequence always yield a positive area.
#' 
#' @examples
#' PolygonArea(x, y)
#' @export
PolygonArea <- function(x, y = NULL, positive = TRUE) {
  xy <- xy.coords(x, y)
  x <- c(xy$x, xy$x[1])
  y <- c(xy$y, xy$y[1])
  area <- (sum((xy$x * y[-1])) - sum((xy$y * x[-1]))) / 2
  
  # Return:
  if (isTRUE(positive)) {
    abs(area)
  } else {
    area
  }
}

#' @describeIn Polygon-Geometry Locate the centre of a polygon
#' @importFrom grDevices xy.coords
#' @examples
#' points(PolygonCentre(x, y), pch = 3, cex = 2)
#' @export
PolygonCentre <- function(x, y = NULL) {
  xy <- xy.coords(x, y)
  area <- PolygonArea(xy, positive = FALSE)
  
  n <- length(xy$x)
  x <- xy$x
  x1 <- c(x[-1], x[1])
  y <- xy$y
  y1 <- c(y[-1], y[1])
  prod <- (x * y1) - (x1 * y)
  
  cbind(x = sum((x + x1) * prod),
        y = sum((y + y1) * prod)) / (6 * area)
}

#' @rdname Polygon-Geometry
#' @export
PolygonCenter <- PolygonCentre


#' @describeIn Polygon-Geometry Enlarge a polygon in all directions
#' 
#' @param buffer Numeric specifying distance by which to grow polygon.
#' @return `GrowPolygon()` returns coordinates of the points of `polygon` after
#' moving each point `buffer` away from the polygon's centroid.
#' @importFrom grDevices xy.coords
#' @examples
#' polygon(GrowPolygon(x, y, 1), border = "darkgreen",
#'         xpd = NA # Allow drawing beyond plot border
#'        )
#' 
#' # Negative values shrink the polygon
#' polygon(GrowPolygon(x, y, -1), border = "red")
#' @export
GrowPolygon <- function(x, y = NULL, buffer = 0) {
  xy <- xy.coords(x, y)
  cent <- PolygonCentre(x, y)
  
  x0 <- xy$x - cent[, "x"]
  y0 <- xy$y - cent[, "y"]
  hyp <- sqrt((x0 * x0) + (y0 * y0))
  stretch <- (hyp + buffer) / hyp
  x1 <- x0 * stretch
  y1 <- y0 * stretch
  
  xy$x <- x1 + cent[, "x"]
  xy$y <- y1 + cent[, "y"]
  
  xy
}
