#' Value of a function at regularly spaced points
#'
#' Evaluates a function at points on a triangular grid. 
#' Intended to facilitate coloured contour plots with [`ColourTernary`].
#'
#' @template FuncParam
#' @template resolutionParam
#' @template directionParam
#' @return A matrix whose rows correspond to:
#' 
#'   **x**, **y**: co-ordinates of the centres of smaller triangles
#'   
#'   **z**: The value of `Func(a, b, c)`, where `a`, `b` and `c` are the ternary coordinates
#'   of `x` and `y`.
#'   
#'   **down**: `0` if the triangle concerned points upwards (or right), `1` otherwise
#' 
#' @concept Contour plots
#' @author Martin R. Smith
#' @export
TernaryPointValues <- function(Func, resolution = 48L, 
                               direction = getOption('ternDirection')) {
  triangleCentres <- TriangleCentres (resolution, direction)
  x <- triangleCentres['x', ]
  y <- triangleCentres['y', ]
  abc <- XYToTernary(x, y)
  
  # Return:
  rbind(x = x, y = y, z = Func(abc[1, ], abc[2, ], abc[3, ]), 
        down = triangleCentres['triDown', ])
}

#' Coordinates of triangle mid-points
#' 
#' @value A matrix containing three named rows:
#'  - `x` _x_ coordinates of triangle midpoints;
#'  - `y` _y_ coordinates of triangle midpoints;
#'  - `triDown` logical specifying whether given triangle points down.
TriangleCentres <- function (resolution = 48L, 
                             direction = getOption('ternDirection')) {
  
  offset <- 1 / resolution / 2L
  triangleHeight <- sqrt(0.75) / resolution
  trianglesInRow <- 2L * rev(seq_len(resolution)) - 1L
  
  if (direction == 1) { # Point up
    
    triX <- seq(from = -0.5 + offset, to = 0.5 - offset, by=offset)
    upY <- seq(from = triangleHeight / 3,
               to = sqrt(0.75) - (2 * triangleHeight / 3), length.out = resolution)
    x <- unlist(lapply(seq_len(resolution), function (yStep) {
      upX <- triX[seq(from = yStep, to = (2L * resolution) - yStep, by = 2L)]
      downX <- if (yStep == resolution) integer(0) else 
        triX[seq(from=yStep + 1, to = (2L * resolution) - yStep - 1, by = 2L)]
      c(rbind(upX, c(downX, NA)))[-((resolution - yStep + 1) * 2L)]
    }))
    y <- rep(upY[seq_len(resolution)], trianglesInRow)
    triDown <- (1 + unlist(lapply(trianglesInRow, seq_len))) %% 2L
    y <- y + (triDown * triangleHeight / 3)
    
  } else if (direction == 3L) { # Point down
    
    triX <- seq(from = -0.5 + offset, to = 0.5 - offset, by=offset)
    upY <- seq(from = 0 - (2 * triangleHeight / 3),
               to = - sqrt(0.75) + triangleHeight / 3, length.out = resolution)
    x <- unlist(lapply(seq_len(resolution), function (yStep) {
      upX <- triX[seq(from = yStep, to = (2L * resolution) - yStep, by = 2L)]
      downX <- if (yStep == resolution) integer(0) else 
        triX[seq(from=yStep + 1, to = (2L * resolution) - yStep - 1, by = 2L)]
      c(rbind(upX, c(downX, NA)))[-((resolution - yStep + 1) * 2L)]
    }))
    y <- rep(upY[seq_len(resolution)], trianglesInRow)
    triDown <- unlist(lapply(trianglesInRow, seq_len)) %% 2L
    y <- y + (triDown * triangleHeight / 3)
    
  } else if (direction == 2L) { # "Up" is to the right
    
    rightX <- seq(from = triangleHeight / 3,
                  to = sqrt(0.75) - (2 * triangleHeight / 3), length.out = resolution)
    triY <- seq(from = -0.5 + offset, to = 0.5 - offset, by=offset)
    y <- unlist(lapply(seq_len(resolution), function (xStep) {
      rightY <- triY[seq(from = xStep, to = (2L * resolution) - xStep, by = 2L)]
      leftY  <- if (xStep == resolution) integer(0) else 
        triY[seq(from=xStep + 1, to = (2L * resolution) - xStep - 1, by = 2L)]
      c(rbind(rightY, c(leftY, NA)))[-((resolution - xStep + 1) * 2L)]
    }))
    x <- rep(rightX[seq_len(resolution)], trianglesInRow)
    triDown <- (1 + unlist(lapply(trianglesInRow, seq_len))) %% 2L
    x <- x + (triDown * triangleHeight / 3)
    
  } else { # (direction == 4L)
    
    rightX <- seq(from = 0 - (2 * triangleHeight / 3),
                  to = - sqrt(0.75) + triangleHeight / 3, length.out = resolution)
    triY <- seq(from = -0.5 + offset, to = 0.5 - offset, by=offset)
    y <- unlist(lapply(seq_len(resolution), function (xStep) {
      rightY <- triY[seq(from = xStep, to = (2L * resolution) - xStep, by = 2L)]
      leftY  <- if (xStep == resolution) integer(0) else 
        triY[seq(from=xStep + 1, to = (2L * resolution) - xStep - 1, by = 2L)]
      c(rbind(rightY, c(leftY, NA)))[-((resolution - xStep + 1) * 2L)]
    }))
    x <- rep(rightX[seq_len(resolution)], trianglesInRow)
    triDown <- (unlist(lapply(trianglesInRow, seq_len))) %% 2L
    x <- x + (triDown * triangleHeight / 3)
  }
  
  #Return:
  rbind(x, y, triDown)
}

#' @describeIn TernaryPointValues Returns the density of points in each triangle
#' @importFrom sp point.in.polygon
#' @export
TernaryDensity <- function (coordinates, resolution = 48L, direction = getOption('ternDirection')) {
  
  scaled <- resolution * vapply(coordinates, function (coord) coord / sum(coord), double(3))
  whichTri <- floor(scaled)
  margins <- scaled %% 1 == 0
  onVertex <- apply(margins, 2, all)
  onEdge <- logical(length(onVertex))
  onEdge[!onVertex] <- apply(margins[, !onVertex], 2, any)
  
  centres <- whichTri[, !onEdge & !onVertex, drop=FALSE]
  edges   <- scaled[, onEdge, drop=FALSE]
  vertices <- scaled[, onVertex, drop=FALSE]
  
  OnEdge <- function (abc) {
    
    # Return 1 if on edge, 2 if on outer edge, 0 otherwise.
  }
  OnVertex <- function (abc) {
    
    # Return 1 if on vertex, 2 if on vertex on edge of plot, 6 if on vertex of plot
  }
  
  # Each point contributes a score of six, which -- if on a vertex -- may need
  # sharing between 2, 3 or 6 triangles.
  #ups <-
    lapply(seq_len(resolution) - 1L, function (a) {
    vapply(seq_len(resolution - a) - 1L, function (b) {
      abc <- c(a, b, resolution - a - b - 1L)
      (6 * sum(apply(centres, 2, identical, abc))) + 
      (3 * sum(OnUpEdge(abc))) + sum(OnUpVertex(abc))
      }, double(1), USE.NAMES = FALSE)
  })
  #downs <- 
    lapply(seq_len(resolution - 1L) - 1L, function (a) {
    vapply(seq_len(resolution - a - 1L) - 1L, function (b) {
      abc <- c(a, b, resolution - a - b - 2L)
      6 * sum(apply(centres, 2, identical, abc))
      }, double(1), USE.NAMES = FALSE)
  })
  
}

#' @keywords internal
#' @export
TernaryUpTiles <- function(x, y, resolution, col) {
  width <- 1 / resolution
  widthBy2 <- width / 2
  height <- sqrt(0.75) / resolution
  heightBy3 <- height / 3
  vapply(seq_along(x), function (i) {
    cornerX <- x[i] + c(0, widthBy2, -widthBy2)
    cornerY <- y[i] + c(heightBy3 + heightBy3, rep(-heightBy3, 2))
    polygon(cornerX, cornerY, col = col[i], border = NA)
    logical(0)
  }, logical(0))
  # Return:
  invisible()
}

#' @keywords internal
#' @export
TernaryDownTiles <- function(x, y, resolution, col) {
  width <- 1 / resolution
  widthBy2 <- width / 2
  height <- sqrt(0.75) / resolution
  heightBy3 <- height / 3
  
  vapply(seq_along(x), function (i) {
    cornerX <- x[i] + c(0, widthBy2, -widthBy2)
    cornerY <- y[i] - c(heightBy3 + heightBy3, rep(-heightBy3, 2))
    polygon(cornerX, cornerY, col = col[i], border = NA)
    logical(0)
  }, logical(0))
  
  # Return: 
  invisible()
}

#' @keywords internal
#' @export
TernaryLeftTiles <- function(x, y, resolution, col) {
  width <- sqrt(0.75) / resolution
  widthBy3 <- width / 3
  height <- 1 / resolution
  heightBy2 <- height / 2
  
  vapply(seq_along(x), function (i) {
    cornerX <- x[i] - c(widthBy3 + widthBy3, rep(-widthBy3, 2))
    cornerY <- y[i] + c(0, heightBy2, -heightBy2)
    polygon(cornerX, cornerY, col = col[i], border = NA)
    logical(0)
  }, logical(0))
  # Return:
  invisible()
}

#' @keywords internal
#' @export
TernaryRightTiles <- function(x, y, resolution, col) {
  width <- sqrt(0.75) / resolution
  widthBy3 <- width / 3
  height <- 1 / resolution
  heightBy2 <- height / 2
  
  vapply(seq_along(x), function (i) {
    cornerX <- x[i] + c(widthBy3 + widthBy3, rep(-widthBy3, 2))
    cornerY <- y[i] + c(0, heightBy2, -heightBy2)
    polygon(cornerX, cornerY, col = col[i], border = NA)
    logical(0)
  }, logical(0))
  # Return:
  invisible()
}


#' Paint tiles on ternary plot
#' 
#' Function to fill a ternary plot with coloured tiles.  Useful in combination with 
#' [`TernaryPointValues`] and [`TernaryContour`].
#' 
#' @aliases TernaryUpTiles TernaryDownTiles TernaryLeftTiles TernaryRightTiles
#' @param x,y Numeric vectors specifying _x_ and _y_ coordinates of centres of each triangle.
#' @param down Logical vector specifying `TRUE` if each triangle should point down (or right),
#' `FALSE` otherwise.
#' @template resolutionParam
#' @param col Vector specifying the colour with which to fill each triangle.
#' @template directionParam
#' 
#' @examples
#' 
#' FunctionToContour <- function (a, b, c) {
#'   a - c + (4 * a * b) + (27 * a * b * c)
#' }
#' 
#' TernaryPlot()
#' 
#' values <- TernaryPointValues(FunctionToContour, resolution=24L)
#' ColourTernary(values)
#' TernaryContour(FunctionToContour, resolution=36L)
#' 
#' 
#' @author Martin R. Smith
#' 
#' @export
TernaryTiles <- function (x, y, down, resolution, col, direction = getOption('ternDirection')) {
  down <- as.logical(down)
  if (direction %% 2) {
    TernaryDownTiles(x[down], y[down], resolution, col[down])
    TernaryUpTiles(x[!down], y[!down], resolution, col[!down])
  } else {
    TernaryLeftTiles(x[down], y[down], resolution, col[down])
    TernaryRightTiles(x[!down], y[!down], resolution, col[!down])
  }
  # Return: 
  invisible()
}


#' Colour a ternary plot according to the output of a function
#' 
#' @param values Numeric vector specifying the values associated with each point, 
#' generated using `[TernaryPointValues]`.
#' @param spectrum Vector of colours to use as a spectrum.
#' @template resolutionParam
#' @template directionParam
#' 
#' @author Martin R. Smith
#' 
#' @concept Contour plots
#' @importFrom viridisLite viridis
#' @export
ColourTernary <- function (values, spectrum = viridisLite::viridis(256L, alpha=0.6),
                           resolution = sqrt(ncol(values)),
                           direction = getOption('ternDirection')) {
  z <- values['z', ]
  zNorm <- z - min(z)
  zNorm <- zNorm / max(zNorm)
  TernaryTiles(values['x', ], values['y', ], values['down', ], resolution = resolution, 
               col=spectrum[as.integer(zNorm * (length(spectrum) - 1L)) + 1L], 
               direction = direction)
  # Return:
  invisible()
}

#' Add contours to a ternary plot
#' 
#' Draws contour lines to depict the value of a function in ternary space
#' 
#' @template FuncParam
#' @template resolutionParam
#' @template directionParam
#' @param \dots Further parameters to pass to `\link[graphics]{contour}
#' 
#' @author Martin R. Smith
#' 
#' @concept Contour plots
#' @importFrom graphics contour
#' @export
TernaryContour <- function (Func, resolution = 96L, direction = getOption('ternDirection'), ...) {
  if (direction == 1L) {
    x <- seq(-0.5, 0.5, length.out = resolution)
    y <- seq(0, sqrt(0.75), length.out = resolution)
  } else if (direction == 2L) {
    x <- seq(0, sqrt(0.75), length.out = resolution)
    y <- seq(-0.5, 0.5, length.out = resolution)
  } else if (direction == 3L) {
    x <- seq(-0.5, 0.5, length.out = resolution)
    y <- seq(-sqrt(0.75), 0, length.out = resolution)
  } else { # (direction == 4) 
    x <- seq(-sqrt(0.75), 0, length.out = resolution)
    y <- seq(-0.5, 0.5, length.out = resolution)
  }
  
  FunctionWrapper <- function(x, y) {
    abc <- XYToTernary(x, y)
    # TODO make more efficient by doing this intelligently rather than lazily
    ifelse(apply(abc < - 0.6 / resolution, 2, any),
           NA,
           Func(abc[1, ], abc[2, ], abc[3, ]))
  }
  z <- outer(X=x, Y=y, FUN=FunctionWrapper)
  contour(x, y, z, add=TRUE, ...)
}

