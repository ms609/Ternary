#' Value of a function at regularly spaced points
#'
#' Evaluates a function at points on a triangular grid. 
#' Intended to facilitate coloured contour plots with \code{\link{ColourTernary}}.
#' 
#' Density plotting functions are somewhat experimental; please
#' \href{https://github.com/ms609/Ternary/issues/new}{report any issues}.
#'
#' @template FuncParam
#' @template resolutionParam
#' @template directionParam
#' @return A matrix whose rows correspond to:
#' 
#'   **x**, **y**: co-ordinates of the centres of smaller triangles
#'   
#'   **z**: The value of `Func(a, b, c)`, where `a`, `b` and `c` are the 
#'   ternary coordinates of `x` and `y`.
#'   
#'   **down**: `0` if the triangle concerned points upwards (or right), 
#'   `1` otherwise
#' 
#' @family contour plotting functions
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
#' @template resolutionParam
#' @template directionParam
#' 
#' @return A matrix containing three named rows:
#'  - `x` _x_ coordinates of triangle midpoints;
#'  - `y` _y_ coordinates of triangle midpoints;
#'  - `triDown` binary integer specifying whether given triangle points down.
#'  
#' @family coordinate translation functions
#' @author Martin R. Smith
#' @export
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
#' @template coordinatesParam
#' @export
TernaryDensity <- function (coordinates, resolution = 48L, direction = getOption('ternDirection')) {
  if (class(coordinates) == 'list') {
    scaled <- resolution * vapply(coordinates, function (coord) coord / sum(coord),
                                  double(3L))
  } else {
    scaled <- resolution * apply(coordinates, 1, function (coord) coord / sum(coord))
  }
  
  whichTri <- floor(scaled)
  margins <- scaled %% 1 == 0
  onVertex <- apply(margins, 2, all)
  onEdge <- logical(length(onVertex))
  onEdge[!onVertex] <- apply(margins[, !onVertex], 2, any)
  
  centres <- whichTri[, !onEdge & !onVertex, drop=FALSE]
  edges   <- scaled[, onEdge, drop=FALSE]
  floorEdges <- floor(edges)
  vertices <- scaled[, onVertex, drop=FALSE]
  vertexLocation <- apply(vertices == 0, 2, sum)
  vertexInternal <- vertexLocation == 0L
  vertexOnEdge <- vertexLocation == 1L
  vertexOnCorner <- vertexLocation == 2L
  
  AllEqual <- function (x, y) all(x == y)
  
  OnUpEdge <- function (abc) {
    # Return 1 for points on edge, 2 for each point on outer edge, 0 for each other.
    onThisEdge <- apply(floorEdges, 2, AllEqual, abc)
    theseEdges <- edges[, onThisEdge, drop=FALSE]
    
    ncol(theseEdges) + 
    {if (abc[1] == 0) sum(theseEdges[1, ] == 0) else 0L} +
    {if (abc[2] == 0) sum(theseEdges[2, ] == 0) else 0L} +
    {if (abc[3] == 0) sum(theseEdges[3, ] == 0) else 0L}
  }
  OnDownEdge <- function (abc) {
    sum(
      apply(edges, 2, AllEqual, abc + c(0.5, 0.5, 1.0)),
      apply(edges, 2, AllEqual, abc + c(0.5, 1.0, 0.5)),
      apply(edges, 2, AllEqual, abc + c(1.0, 0.5, 0.5))
    )
  }
  OnUpVertex <- function (abc) {
    onThisTriangle <-
      apply(vertices, 2, AllEqual, abc + c(1L, 0L, 0L)) |
      apply(vertices, 2, AllEqual, abc + c(0L, 1L, 0L)) |
      apply(vertices, 2, AllEqual, abc + c(0L, 0L, 1L))

    sum(vertexInternal[onThisTriangle], # Return 1 for each point on vertex, 
        2L * vertexOnEdge[onThisTriangle], # 2 for each point on a vertex on edge of plot, 
        6L * vertexOnCorner[onThisTriangle])# 6 for each point on outer vertex of plot
  }
  OnDownVertex <- function (abc) {
    onThisTriangle <-
      apply(vertices, 2, AllEqual, abc + c(1L, 1L, 0L)) |
      apply(vertices, 2, AllEqual, abc + c(1L, 0L, 1L)) |
      apply(vertices, 2, AllEqual, abc + c(0L, 1L, 1L))
    
    sum(vertexInternal[onThisTriangle], 2L * vertexOnEdge[onThisTriangle])
  }
  
    
  # Each point contributes a score of six, which -- if on a vertex -- may need
  # sharing between 2, 3 or 6 triangles.
  
  #lapply(seq_len(resolution) - 1L, function (a) {
  #  sapply(seq_len(resolution - a) - 1L, function (b) {
  #    abc <- c(a, b, resolution - a- b - 1L)
  #    paste0(paste0(abc, collapse=','), ': E', 3*OnUpEdge(abc), " +C", OnUpVertex(abc))
  #  }, USE.NAMES = FALSE)
  #})
  
  #lapply(seq_len(resolution - 1L) - 1L, function (a) {
  #  sapply(seq_len(resolution - a - 1L) - 1L, function (b) {
  #    abc <- c(a, b, resolution - a - b - 2L)
  #    paste0(a, b, abc[3], ': ', OnDownEdge(abc) , '; C: ', OnDownVertex(abc))
  #    }, USE.NAMES = FALSE)
  #})
  
  # Work with an upright triangle for now.  Then translate it if necessary
  ups <- unlist(lapply(seq_len(resolution) - 1L, function (a) {
    vapply(seq_len(resolution - a) - 1L, function (b) {
      abc <- c(a, b, resolution - a - b - 1L)
      (6 * sum(apply(centres, 2, AllEqual, abc))) + 
      (3 * sum(OnUpEdge(abc))) + sum(OnUpVertex(abc))
      }, double(1), USE.NAMES = FALSE)
  }))
  downs <- unlist(
    lapply(seq_len(resolution - 1L) - 1L, function (a) {
    vapply(seq_len(resolution - a - 1L) - 1L, function (b) {
      abc <- c(a, b, resolution - a - b - 2L)
      6 * sum(apply(centres, 2, AllEqual, abc)) +
        (3 * sum(OnDownEdge(abc))) + sum(OnDownVertex(abc))
      }, double(1), USE.NAMES = FALSE)
  }))
  
  centrePoints <- TriangleCentres(resolution, direction)
  triDown <- as.logical(centrePoints['triDown', ])
  ret <- integer(length(ups) + length(downs))
  towardsBase <- if (direction < 3L) triDown else !triDown
  ret[towardsBase] <- downs
  ret[!towardsBase] <- ups
  
  switch(direction, {
    # 1L = point up
    xy <- centrePoints[1:2, ]
  }, {
    #2L = right
    xy <- rbind(x = centrePoints[1, ], 
                y = -centrePoints[2, ])
  }, {
    #3L = down
    xy <- rbind(x = -centrePoints[1, ],
                y = centrePoints[2, ])
  }, {
    #4L = left
    xy <- centrePoints[1:2, ]
  })
  
  # TernaryPlot(grid.lines=3, axis.labels=1:3, point='right')
  # ColourTernary(rbind(xy, z = ret, down = triDown))

  # Return:
  rbind(xy, z = ret, down = triDown)
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
#' generated using [`TernaryPointValues`].
#' @param spectrum Vector of colours to use as a spectrum.
#' @template resolutionParam
#' @template directionParam
#' 
#' @author Martin R. Smith
#' 
#' @family contour plotting functions
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
#' Draws contour lines to depict the value of a function in ternary space.
#' 
#' @template FuncParam
#' @template resolutionParam
#' @template directionParam
#' @template dotsToContour
#' 
#' @author Martin R. Smith
#' 
#' @family contour plotting functions
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

#' Add contours of estimated point density to a ternary plot
#' 
#' Uses two-dimensional kernel density estimation to plot contours of 
#' point density.
#' 
#' This function is modelled on MASS::kde2d, which uses
#' "an axis-aligned bivariate normal kernel, evaluated on a square grid".
#' 
#' This is to say, values are calculated on a square grid, and contours fitted
#' between these points.  This produces a couple of artefacts.
#' Firstly, contours may not extend beyond the outermost point within the 
#' diagram, which may fall some distance from the margin of the plot if a 
#' low `resolution` is used.  Setting a negative `tolerance` parameter allows
#' these contours to extend closer to (or beyond) the margin of the plot.
#' 
#' Individual points cannot fall outside the margins of the ternary diagram,
#' but their associated kernels can. In order to sample regions of the kernels
#' that have 'bled' outside the ternary diagram, each point's value is 
#' calculated by summing the point density at that point and at equivalent 
#' points outside the ternary diagram, 'reflected' across the margin of 
#' the plot (see function [`ReflectedEquivalents`]).  This correction can be
#' disabled by setting the `edgeCorrection` parameter to `FALSE`.
#' 
#' A model based on a triangular grid may be more appropriate in certain
#' situations, but is non-trivial to implement; if this distinction is 
#' important to you, please let the maintainers known by opening a 
#' \href{https://github.com/ms609/Ternary/issues/new?title=Triangular+KDE}{Github issue}.
#' 
#' 
#' @template coordinatesParam
#' @param bandwidth Vector of bandwidths for x and y directions. 
#' Defaults to normal reference bandwidth (see MASS::bandwidth.nrd).
#' A scalar value will be taken to apply to both directions.
#' @template resolutionParam
#' @param tolerance Numeric specifying how close to the margins the contours 
#' should be plotted, as a fraction of the size of the triangle.
#' Negative values will cause contour lines to extend beyond the margins of the plot.
#' @template directionParam
#' @template dotsToContour
#' @param edgeCorrection Logical specifying whether to correct for edge effects
#'  (see details).
#' 
#' @family contour plotting functions
#' @author Adapted from MASS::kde2d by Martin R. Smith
#' @importFrom stats dnorm quantile var
#' @export
TernaryDensityContour <- function (coordinates, bandwidth, resolution = 25L, 
                                   tolerance = -0.2 / resolution,
                                   edgeCorrection = TRUE,
                                   direction = getOption('ternDirection'),
                                   ...) {
  # Adapted from MASS::kde2d
  xy <- apply(coordinates, 1, TernaryCoords)
  x <- xy[1, ]
  y <- xy[2, ]
  n <- length(x)
  
  Bandwidth <- function (x, lengthX) {
    # Adapted from MASS::bandwidth.nrd
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2L] - r[1L]) / 1.34
    # Don't multiply by 4 just to divide by 4 again...
    1.06 * min(sqrt(var(x)), h) * lengthX ^ (-0.2)
  }
  
  h <- if (missing(bandwidth)) {
    c(Bandwidth(x, n), Bandwidth(y, n))
  } else {
    rep(bandwidth / 4L, length.out = 2L)
  }
  if (any(h <= 0))
    stop("bandwidths must be strictly positive")
  
  switch (direction, {
    gx <- seq(-0.5, 0.5, length.out = resolution)
    gy <- seq(0, sqrt(0.75), length.out = resolution)
  }, {
    gx <- seq(0, sqrt(0.75), length.out = resolution)
    gy <- seq(-0.5, 0.5, length.out = resolution)
  }, {
    gx <- seq(-0.5, 0.5, length.out = resolution)
    gy <- seq(-sqrt(0.75), 0, length.out = resolution)
  }, { 
    gx <- seq(-sqrt(0.75), 0, length.out = resolution)
    gy <- seq(-0.5, 0.5, length.out = resolution)
  })
  
  if (edgeCorrection) {
    KDE <- function (ix, iy) {
      if (OutsidePlot(ix, iy, tolerance = tolerance)) NA else {
        reflections <- ReflectedEquivalents(ix, iy, direction=direction)[[1]]
        ax <- outer(c(ix, reflections[, 1]), x, "-") / h[1L]
        ay <- outer(c(iy, reflections[, 2]), y, "-") / h[2L]
        sum(vapply(seq_len(1L + dim(reflections)[1]), function (i)
          tcrossprod(matrix(dnorm(ax[i, ]), ncol=n), matrix(dnorm(ay[i, ]), ncol=n)),
          double(1))) / prod(n, h)
      }
    }
    
    z <- matrix(mapply(KDE, gx, rep(gy, each=length(gx))), ncol=length(gx))
  } else {
    ax <- outer(gx, x, "-") / h[1L]
    ay <- outer(gy, y, "-") / h[2L]
    z <- tcrossprod(matrix(dnorm(ax), ncol = n),
                    matrix(dnorm(ay), ncol = n)) / prod(n, h)
    
    # TODO make more efficient by doing this intelligently rather than lazily
    zOffPlot <- outer(gx, gy, OutsidePlot, tolerance = tolerance)
    z[zOffPlot] <- NA
  }
  
  contour(list(x = gx, y = gy, z = z), add=TRUE, ...)
}