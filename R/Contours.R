#' Value of a function at regularly spaced points
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
#' @author Martin R. Smith
#' @export
TernaryPointValues <- function(Func, resolution = 48L, direction = getOption('ternDirection')) {
  if (direction == 1) {
    xRange <- c(-0.5, 0.5)
    dX <- 1
    yRange <- c(0, sqrt(0.75))
    dY <- sqrt(0.75)
    offset <- dX / resolution / 2L
    triangleHeight <- dY / resolution
    trianglesInRow <- 2L * rev(seq_len(resolution)) - 1L
    
    triX <- seq(from=xRange[1] + offset, to=xRange[2] - offset, by=offset)
    upY <- seq(from=yRange[1] + triangleHeight / 3,
               to=yRange[2] - (2 * triangleHeight / 3), length.out = resolution)
    x <- unlist(lapply(seq_len(resolution), function (yStep) {
      upX <- triX[seq(from=yStep, to=(2L * resolution) - yStep, by = 2L)]
      downX <- if(yStep == resolution) integer(0) else 
        triX[seq(from=yStep + 1, to = (2L * resolution) - yStep - 1, by = 2L)]
      c(rbind(upX, c(downX, NA)))[-((resolution - yStep + 1) * 2L)]
    }))
    y <- rep(upY[seq_len(resolution)], trianglesInRow)
    triDown <- (1 + unlist(lapply(trianglesInRow, seq_len))) %% 2L
    y <- y + (triDown * triangleHeight / 3)
  }
  abc <- XYToTernary(x, y)
  # Return:
  rbind(x = x, y = y, z = Func(abc[1, ], abc[2, ], abc[3, ]), down = triDown)
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
  
  apply(cbind(x, y, col), 1, function (tri) {
    cornerX <- as.double(tri['x']) + c(0, widthBy2, -widthBy2)
    cornerY <- as.double(tri['y']) - c(heightBy3 + heightBy3, rep(-heightBy3, 2))
    polygon(cornerX, cornerY, col = tri['col'], border = NA)
  })
  # Return: 
  invisible()
}

#' @aliases TernaryUpTiles TernaryDownTiles TernaryLeftTiles TernaryRightTiles
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
#' @importFrom graphics contour
#' @export
TernaryContour <- function (Func, resolution = 96L, direction = getOption('ternDirection'), ...) {
  if (direction == 1) {
    x <- seq(-0.5, 0.5, length.out = resolution)
    y <- seq(0, sqrt(0.75), length.out = resolution)
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

