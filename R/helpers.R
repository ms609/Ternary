.IsometricXLim <- function (xlim, ylim, direction) {
  if (is.null(xlim) && !is.null(ylim)) {
    TernaryXRange(direction) * (ylim[2] - ylim[1])
  } else {
    xlim
  }
}

.IsometricYLim <- function (xlim, ylim, direction) {
  if (is.null(ylim) && !is.null(xlim)) {
    ylim <- TernaryYRange(direction) * .LimRange(xlim)
  }
  
  xRange <- .LimRange(xlim)
  yRange <- .LimRange(ylim)

  if (length(xlim) > 0 && abs(xRange - yRange) > 1e-07) {
    if (abs(xRange) > abs(yRange)) {
      ylim <- ylim * (xRange / yRange)
      warning("x range > y range, but isometric = TRUE; setting ylim = c(", 
              ylim[1], ', ', ylim[2], ")")
    }
  }
  
  # Return:
  ylim
}

.LimRange <- function (lim) {
  lim[2] - lim[1]
}

.CheckIsometricXRange <- function (xlim, ylim) {
  xRange <- .LimRange(xlim)
  yRange <- .LimRange(ylim)
  
  if (length(xlim) > 0 && abs(xRange - yRange) > 1e-07) {
    if (abs(xRange) < abs(yRange)) {
      xlim <- xlim * (yRange / xRange)
      warning("x range < y range, but isometric = TRUE; setting xlim = c(", 
              xlim[1], ', ', xlim[2], ")")
    } else {
      stop("Unhandled exception: x range > y range, but isometric = TRUE;",
           "should have set ylim = c(", ylim[1], ', ', ylim[2], ")")
    }
  }
  
  xlim
}

.Axes <- function () {
  vapply(list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 0, 0)),
         TernaryCoords, 
         double(2))
}
.AxesX <- function () .Axes()[1, ]
.AxesY <- function () .Axes()[2, ]

.PlotBackground <- function (col) {
  polygon(.AxesX(), .AxesY(), col = col, border = NA)
}

.PlotGrid <- function (p, col, lty, lwd) {
  q <- 1 - p
  lineEnds <- vapply(list(c(p, q, 0), c(p, 0, q),
                          c(0, p, q), c(q, p, 0),
                          c(q, 0, p), c(0, q, p)),
                     TernaryCoords, double(2))
  lapply(list(c(1, 2), c(3, 4), c(5, 6)), function (i) 
    lines(lineEnds[1, i], lineEnds[2, i], col = col[i[2]/2], 
          lty = lty[i[2]/2], lwd = lwd[i[2]/2]))
  NULL
}

.GridExists <- function (grid.lines) {
  !is.null(grid.lines) && !is.na(grid.lines) && grid.lines > 1L
}

.PlotMinorGridLines <- function (grid.lines, grid.minor.lines, ...) {
  if (.GridExists(grid.lines) && grid.minor.lines > 0L) {
    nMinorLines <- grid.lines * (grid.minor.lines + 1L)  + 1L
    minorLinePoints <- seq(from = 0, to = 1, length.out = 
                             nMinorLines)[-seq(from = 1, to = nMinorLines, 
                                               by = grid.minor.lines + 1L)]
    lapply(minorLinePoints, .PlotGrid, ...)
  }
}

.PlotMajorGridLines <- function (grid.lines, ...) {
  if (.GridExists(grid.lines)) {
    linePoints <- seq(from = 0, to = 1, length.out = grid.lines + 1L)
    lapply(linePoints[-c(1, grid.lines + 1L)], .PlotGrid, ...)
  }
}

.Triplicate <- function (x) if (length(x) == 1) rep(x, 3) else x

.ValidateGridLines <- function (grid.lines) {
  if (!is.integer(grid.lines)) {
    grid.lines <- ceiling(grid.lines)
  }
}

