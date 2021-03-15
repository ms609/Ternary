.IsometricXLim <- function (xlim, ylim, direction) {
  if (is.null(xlim) && !is.null(ylim)) {
    TernaryXRange(direction) * (ylim[2] - ylim[1])
  } else {
    xlim
  }
}

.IsometricYLim <- function (xlim, ylim, direction) {
  if (is.null(ylim) && !is.null(xlim)) {
    TernaryYRange(direction) * .LimRange(xlim)
  } else {
    ylim
  }
}

.LimRange <- function (lim) {
  lim[2] - lim[1]
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

.PlotMinorGridLines <- function (grid.lines, grid.minor.lines, ...) {
  if (grid.minor.lines > 0L) {
    nMinorLines <- grid.lines * (grid.minor.lines + 1L)  + 1L
    minorLinePoints <- seq(from = 0, to = 1, length.out = 
                             nMinorLines)[-seq(from = 1, to = nMinorLines, 
                                               by = grid.minor.lines + 1L)]
    lapply(minorLinePoints, .PlotGrid, ...)
  }
}

.PlotMajorGridLines <- function (grid.lines, ...) {
  linePoints <- seq(from = 0, to = 1, length.out = grid.lines + 1L)
  lapply(linePoints[-c(1, grid.lines + 1L)], .PlotGrid, ...)
}
