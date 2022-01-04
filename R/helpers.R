.StartPlot <- function (tern, ...) {
  padVec <- c(-1, 1) * tern$padding
  
  plot(0, type = 'n', axes = FALSE, xlab = '', ylab = '',
       xlim = tern$xlim + padVec,
       ylim = tern$ylim + padVec, ...)
}

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

.PlotBackground <- function (tern) {
  polygon(tern$axesX, tern$axesY, col = tern$col, border = NA)
}

.GridExists <- function (grid.lines) {
  !is.null(grid.lines) && !is.na(grid.lines) && grid.lines > 1L
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

.PlotAxisTicks <- function (tern = getOption('.Last.triangle')) {
  if (tern$gridExists && tern$axis.tick) {
    lapply(seq_along(tern$gridPoints), function (i) {
      p <- tern$gridPoints[i]
      q <- 1 - p
      gridEnds <- vapply(list(c(p, 0, q), c(q, p, 0), c(0, q, p)),
                         TernaryCoords, double(2))
      lapply(1:3, .AxisTick, gridEnds)
    })
    
  }
}

.AxisLabel <- function (side, lineEnds, lab,
                        tern = getOption('.Last.triangle')) {
  selected <- tern$sideOrder[side]
  lng <- tern$ticks.length[side] * tern$axisMult[side]
  text(lineEnds[1, side] + sin(tern$axisRadians[side]) * lng,
       lineEnds[2, side] + cos(tern$axisRadians[side]) * lng,
       lab, srt = tern$axisRotation[side],
       pos = tern$axisPosition[side],
       font = tern$axis.font[selected],
       cex = tern$axis.cex[selected],
       col = tern$lab.col[selected])
}

.AxisTick <- function (side, lineEnds, tern = getOption('.Last.triangle')) {
  
  len <- tern$ticks.length
  col <- tern$ticks.col
  lwd <- tern$ticks.lwd
  
  selected <- tern$sideOrder[side]
  
  lines(lineEnds[1, side] + 
          c(0, sin(tern$axisRadians[side]) * len[side]),
        lineEnds[2, side] + 
          c(0, cos(tern$axisRadians[side]) * len[side]),
        col = col[selected], lwd = lwd[selected])
}

.PlotAxisLabels <- function (tern) {
  if (tern$gridExists) {
    lab <- tern$axis.labels
    
    lapply(seq_along(tern$gridPoints), function (i) {
      p <- tern$gridPoints[i]
      q <- 1 - p
      lineEnds <- vapply(list(c(p, 0, q), c(q, p, 0), c(0, q, p)),
                         TernaryCoords, double(2))
      
      if (length(lab) > 1 || lab != FALSE) {
        if (length(lab) == 1) {
          lab <- round(tern$gridPoints * 100, 1)
        }
        if (!is.null(tern$grid.lines) &&
            length(lab) == tern$grid.lines) {
          lab <- c('', lab)
        }
        if (!tern$ticks.incline[1]){
          lab <- rev(lab)
        }
        
        # Annotate axes
        lapply(1:3, .AxisLabel, lineEnds, lab = lab[i])
      }
    })
  }
}

.TitleAxis <- function (side, tern = getOption('.Last.triangle')) {
  selected <- tern$sideOrder[side]
  
  
  loff <- tern$lab.offset[selected]
  .DirectionalOffset <- function (degrees) {
    c(sin(degrees * pi / 180), cos(degrees * pi/ 180))
  }
  do <- matrix(c(300,  60, 120, 210, 
                  60, 120, 210, 330, 
                 180, 270,   0,  90), 4, 3)
  
  rot <- matrix(c( 60, 330,  60, 330, 
                  300,  30, 300,  30,
                    0,  90,   0, 270), 4, 3)
  
  xy <- TernaryCoords(switch(side, c(1, 0, 1), c(1, 1, 0), c(0, 1, 1))) + 
    (loff * .DirectionalOffset(do[tern$direction, side]))
  
  text(xy[1], xy[2], switch(selected, tern$alab, tern$blab, tern$clab),
       cex = tern$lab.cex[selected],
       font = tern$lab.font[selected],
       srt = rot[tern$direction, side],
       col = tern$lab.col[selected])
}

.AxisLines <- function (side, tern = getOption('.Last.triangle')) {
  selected <- tern$sideOrder[side]
  
  spots <- switch(side, 3:4, 1:2, 2:3)
  lines(tern$axesX[spots], tern$axesY[spots],
        col = tern$axis.col[selected],
        lty = tern$axis.lty[selected], 
        lwd = tern$axis.lwd[selected])
}

.TitleCorners <- function (side, tern = getOption('.Last.triangle')) {
  clockwise <- tern$ticks.incline[1]
  direction <- tern$direction
  len <- tern$ticks.length
  
  
  ax <- ifelse(clockwise,
               switch(direction, -4, 4, 1, -3),
               switch(direction, 4, 4, -1, -3)) * len[1]

  ay <- ifelse(clockwise,
               switch(direction, 1, -4, -2, -4),
               switch(direction, 1, -4, -2, 4)) * len[1]

  
  bx <- c(4, 4, -2, -3)[direction] * len[2]
  by <- c(-4, -2, 4, 2.4)[direction] * len[2]
  cx <- c(-3, 0, 2, -3)[direction] * len[3]
  cy <- c(-4, 2, 4, -2)[direction] * len[3]
  
  # Title corners
  .TitleCorner(1, tern, ax, ay, tern$atip, tern$atip.pos, srt = tern$atip.rotate)
  .TitleCorner(2, tern, bx, by, tern$btip, tern$btip.pos, srt = tern$btip.rotate)
  .TitleCorner(3, tern, cx, cy, tern$ctip, tern$ctip.pos, srt = tern$ctip.rotate)
  
}

.TitleCorner <- function (side, tern, x, y, tip, pos, srt) {
  text(tern$axesX[side] + x, tern$axesY[side] + y, tip, pos = pos, srt = srt,
       cex = tern$tip.cex[side],
       font = tern$tip.font[side],
       col = tern$tip.col[side])
}