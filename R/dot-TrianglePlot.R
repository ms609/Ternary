.Triplicate <- function (x) if (length(x) == 1) rep(x, 3) else x

.ValidateGridLines <- function (grid.lines) {
  if (!is.integer(grid.lines)) {
    ceiling(grid.lines)
  } else {
    grid.lines
  }
}

.TrianglePlot <- function (atip, btip, ctip,
                           alab, blab, clab,
                           atip.pos, btip.pos, ctip.pos,
                           atip.rotate, btip.rotate, ctip.rotate,
                           axis.col,
                           axis.cex,
                           axis.labels,
                           axis.lty,
                           axis.font,
                           axis.lwd,
                           axis.pos,
                           axis.rotate,
                           axis.tick,
                           col,
                           grid.lines,
                           grid.col,
                           grid.lwd,
                           grid.lty,
                           grid.minor.col,
                           grid.minor.lines,
                           grid.minor.lty,
                           grid.minor.lwd,
                           lab.col,
                           lab.cex,
                           lab.font,
                           lab.offset,
                           isometric,
                           padding,
                           point,
                           sideOrder = 1:3,
                           ticks.col,
                           ticks.incline,
                           ticks.length,
                           ticks.lwd,
                           tip.col,
                           tip.cex,
                           tip.font,
                           xlim, ylim,
                           ...) {
  direction <- 1L + (pmatch(tolower(point), c("right", "down", "left", "up",
                                              "east", "south", "west", "north",
                                              2L, 3L, 4L, 1L)) %% 4L)
  if (is.na(direction)) {
    stop("`point` must be one of up, down, left or right")
  } else {
    options("ternDirection" = direction)
  }
  
  axis.rotate <- .Triplicate(axis.rotate)
  axis.pos <- .Triplicate(axis.pos)
  ticks.incline <- .Triplicate(ticks.incline)
  
  if (isometric) {
    
    if (is.null(xlim) && !is.null(ylim)) {
      xlim <- TernaryXRange(direction) * (ylim[2] - ylim[1])
    }
    xRange <- xlim[2] - xlim[1]
    if (is.null(ylim) && !is.null(xlim)) {
      ylim <- TernaryYRange(direction) * xRange
    }
    yRange <- ylim[2] - ylim[1]
    
    if (length(xlim) > 0 && abs(xRange - yRange) > 1e-07) {
      if (abs(xRange) < abs(yRange)) {
        xlim <- xlim * (yRange / xRange)
        warning("x range < y range, but isometric = TRUE; setting xlim = c(", 
                xlim[1], ", ", xlim[2], ")")
      } else {
        ylim <- ylim * (xRange / yRange)
        warning("x range > y range, but isometric = TRUE; setting ylim = c(", 
                ylim[1], ", ", ylim[2], ")")
      }
    }
  }
  if (is.null(xlim)) xlim <- TernaryXRange(direction)
  if (is.null(ylim)) ylim <- TernaryYRange(direction)
  
  
  axes <- vapply(list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 0, 0)),
                 TernaryCoords, double(2))
  
  axisBasis <- ifelse(ticks.incline, c(180, 300, 60), c(240, 0, 120))
  axisDegrees <- (axisBasis + (direction * 90)) %% 360
  
  axisRotation <- ifelse(ticks.incline,
    switch(direction,
           c(0, 60, -60),
           c(270, -30, 30),
           c(0, 60, -60),
           c(90, -30, 30)),
    switch(direction,
           c(-60, 0, 60),
           c(30, -90, -30),
           c(-60, 0, 60),
           c(30, -90, -30))
  )
  
  if (is.logical(axis.rotate)) {
    axisRotation <- ifelse(axis.rotate, axisRotation, 0)
  } else {
    axisRotation <- axis.rotate
  }
  
  if (!is.null(axis.pos)) {
    axisPosition <- axis.pos
  } else {
    axisPosition <- ifelse(ticks.incline,
      switch(direction,
             c(2, 4, 4),
             c(2, 4, 2),
             c(4, 2, 2),
             c(2, 2, 4)),
      switch(direction, 
             c(2, 4, 2),
             c(4, 4, 2),
             c(4, 2, 4),
             c(2, 2, 4))
    )
    
    if (is.logical(axis.rotate)) {
      pos.unrotated <- switch(direction, 
                              c(2, 4, 1),
                              c(3, 1, 2),
                              c(4, 2, 3),
                              c(1, 3, 4))
      axisPosition <- ifelse(axis.rotate, axisPosition, pos.unrotated)
    }
  }
  
  axisMult <- ifelse(ticks.incline,
    switch(direction, c(5, 5, 16), c(16, 9, 8), c(10, 8, 16), c(12, 9, 8)) / 10,
    switch(direction, c(5, 4, 8), c(4, 6, 4), c(4, 4, 7), c(5, 7, 3)) / 5)


  if (is.null(atip.rotate)) {
    atip.rotate <- switch(direction, 0, 30, 0,
                          ifelse(ticks.incline[1], 330, 30))
    atip.pos <- ifelse(ticks.incline[1],
                       switch(direction, 2, 2, 4, 4),
                       switch(direction, 4, 2, 2, 4))

  }
  if (is.null(btip.rotate)) {
    btip.rotate <- switch(direction, 0, 0, 0, 0)
    btip.pos <- switch(direction, 2, 4, 4, 2)
  }
  if (is.null(ctip.rotate)) {
    ctip.rotate <- switch(direction, 0, 0, 0, 0)
    ctip.pos <- switch(direction, 4, 4, 2, 2)
  }


  # Return:
  structure(list(
    alab = alab,
    blab = blab,
    clab = clab,
    atip = atip,
    btip = btip,
    ctip = ctip,
    atip.pos = atip.pos,
    btip.pos = btip.pos,
    ctip.pos = ctip.pos,
    atip.rotate = atip.rotate,
    btip.rotate = btip.rotate,
    ctip.rotate = ctip.rotate,

    axes = axes,
    axesX = axes[1, ],
    axesY = axes[2, ],

    axisDegrees = axisDegrees,
    axisMult = axisMult,
    axisPosition = axisPosition,
    axisRadians = axisDegrees * pi / 180,
    axisRotation = axisRotation,

    axis.col = .Triplicate(axis.col),
    axis.cex = .Triplicate(axis.cex),
    axis.font = .Triplicate(axis.font),
    axis.labels = axis.labels,
    axis.lty = .Triplicate(axis.lty),
    axis.lwd = .Triplicate(axis.lwd),
    axis.pos = axis.pos,
    axis.rotate = axis.rotate,
    axis.tick = axis.tick,

    col = col,

    direction = direction,

    lab.col = .Triplicate(lab.col),
    lab.cex = .Triplicate(lab.cex),
    lab.font = .Triplicate(lab.font),
    lab.offset = .Triplicate(lab.offset),


    gridExists = .GridExists(grid.lines),
    gridPoints = seq(from = 0, to = 1, length.out = grid.lines + 1L),

    grid.lines = .ValidateGridLines(grid.lines),
    grid.col = .Triplicate(grid.col),
    grid.lwd = .Triplicate(grid.lwd),
    grid.lty = .Triplicate(grid.lty),
    grid.minor.lines = .ValidateGridLines(grid.minor.lines),
    grid.minor.col = .Triplicate(grid.minor.col),
    grid.minor.lty = .Triplicate(grid.minor.lty),
    grid.minor.lwd = .Triplicate(grid.minor.lwd),

    padding = padding,

    sideOrder = sideOrder,

    ticks.col = .Triplicate(ticks.col),
    ticks.incline = ticks.incline,
    ticks.length = .Triplicate(ticks.length),
    ticks.lwd = .Triplicate(ticks.lwd),

    tip.col = .Triplicate(tip.col),
    tip.cex = .Triplicate(tip.cex),
    tip.font = .Triplicate(tip.font),

    xlim = xlim,

    ylim = ylim,
    ...), class = "TernaryPlot")
}
