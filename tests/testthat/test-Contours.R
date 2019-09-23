context('Contour plotting')
library('vdiffr')

test_that('Densities are correctly calculated', {
  
  coordinates <- list(middle = c(1, 1, 1),
                      top = c(1, 0, 0),
                      belowTop = c(2, 1, 1),
                      leftSideSolid = c(9, 2, 9),
                      leftSideSolid2 = c(9, 2, 9) / 2,
                      right3way = c(1, 2, 0),
                      rightEdge = c(2.5, 0.5, 0),
                      leftBorder = c(1, 1, 4),
                      topBorder = c(2, 1, 3),
                      rightBorder = c(1, 2, 3)
  )
  
  values <- TernaryDensity(coordinates, resolution=3L, direction=1L)
  expect_equal(c(3, 10, 4, 3, 2, 16, 7, 3, 12),
               values['z', ])
  
})

test_that('Contours are plotted', {
  
  Contours <- function () {
    par(mar=rep(0, 4), mfrow=c(2, 2))
    
    FunctionToContour <- function (a, b, c) {
      a - c + (4 * a * b) + (27 * a * b * c)
    }
    
    TernaryPlot(alab = 'a', blab = 'b', clab = 'c', point = 1L)
    ColourTernary(TernaryPointValues(FunctionToContour, resolution=6L))
    TernaryContour(FunctionToContour, resolution=12L)
    
    TernaryPlot(alab = 'a', blab = 'b', clab = 'c', point = 2L)
    ColourTernary(TernaryPointValues(FunctionToContour, resolution=6L))
    TernaryContour(FunctionToContour, resolution=12L)
    
    TernaryPlot(alab = 'a', blab = 'b', clab = 'c', point = 3L)
    ColourTernary(TernaryPointValues(FunctionToContour, resolution=6L))
    TernaryContour(FunctionToContour, resolution=12L)
    
    TernaryPlot(alab = 'a', blab = 'b', clab = 'c', point = 4L)
    ColourTernary(TernaryPointValues(FunctionToContour, resolution=6L))
    TernaryContour(FunctionToContour, resolution=12L)
  }
  expect_doppelganger('Contours', Contours)
  
  ContoursSkiwiff <- function () {
    FunctionToContour <- function (a, b, c) {
      a - c + (4 * a * b) + (27 * a * b * c)
    }
    SubTest <- function (direction) {
      ColourTernary(TernaryPointValues(FunctionToContour, resolution=6L, 
                                       direction = direction))
      TernaryContour(FunctionToContour, resolution = 12L, direction = direction)
    }
    
    par(mar=rep(0, 4), mfrow=c(2, 2))
    TernaryPlot(point = 3L, ylim=c(0, 1))
    SubTest(1)
    
    TernaryPlot(point = 4L, xlim=c(0, 1))
    SubTest(2)
    
    TernaryPlot(point = 1L, ylim=c(-1, 0))
    SubTest(3)
    
    TernaryPlot(point = 2L, xlim=c(-1, 0))
    SubTest(4)
  }
  expect_doppelganger('Contours-skiwiff', ContoursSkiwiff)
  
  
  DensityContours <- function () {
    par(mar=rep(0.2, 4))
    TernaryPlot()
    
    nPoints <- 400L
    set.seed(0)
    coordinates <- cbind(abs(rnorm(nPoints, 2, 3)),
                         abs(rnorm(nPoints, 1, 1.5)),
                         abs(rnorm(nPoints, 1, 0.5)))
    
    ColourTernary(TernaryDensity(coordinates, resolution=10L))
    TernaryPoints(coordinates, col='red', pch='.')
    TernaryDensityContour(coordinates, resolution=10L)
  }
  expect_doppelganger('density-contours', DensityContours)
  
  
  DensityContours2 <- function () {
    par(mar=rep(0.2, 4))
    TernaryPlot(point=2)
    
    nPoints <- 400L
    set.seed(0)
    coordinates <- cbind(abs(rnorm(nPoints, 2, 3)),
                         abs(rnorm(nPoints, 1, 1.5)),
                         abs(rnorm(nPoints, 1, 0.5)))
    
    TernaryPoints(coordinates, col='red', pch='.')
    TernaryDensityContour(coordinates, resolution=10L, edgeCorrection = FALSE)
  }
  expect_doppelganger('density-contours-2', DensityContours2)
  
  
  
  DensityContours3 <- function () {
    par(mar=rep(0.2, 4))
    TernaryPlot(point=3)
    
    nPoints <- 400L
    set.seed(0)
    coordinates <- cbind(abs(rnorm(nPoints, 2, 3)),
                         abs(rnorm(nPoints, 1, 1.5)),
                         abs(rnorm(nPoints, 1, 0.5)))
    
    TernaryPoints(coordinates, col='red', pch='.')
    TernaryDensityContour(coordinates, resolution=10L)
  }
  expect_doppelganger('density-contours-3', DensityContours3)
  
  LoResDensCont <- function () {
    coordinates <- list(middle = c(1, 1, 1),
                        top = c(3, 0, 0),
                        belowTop = c(2, 1, 1),
                        leftSideSolid = c(9, 2, 9),
                        leftSideSolid2 = c(9.5, 2, 8.5),
                        right3way = c(1, 2, 0),
                        rightEdge = c(2.5, 0.5, 0),
                        leftBorder = c(1, 1, 4),
                        topBorder = c(2, 1, 3),
                        rightBorder = c(1, 2, 3)
    )
    par(mfrow=c(2, 2), mar=rep(0.2, 4))
    TernaryPlot(grid.lines=3, axis.labels=1:3, point='up')
    values <- TernaryDensity(coordinates, resolution=3L)
    ColourTernary(values)
    TernaryPoints(coordinates, col='red')
    text(values[1, ], values[2, ], paste(values[3, ], '/ 6'), cex=0.8)
    
    TernaryPlot(grid.lines=3, axis.labels=1:3, point='right')
    values <- TernaryDensity(coordinates, resolution=3L)
    ColourTernary(values)
    TernaryPoints(coordinates, col='red')
    text(values[1, ], values[2, ], paste(values[3, ], '/ 6'), cex=0.8)
    
    TernaryPlot(grid.lines=3, axis.labels=1:3, point='down')
    values <- TernaryDensity(coordinates, resolution=3L)
    ColourTernary(values)
    TernaryPoints(coordinates, col='red')
    text(values[1, ], values[2, ], paste(values[3, ], '/ 6'), cex=0.8)
    
    TernaryPlot(grid.lines=3, axis.labels=1:3, point='left')
    values <- TernaryDensity(coordinates, resolution=3L)
    ColourTernary(values)
    TernaryPoints(coordinates, col='red')
    text(values[1, ], values[2, ], paste(values[3, ], '/ 6'), cex=0.8)
    
    TernaryDensityContour(t(vapply(coordinates, I, double(3L))), 
                          resolution=12L, tolerance=-0.02, col='orange')
  }
  expect_doppelganger('lo-res-density-contours', LoResDensCont)
  
})

test_that('Errors are handled', {
  # Postive bandwidths
  expect_error(TernaryDensityContour(rbind(c(1, 1, 1)), -1))
})
