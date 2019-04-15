context('Contour plotting')

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
