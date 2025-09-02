test_that("Polygon geometry", {
  x <- c(-3, -1, 6, 3, -4)
  y <- c(-2, 4, 1, 10, 9)
  expect_warning(expect_equal(PolygonArea(x, y), 60), "deprecated")
  expect_warning(expect_equal(PolygonArea(rev(x), rev(y)), 60), "deprecated")
  
  expect_warning(expect_equal(xy.coords(PolygonCentre(x, y))$x, 1.1 / 3),
                 "deprecated")
  expect_warning(expect_equal(xy.coords(PolygonCenter(x, y))$y, 16.6 / 3),
               "deprecated")
  
  expect_warning(expect_equal(GrowPolygon(x, y, 0), xy.coords(x, y)), "deprecated")
  
  dfr <- data.frame(x = c(2, 2.5, 4, 5, 4.5, 3, 2),
                    y = c(2, 3, 3.5, 3, 2.8, 2.5, 2))
  expect_warning(expect_equal(-PolygonArea(dfr), 
                              PolygonArea(dfr, positive = FALSE)),
                 "deprecated")
})
