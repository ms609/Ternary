test_that("Polygon geometry", {
  # https://www.wikihow.com/Calculate-the-Area-of-a-Polygon
  x <- c(-3, -1, 6, 3, -4)
  y <- c(-2, 4, 1, 10, 9)
  expect_equal(PolygonArea(x, y), 60)
  expect_equal(PolygonArea(rev(x), rev(y)), 60) # not -60

  expect_equal(xy.coords(PolygonCentre(x, y))$x, 1.1 / 3)
  expect_equal(xy.coords(PolygonCenter(x, y))$y, 16.6 / 3)

  expect_equal(GrowPolygon(x, y, 0), xy.coords(x, y))
  expect_equal(GrowPolygon(c(-1, 3, 3, -1),
                           c(-1, -1, 3, 3),
                           sqrt(2)),
               list(x = c(-2, 4, 4, -2),
                    y = c(-2, -2, 4, 4),
                    xlab = NULL, ylab = NULL)
  )


  # From https://stackoverflow.com/questions/52244519
  # Note anti-clockwise specification & repeated final point
  dfr <- data.frame(x = c(2, 2.5, 4, 5, 4.5, 3, 2),
                    y = c(2, 3, 3.5, 3, 2.8, 2.5, 2))
  expect_equal(-PolygonArea(dfr), PolygonArea(dfr, positive = FALSE))
  cent <- PolygonCentre(dfr)
  expect_equal(point.in.polygon(cent[, "x"], cent[, "y"], dfr$x, dfr$y), 1L)
})
