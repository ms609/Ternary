context("Ternary plotting")
test_that("Errors are handled gracefully", {
  expect_error(TernaryCoords(c(1, 2)))
  expect_error(TernaryCoords(c("a", "b", "c")))
})

test_that("TernaryCoords gives correct coordinates", {
  options('ternDirection' = 2)
  input <- list(
    c(1, 0, 0), 
    c(0, 2, 0),
    c(0, 0, 3),
    c(9, 9, 0),
    c(1, 1, 2)
  )
  output <- list(
    c(0, 0.5),
    c(0, -0.5),
    c(sin(pi/3), 0),
    c(0, 0),
    c(sin(pi/3)/2, 0)
  )
  expect_equal(output, lapply(input, TernaryCoords))
  expect_error(TernaryCoords(rep(1, 5), 1, 1))
  expect_equal(c(0, 0.5), TernaryCoords(1, 0, 0))
})

test_that("Ternary plotting does not fail", {
  expect_null(TernaryPlot('A', 'B', 'C'))
  expect_null(HorizontalGrid())
  expect_error(TernaryPoints(rep(1, 5)))
  expect_null(TernaryPoints(c(1,1,1)))
  expect_null(TernaryText(c(1,1,1), 'A'))
  expect_null(TernaryLines(list(c(1,1,1), c(0, 1, 2)), lwd=2))
  expect_null(TernaryPolygon(matrix(c(
    30, 40, 30,
    30, 30, 40,
    55, 20, 25
  ), ncol=3, byrow=TRUE)))
})
