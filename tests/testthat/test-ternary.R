context("Ternary plotting")
test_that("Errors are handled gracefully", {
  expect_error(TernaryCoords(c(1, 2)))
  expect_error(TernaryCoords(c("a", "b", "c")))
})

test_that("TernaryCoords gives correct coordinates", {
  options('ternDirection' = 2L)
  input <- cbind(
    c(a = 0, b = 0, c = 1), 
    c(a = 0, b = 2, c = 0),
    c(a = 3, b = 0, c = 0),
    c(a = 0, b = 9, c = 9),
    c(a = 2, b = 1, c = 1)
  )
  listInput <- lapply(1:5, function(i) input[, i])
  output <- cbind(
    c(0, 0.5),
    c(0, -0.5),
    c(sin(pi/3), 0),
    c(0, 0),
    c(sin(pi/3)/2, 0)
  )
  expect_equal(output, apply(input, 2, TernaryCoords))
  expect_equal(output, CoordinatesToXY(listInput))
  expect_equal(output, CoordinatesToXY(input))
  expect_equal(matrix(output[, 1], 2, 1), CoordinatesToXY(input[, 1]))
  
  expect_equal(input / rep(colSums(input), each=3), XYToTernary(output[1, ], output[2, ]))
  expect_error(TernaryCoords(rep(1, 5), 1, 1))
  expect_equal(c(0, 0.5), TernaryCoords(0, 0, 1))
})

test_that("Ternary plotting does not fail", {
  expect_null(TernaryPlot('A', 'B', 'C'))
  expect_null(HorizontalGrid())
  expect_error(TernaryPoints(rep(1, 5)))
  expect_null(TernaryPoints(c(1, 1, 1)))
  expect_null(TernaryText(c(1, 1, 1), 'A'))
  expect_null(TernaryLines(list(c(1, 1, 1), c(0, 1, 2)), lwd=2))
  expect_null(TernaryPolygon(matrix(c(
    30, 40, 30,
    30, 30, 40,
    55, 20, 25
  ), ncol=3, byrow=TRUE)))
})
