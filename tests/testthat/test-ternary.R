context("Ternary plotting")
test_that("Errors are handled gracefully", {
  expect_error(TernaryCoords(c(1, 2)))
  expect_error(TernaryCoords(c("a", "b", "c")))
})

test_that("TernaryCoords gives correct coordinates", {
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
  expect_null(TernaryPlot('A', 'B', 'C'))
})
