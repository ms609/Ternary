test_that("Coordinates are reflected correctly", {
  expect_equal(
    ReflectedEquivalents(0.01, 0.8, direction = 1)[[1]],
    matrix(
      c(
        -0.06217968, 0.84167296,
        0.05217968, 0.82435245,
        0.01000000, -0.80000000,
        1.43782032, -0.02435245,
        -1.44782032, -0.04167296,
        0.01000000, 0.93205081,
        -1.44782032, 0.04167296,
        0.05217968, 0.90769836,
        0.05217968, -0.82435245,
        -0.06217968, 0.89037785,
        1.43782032, 0.02435245,
        -0.06217968, -0.84167296
      ),
      ncol = 2, byrow = TRUE
    )
  )

  expect_equal(
    round(ReflectedEquivalents(0.8, 0.01, direction = 2)[[1]], 5L),
    matrix(c(
      0.82435, 0.05218,
      0.84167, -0.06218,
      -0.80000, 0.01000,
      -0.04167, -1.44782,
      -0.02435, 1.43782,
      0.93205, 0.01000,
      0.02435, 1.43782,
      0.89038, -0.06218,
      -0.84167, -0.06218,
      0.90770, 0.05218,
      0.04167, -1.44782,
      -0.82435, 0.05218
    ), ncol = 2, byrow = TRUE)
  )

  expect_equal(
    round(ReflectedEquivalents(0.01, -0.8, direction = 3)[[1]], 5),
    matrix(c(
      0.05218, -0.82435,
      -0.06218, -0.84167,
      0.01000, 0.80000,
      -1.44782, 0.04167,
      1.43782, 0.02435,
      0.01000, -0.93205,
      1.43782, -0.02435,
      -0.06218, -0.89038,
      -0.06218, 0.84167,
      0.05218, -0.90770,
      -1.44782, -0.04167,
      0.05218, 0.82435
    ), ncol = 2, byrow = TRUE)
  )

  expect_equal(
    round(ReflectedEquivalents(-0.13, 0.375, direction = 4)[[1]], 5L),
    matrix(c(
      -0.17325, 0.44992,
      -0.82277, -0.82492,
      0.13000, 0.37500,
      0.69277, -1.05008,
      0.04325, 0.67508,
      -1.60205, 0.37500,
      -0.04325, 0.67508,
      -0.90928, -0.82492,
      0.82277, -0.82492,
      -1.55880, 0.44992,
      -0.69277, -1.05008,
      0.17325, 0.44992
    ), ncol = 2, byrow = TRUE)
  )
})

test_that("matrices can be coordinated", {
  abc <- matrix(1:12, 3)
  value <- TernaryToXY(abc)
  expect_equal(apply(abc, 2, TernaryToXY), unname(value))
  expect_equal(c("x", "y"), rownames(value))
})

test_that("Ranges are correct", {
  expect_equal(c(-1, 1) / 2, TernaryXRange(direction = 1))
  expect_equal(c(-1, 1) / 2, TernaryYRange(direction = 2))
  expect_equal(c(-1, 1) / 2, TernaryXRange(direction = 3))
  expect_equal(c(-1, 1) / 2, TernaryYRange(direction = 4))
  expect_equal(c(-1, 0) + ((1 - sqrt(0.75)) / 2), TernaryYRange(direction = 3))
  expect_equal(c(0, 1) - ((1 - sqrt(0.75)) / 2), TernaryXRange(direction = 2))
  expect_equal(c(-1, 0) + ((1 - sqrt(0.75)) / 2), TernaryXRange(direction = 4))
})

test_that("Coordination supports ranges", {
  expect_equal(
    TernaryToXY(c(20, 15, 65), region = rbind(c(0, 0, 50), c(50, 50, 100))),
    TernaryToXY(c(40, 30, 30))
  )
})


test_that("Regions are supported both ways", {
  my_corners <- list(c(22, 66, 12), c(22, 72, 6), c(15, 80, 5), c(12, 76, 12))
  expect_equal(
    TernaryCoords(22, 72, 6, # my_corners[[2]]
                  region = .SetRegion(my_corners, prettify = 10, set = FALSE)),
    c(1/9, 0.4811252),
    tolerance = 0.001
  )
  
  expect_equal(
    TernaryCoords(my_corners[[2]], region = my_corners),
    TernaryToXY(
      22, 72, 6,
      region = .SetRegion(my_corners, prettify = NA_integer_, set = FALSE)
    ),
    tolerance = 0.001
  )
  
  prior <- .SetRegion(my_corners, prettify = 10)
  on.exit(options(prior))
  expect_equal(
    TernaryToXY(22, 72, 6),
    c(1/9, 0.4811252),
    tolerance = 0.001
  )
  expect_equal(
    unname(XYToTernary(1/9, 0.4811252)),
    cbind(my_corners[[2]] / 100),
    tolerance = 0.001
  )
})

test_that("OutsidePlot() works", {
  options("ternDirection" = 1L)
  expect_true(OutsidePlot(100, 100))
  expect_false(OutsidePlot(0, 0))
  expect_equal(OutsidePlot(0:1, 1:0), c(TRUE, TRUE))
  expect_false(OutsidePlot(0, 0.8))
  expect_true(OutsidePlot(0, 0.8, tolerance = 0.05))
})

test_that("Corner cases are correct", {
  expect_equal(TernaryToXY(c(0, 0, 0)), TernaryToXY(c(2, 2, 2)))
})

test_that("Errors are handled nicely", {
  expect_error(TernaryCoords(1:3, direction = 5))
  expect_error(TernaryXRange(direction = 5))
  expect_error(TernaryYRange(direction = 5))
  expect_error(XYToTernary(1:2, 1:2, direction = 5))
  expect_error(XYToTernary(letters[1:2], 1:2))
  expect_error(XYToTernary(1:2, letters[1:2]))
})
