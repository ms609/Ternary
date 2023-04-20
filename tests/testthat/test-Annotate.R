test_that("Annotate() works", {
  points <- list(c(1, 2, 7), c(1, 7, 2),
                 c(2, 1, 7), c(7, 1, 2),
                 c(2, 7, 1), c(7, 2, 1),
                 c(3, 3, 4), c(3, 4, 3)
  )
  
  AnnotateBasics <- function() {
    TernaryPlot()
    TernaryText(points, 1:8)
    Annotate(points[1:2], side = 1, 1:2, outset = 0.2)
    Annotate(points[3:4], side = "b", 3:4, offset= 1.5,
             col = "darkgreen", line.col = "#003399aa",
             font = 3:4, lwd = c(10, 2))
    Annotate(points[5:6], side = "3", labels = 5:6,  outset = 0)
  }

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("Annotate-basics", AnnotateBasics)
  
  AnnotateAutoLocate <- function() {
    TernaryPlot()
    Annotate(do.call(rbind, points),
             col = rainbow(8),
             offset = 2,
             font = 3,
             lwd = 1,
             lty = "dotted")
    Annotate(do.call(rbind, points),
             side = c("a", 1, "b", 0, "c", NA, NA, "N"),
             col = rainbow(8),
             lwd = 3)
  }
  
  vdiffr::expect_doppelganger("Annotate-auto-locate", AnnotateAutoLocate)
})
