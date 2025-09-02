test_that("Annotate() works", {
  points <- rbind(c(1, 2, 7), c(1, 7, 2),
                  c(2, 1, 7), c(7, 1, 2),
                  c(2, 7, 1), c(7, 2, 1),
                  c(3, 3, 4), c(3, 4, 3)
  )
  
  AnnotateBasics <- function() {
    ptsList <- .SimpleApply(points, 1, c)
    TernaryPlot()
    TernaryText(ptsList, 1:8)
    Annotate(ptsList[1:2], side = 1, outset = 0.2)
    Annotate(ptsList[3:4], side = "b", 3:4, offset= 1.5,
             col = "darkgreen", line.col = "#003399aa",
             font = 3:4, lwd = c(10, 2))
    Annotate(ptsList[5:6], side = "3", labels = 5:6,  outset = 0)
  }

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("Annotate-basics", AnnotateBasics)
  
  AnnotateAutoLocate <- function() {
    TernaryPlot()
    Annotate(points,
             col = rainbow(8),
             offset = 2,
             font = 3,
             lwd = 1,
             lty = "dotted")
    Annotate(as.data.frame(.SimpleApply(points, 2, c)),
             side = c("a", 1, "b", 0, "c", NA, NA, "N"),
             col = rainbow(8),
             lwd = 3)
  }
  
  vdiffr::expect_doppelganger("Annotate-auto-locate", AnnotateAutoLocate)
  
  vdiffr::expect_doppelganger("Annotate-zoomed", function() {
    my_corners <- list(c(22, 66, 12), c(22, 72, 6), c(15, 80, 5), c(12, 76, 12))
    TernaryPlot(region = my_corners)
    TernaryPolygon(my_corners, col = "#2cbe4e88")
    Annotate(my_corners, 1:4)
  })
})
