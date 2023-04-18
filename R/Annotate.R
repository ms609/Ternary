#' Annotate individual points on a ternary diagram
#' 
#' @template coordinatesParam
#' @param annotation Character vector of same length as `coordinates`
#' specifying text with which to annotate each point.
#' @param side Optional integer vector specifying which side of the ternary
#' plot each point should be labelled on. Points labelled 0 will not be
#' annotated (but still require an entry in `annotation`).
#' @param \dots Further parameters to [`text()`] and [`segments()`].
#' 
#' @examples
#' data("Seatbelts")
#' seats <- c("drivers", "front", "rear")
#' seat <- Seatbelts[month.abb %in% "Oct", seats]
#' TernaryPlot(alab = seats[1], blab = seats[2], clab = seats[3])
#' TernaryPoints(seat, pch = 16, cex = 0.5)
#' Annotate(seat, annotation = 1969:1984, col = )
#'  
#' @importFrom graphics segments text
#' @importFrom RcppHungarian HungarianSolver
#' @template MRS
#' @export
Annotate <- function(coordinates, annotation, side, ...) {
  xy <- CoordinatesToXY(coordinates)
  direction <- getOption("ternDirection", 1)
  if (missing(side)) {
    middle <- rowMeans(xy)#apply(xy, 1, median)
    centred <- xy - middle
    angle <- atan2(centred[2, ], centred[1, ])
    corners <- CoordinatesToXY(diag(3)) - middle
    cornerAngles <- atan2(corners[2, ], corners[1, ])
    side <- switch(direction, 3:1, c(2, 1, 3), c(1, 3, 2), c(1, 3, 2))[
      1 + ((as.double(cut(angle, c(-pi, cornerAngles, pi))) - 1) %% 3)]
  }
  if (is.character(side)) {
    side <- 1 + ((match(tolower(side), c("a", "b", "c", 1:3)) - 1) %% 3)
  }
  ends <- TernaryCoords(cbind(c(90, 10, 0), c(10, 90, 0),
                              c(0, 90, 10), c(0, 10, 90),
                              c(10, 0, 90), c(90, 0, 10))) + 
    0.12 * switch(getOption("ternDirection", 1),
                  c(1, 0, 1, 0, 0, -1, 0, -1, -1, 0, -1, 0),
                  c(0, -1, 0, -1, -1, 0, -1, 0, 0, 1, 0, 1),
                  c(-1, 0, -1, 0, 0, 1, 0, 1, 1, 0, 1, 0),
                  c(0, 1, 0, 1, 1, 0, 1, 0, 0, -1, 0, -1)
                  )
  nAnchors <- table(side, dnn = NULL)
  anchors <- list(
    rbind(x = seq(ends["x", 1], ends["x", 2], length.out = nAnchors[["1"]]),
          y = seq(ends["y", 1], ends["y", 2], length.out = nAnchors[["1"]])),
    rbind(x = seq(ends["x", 3], ends["x", 4], length.out = nAnchors[["2"]]),
          y = seq(ends["y", 3], ends["y", 4], length.out = nAnchors[["2"]])),
    rbind(x = seq(ends["x", 5], ends["x", 6], length.out = nAnchors[["3"]]),
          y = seq(ends["y", 5], ends["y", 6], length.out = nAnchors[["3"]]))
  )
  pos <- switch(direction, c(4, 4, 2), c(4, 2, 4), c(2, 4, 4), c(2, 4, 2))
  srt <- switch(direction, c(30, -90, -60), c(-60, 0, 30), c(30, 90, -60),
                c(-60, 0, 30))
  
  for (i in 1:3) {
    xyI <- xy[, side == i]
    anchorI <- anchors[[i]]
    diffX <- outer(xyI[1, ], anchorI[1, ], `-`)
    diffY <- outer(xyI[2, ], anchorI[2, ], `-`)
    euclid <- sqrt((diffX ^ 2) + (diffY ^ 2))
    matching <- RcppHungarian::HungarianSolver(euclid)$pairs
    anchorX <- anchorI[1, matching[, 2]]
    anchorY <- anchorI[2, matching[, 2]]
    segments(xyI[1, matching[, 1]], xyI[2, matching[, 1]],
             anchorX, anchorY, ...)
    text(anchorX, anchorY, annotation[side == i],
         pos = pos[i],
         srt = srt[i],
         ...)
  }
}
