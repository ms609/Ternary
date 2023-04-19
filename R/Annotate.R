#' Annotate individual points on a ternary diagram
#' 
#' @template coordinatesParam
#' @param labels Character vector of same length as `coordinates`
#' specifying text with which to annotate each point.
#' @param side Optional integer vector specifying which side of the ternary
#' plot each point should be labelled on. Points labelled 0 will not be
#' annotated (but still require an entry in `annotation`).
#' @param line.col,lty,lwd parameters to [`segments()`].
#' @param col,font,offset parameters to [`text()`].
#' @param \dots Further parameters to [`text()`] and [`segments()`].
#' 
#' @examples
#' data("Seatbelts")
#' seats <- c("drivers", "front", "rear")
#' seat <- Seatbelts[month.abb %in% "Oct", seats]
#' TernaryPlot(alab = seats[1], blab = seats[2], clab = seats[3])
#' TernaryPoints(seat, pch = 16, cex = 0.5)
#' Annotate(seat[1, ], side = 1)
#' Annotate(seat, labels = 1969:1984)
#' 
#'  
#' @importFrom graphics segments text
#' @importFrom RcppHungarian HungarianSolver
#' @template MRS
#' @export
Annotate <- function(coordinates, labels, side,
                     line.col = col, lty = par("lty"), lwd = par("lwd"),
                     col = par("col"), font = par("font"), offset = 0.5,
                     ...) {
  xy <- CoordinatesToXY(coordinates)
  direction <- getOption("ternDirection", 1)
  n <- dim(xy)[2]
  if (missing(side)) {
    middle <- rowMeans(xy)#apply(xy, 1, median)
    centred <- xy - middle
    angle <- atan2(centred[2, ], centred[1, ])
    corners <- CoordinatesToXY(diag(3)) - middle
    cornerAngles <- atan2(corners[2, ], corners[1, ])
    side <- switch(direction, 3:1, c(2, 1, 3), c(1, 3, 2), c(1, 3, 2))[
      1 + ((as.double(cut(angle, c(-pi, cornerAngles, pi)))) %% 3)]
  } else {
    if (is.character(side)) {
      side <- 1 + ((match(tolower(side), c("a", "b", "c", 1:3)) - 1) %% 3)
    }
    side <- rep_len(side, n)
  }
  ends <- TernaryCoords(cbind(c(0, 90, 10), c(0, 10, 90),
                              c(10, 0, 90), c(90, 0, 10),
                              c(90, 10, 0), c(10, 90, 0))) + 
    0.12 * switch(getOption("ternDirection", 1),
                  c(0, -1, 0, -1, -1, 0, -1, 0, 1, 0, 1, 0),
                  c(-1, 0, -1, 0, 0, 1, 0, 1, 0, -1, 0, -1),
                  c(0, 1, 0, 1, 1, 0, 1, 0, -1, 0, -1, 0),
                  c(1, 0, 1, 0, 0, -1, 0, -1, 0, 1, 0, 1)
                  )
  nAnchors <- table(side, dnn = NULL)
  nAnchors <- vapply(
    as.character(1:3),
    function(i) if (i %in% names(nAnchors)) nAnchors[[i]] else 0L,
    0L
  )
  anchors <- list(
    rbind(x = seq(ends["x", 1], ends["x", 2], length.out = nAnchors[1]),
          y = seq(ends["y", 1], ends["y", 2], length.out = nAnchors[1])),
    rbind(x = seq(ends["x", 3], ends["x", 4], length.out = nAnchors[2]),
          y = seq(ends["y", 3], ends["y", 4], length.out = nAnchors[2])),
    rbind(x = seq(ends["x", 5], ends["x", 6], length.out = nAnchors[3]),
          y = seq(ends["y", 5], ends["y", 6], length.out = nAnchors[3]))
  )
  pos <- switch(direction, c(4, 2, 4), c(2, 4, 4), c(4, 4, 2), c(4, 2, 2))
  srt <- switch(direction, c(-90, -60, 30), c(0, 30, -60), c(90, -60, 30),
                c(0, 30, -60))
  
  line.col <- rep_len(line.col, n)
  lty <- rep_len(lty, n)
  lwd <- rep_len(lwd, n)
  col <- rep_len(col, n)
  font <- rep_len(font, n)
  offset <- rep_len(offset, n)
  
  for (i in 1:3) {
    onSide <- side == i
    if (any(onSide)) {
      xyI <- xy[, onSide, drop = FALSE]
      anchorI <- anchors[[i]]
      diffX <- outer(xyI[1, ], anchorI[1, ], `-`)
      diffY <- outer(xyI[2, ], anchorI[2, ], `-`)
      euclid <- sqrt((diffX ^ 2) + (diffY ^ 2))
      matching <- RcppHungarian::HungarianSolver(euclid)$pairs
      anchorX <- anchorI[1, matching[, 2]]
      anchorY <- anchorI[2, matching[, 2]]
      segments(xyI[1, matching[, 1]], xyI[2, matching[, 1]],
               anchorX, anchorY,
               col = line.col[onSide], lwd = lwd[onSide], lty = lty[onSide],
               ...)
      text(anchorX, anchorY, labels[onSide],
           pos = pos[i],
           srt = srt[i],
           xpd = NA,
           col = col[onSide], font = font[onSide], offset = offset[onSide],
           ...)
    }
  }
}
