#' Annotate points on a ternary plot
#' 
#' `Annotate()` identifies and label individual points on a ternary diagram
#' in the plot margins.
#' 
#' @inheritParams CoordinatesToXY
#' @param labels Character vector specifying text with which to annotate
#' each entry in `coordinates`.
#' @param side Optional vector specifying which side of the ternary
#' plot each point should be labelled on, using the notation `"a", "b", "c"` or
#' `1, 2, 3`.
#' Entries of `"n"` or `0` will not be annotated
#' (but still require an entry in `labels`).
#' Entries of `NA` will be allocated a side automatically,
#' based on the midpoint of `coordinates`.
#' @param outset Numeric specifying distance from plot margins to labels.
#' @param line.col,lty,lwd parameters to [`segments()`].
#' @param col,font,offset parameters to [`text()`].
#' @param \dots Further parameters to [`text()`] and [`segments()`].
#' 
#' @examples
#' # Load some data
#' data("Seatbelts")
#' seats <- c("drivers", "front", "rear")
#' seat <- Seatbelts[month.abb %in% "Oct", seats]
#' law <- Seatbelts[month.abb %in% "Oct", "law"]
#' 
#' # Set up plot
#' oPar <- par(mar = c(2, 0, 0, 0))
#' TernaryPlot(alab = seats[1], blab = seats[2], clab = seats[3])
#' TernaryPoints(seat, cex = 0.8, col = 2 + law)
#' 
#' # Annotate points by year
#' Annotate(seat, labels = 1969:1984, col = 2 + law)
#' 
#' # Restore original graphical parameters
#' par(oPar)  
#' @seealso [Annotation vignette](
#' https://ms609.github.io/Ternary/dev/articles/annotation.html) gives 
#' further suggestions for manual annotation.
#' @importFrom graphics segments text
#' @importFrom RcppHungarian HungarianSolver
#' @template MRS
#' @export
Annotate <- function(coordinates, labels, side, outset = 0.16,
                     line.col = col, lty = par("lty"), lwd = par("lwd"),
                     col = par("col"), font = par("font"), offset = 0.5,
                     ...) {
  xy <- CoordinatesToXY(coordinates)
  direction <- getOption("ternDirection", 1)
  n <- dim(xy)[2]
  if (missing(side)) {
    side <- rep_len(NA_integer_, n)
  } else if (is.character(side)) {
    side <- match(tolower(side), c("a", "b", "c", 0:3, "n")) %% 4
  } 
  if (any(is.na(side))) {
    middle <- rowMeans(xy) #apply(xy, 1, median)
    centred <- xy - middle
    angle <- atan2(centred[2, ], centred[1, ])
    corners <- CoordinatesToXY(diag(3)) - middle
    cornerAngles <- atan2(corners[2, ], corners[1, ])
    side[is.na(side)] <- switch(
      direction, 3:1, c(2, 1, 3), c(1, 3, 2), c(1, 3, 2))[
      1 + ((as.double(cut(angle, c(-pi, cornerAngles, pi)))) %% 3)][
        is.na(side)]
  }
  side <- rep_len(side, n)
  region <- options("ternRegion")[["ternRegion"]][, 1:3]
  inset <- (region["max", ] - region["min", ]) / 10
  mins <- region["min", ]
  low <- mins + inset
  high <- region["max", ] - inset
  ends <- TernaryCoords(
    cbind(c(mins[[1]], high[[2]], low[[3]]), c(mins[[1]], low[[2]], high[[3]]),
          c(low[[1]], mins[[2]], high[[3]]), c(high[[1]], mins[[2]], low[[3]]),
          c(high[[1]], low[[2]], mins[[3]]), c(low[[1]], high[[2]], mins[[3]]))
    ) +
    outset * switch(getOption("ternDirection", 1),
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
  if (missing(labels)) {
    labels <- seq_len((
      if (is.data.frame(coordinates) || is.matrix(coordinates)) nrow
      else if (is.list(coordinates)) length
      else function (x) length(x) / 3)(coordinates))
  }
  
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
