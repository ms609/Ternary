ternRegionDefault <- cbind(a = c(0, 100), b = c(0, 100), c = c(0, 100))
ternRegion20 <- cbind(a = c(20, 80), b = c(20, 80), c = c(20, 80))
ternRegionA <- cbind(a = c(40, 100), b = c(0, 60), c = c(0, 60))

.RegionCorners <- function(region = getOption("ternRegion", ternRegionDefault)) {
  cbind(region[c(2, 3, 5)], region[c(1, 4, 5)], region[c(1, 3, 6)])
}

.RegionXY <- function(region = getOption("ternRegion", ternRegionDefault)) {
  apply(TernaryToXY(.RegionCorners(region), region = NULL), 1, range)
}

.Normalize <- function(x, range) {
  min <- min(range)
  max <- max(range)
  (x - min) / (max - min)
}

.Unnormalize <- function(x, range) {
  min <- min(range)
  max <- max(range)
  ((max - min) * x) + min
}

.NormalizeToRegion <- function(
    xy,
    region = getOption("ternRegion", ternRegionDefault)) {
  
  if (all(region == ternRegionDefault)) {
    xy
  } else {
    range <- .RegionXY(region)
    c(.Unnormalize(.Normalize(xy[1], TernaryXRange()), range[, "x"]),
      .Unnormalize(.Normalize(xy[2], TernaryYRange()), range[, "y"]))
  }
}

.NormalizeToRegionABC <- function(
    abc,
    region = getOption("ternRegion", ternRegionDefault)) {
  c(
    .Normalize(abc[1], region[[1]]),
    .Normalize(abc[2], region[[2]]),
    .Normalize(abc[3], region[[3]])
    )
}

.UnnormalizeFromRegion <- function(
    abc,
    region = getOption("ternRegion", ternRegionDefault)) {
  c(
    .Unnormalize(abc[1], region[[1]][1], region[[1]][2]),
    .Unnormalize(abc[2], region[[2]][1], region[[2]][2]),
    .Unnormalize(abc[3], region[[3]][1], region[[3]][2])
    )
}

#' @export
SetRegion <- function(region) UseMethod("SetRegion")

.RegionIsEquilateral <- function(region) {
  length(unique(apply(region, 2, max) - apply(region, 2, min))) == 1
}

.RegionIsValid <- function(region) {
  .RegionIsEquilateral(region)
}

#' @export
SetRegion.list <- function(region) {
  options(ternRegion = vapply(region, range, c(min = 0, max = 1)))
}

#' @export
SetRegion.matrix <- function(region) {
  options(ternRegion = apply(region, 2, range))
}

SetRegion.NULL <- function(region) {}
