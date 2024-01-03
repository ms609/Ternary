ternRegionDefault <- cbind(a = c(0, 100), b = c(0, 100), c = c(0, 100))
ternRegion20 <- cbind(a = c(20, 80), b = c(20, 80), c = c(20, 80))
ternRegionA <- cbind(a = c(40, 100), b = c(0, 60), c = c(0, 60))

.RegionCorners <- function(region = getOption("ternRegion", ternRegionDefault)) {
  cbind(a = region[c(2, 3, 5)],
        b = region[c(1, 4, 5)],
        c = region[c(1, 3, 6)])
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

.Rebase <- function(x, subRange, fullRange) {
  .Unnormalize(.Normalize(x, subRange), fullRange)
}

.NormalizeToRegion <- function(
    xy,
    region = getOption("ternRegion", ternRegionDefault)) {
  
  if (all(region == ternRegionDefault)) {
    xy
  } else {
    range <- .RegionXY(region)
    c(.Rebase(xy[1], range[, "x"], TernaryXRange()),
      .Rebase(xy[2], range[, "y"], TernaryYRange()))
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

.RegionInRange <- function(region) {
  min(region) >= 0 && max(region) <= 100
}

.RegionIsValid <- function(region) {
  .RegionIsEquilateral(region) && .RegionInRange(region)
}

.MakeRegion <- function(region) {
  ranges <- region[2, ] - region[1, ]
  maxRange <- max(ranges)
  if (maxRange > 100) {
    warning("Largest possible region is (0, 100)")
    return(options(ternRegion = ternRegionDefault))
  } else if (maxRange <= 0) {
    warning("Region must have positive size; ignoring")
    return(options(ternRegion = ternRegionDefault))
  }
  mid <- apply(region, 2, mean)
  ret <- rbind(min = mid, max = mid) + (c(-maxRange, maxRange) / 2)
  tooSmall <- ret[1, ] < 0
  ret[, tooSmall] <- ret[, tooSmall] - rep(ret[1, tooSmall], each = 2)
  tooBig <- ret[2, ] > 100
  ret[, tooBig] <- ret[, tooBig] - rep(ret[2, tooBig] - 100, each = 2)
  
  options(ternRegion = ret)
}

#' @export
SetRegion.list <- function(region) {
  SetRegion(do.call(rbind, region))
}

#' @export
SetRegion.matrix <- function(region) {
  .MakeRegion(apply(region, 2, range))
}
