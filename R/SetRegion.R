ternRegionDefault <- cbind(
  a = c(min = 0, max = 100),
  b = c(0, 100),
  c = c(0, 100)
)
ternRegion20 <- cbind(a = c(20, 60), b = c(20, 60), c = c(20, 60))
ternRegionA <- cbind(a = c(40, 100), b = c(0, 60), c = c(0, 60))

#' Set plotting region
#' 
#' Sets the region of the ternary plot being drawn.
#' Usually called from within `TernaryPlot()`; everyday users are unlikely to
#' need to call this function directly.
#' 
#' @inheritParams TernaryPlot
#' @param prettify If numeric, the plotting region will be expanded to allow
#'  grid lines to be produced with `pretty(n = prettify)`. If `NA`, the
#'  smallest region encompassing `region` will be used.
#' 
#' @return `.SetRegion()` returns the value of `options(ternRegion = region)`.
#' @template MRS
#' @examples
#' # XY Coordinates under original plotting region
#' TernaryToXY(c(1, 2, 3))
#' previous <- .SetRegion(ternRegion20)
#' 
#' # New region options set
#' getOption("ternRegion")
#' 
#' # Coordinates under new plotting region
#' TernaryToXY(c(1, 2, 3))
#' 
#' # Restore previous setting
#' options(previous)
#' getOption("ternRegion")
#' @keywords internal
#' @export
.SetRegion <- function(region, prettify = NA_integer_) UseMethod(".SetRegion")

#' @export
.SetRegion.list <- function(region, prettify = NA_integer_) {
  .SetRegion(do.call(rbind, region), prettify = prettify)
}

#' @export
.SetRegion.matrix <- function(region, prettify = NA_integer_) {
  .MakeRegion(apply(region, 2, range), prettify = prettify)
}

.RegionCorners <- function(
    region = getOption("ternRegion", ternRegionDefault)
  ) {
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
    fullRange <- .RegionXY(ternRegionDefault)
    c(.Rebase(xy[1], range[, "x"], fullRange[, "x"]),
      .Rebase(xy[2], range[, "y"], fullRange[, "y"]))
  }
}

.RegionIsEquilateral <- function(region) {
  length(unique(apply(region, 2, max) - apply(region, 2, min))) == 1
}

.RegionInRange <- function(region) {
  min(region) >= 0 && max(region) <= 100
}

.RegionCorners100 <- function(region) {
  sum(region[c(1, 3, 5) + c(1, 0, 0)]) == 100 &&
  sum(region[c(1, 3, 5) + c(0, 1, 0)]) == 100 &&
  sum(region[c(1, 3, 5) + c(0, 0, 1)]) == 100
}

.RegionIsValid <- function(region) {
  .RegionIsEquilateral(region) && 
    .RegionInRange(region) &&
    .RegionCorners100(region)
}

.MakeRegion <- function(ranges, prettify = NA_integer_) {
  spans <- ranges[2, ] - ranges[1, ]
  maxSpan <- max(spans)
  if (maxSpan > 100) {
    warning("Largest possible region is (0, 100)")
    return(options(ternRegion = ternRegionDefault))
  } else if (maxSpan <= 0) {
    warning("Region must have positive size; ignoring")
    return(options(ternRegion = ternRegionDefault))
  }
  
  .Max <- function(i) {
    100 - sum(ranges[1, -i])
  }
  region <- rbind(min = ranges[1, ], max = vapply(1:3, .Max, double(1)))
  if (!is.na(prettify)) {
    prettyRegion <- apply(region, 2, pretty, prettify)
    prettyRegion <- prettyRegion[c(1, nrow(prettyRegion)), ]
    if (.RegionIsValid(prettyRegion)) {
      region <- prettyRegion
    }
  }
  
  options(ternRegion = region)
}

