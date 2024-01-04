ternRegionDefault <- structure(cbind(
  a = c(min = 0, max = 100),
  b = c(0, 100),
  c = c(0, 100)
), class = "ternRegion")
ternRegion20 <- structure(
  cbind(a = c(min = 20, max = 60), b = c(20, 60), c = c(20, 60)),
  class = "ternRegion")
ternRegionA <- structure(
  cbind(a = c(40, 100), b = c(0, 60), c = c(0, 60)),
  class = "ternRegion")

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
#' @param set Logical specifying whether to set `options(ternRegion = region)`
#' 
#' @return `.SetRegion()` returns the value of `options(ternRegion = region)` 
#' if `set == TRUE`, or the region, otherwise..
#' @template MRS
#' @examples
#' # XY Coordinates under original plotting region
#' TernaryToXY(c(1, 2, 3))
#' previous <- .SetRegion(rbind(min = c(20, 20, 20), max = c(60, 60, 60)))
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
.SetRegion <- function(region, prettify = NA_integer_, set = TRUE) {
  UseMethod(".SetRegion")
}

#' @export
.SetRegion.list <- function(region, prettify = NA_integer_, set = TRUE) {
  names <- tolower(names(region))
  region <- do.call(rbind, region)
  if (!is.null(names) && length(names) == 2 && 
      all(c("min", "max") %in% names)) {
    .MakeRegion(
      apply(region, 2, range),
      prettify = prettify,
      set = set
    )
  } else {
    .SetRegion(region, prettify = prettify, set = set)
  }
}

#' @export
.SetRegion.data.frame <- .SetRegion.list

#' @export
.SetRegion.matrix <- function(region, prettify = NA_integer_, set = TRUE) {
  .MakeRegion(apply(region * 100 / rowSums(region), 2, range),
              prettify = prettify, set = set)
}

#' @export
.SetRegion.ternRegion <- function(region, prettify = NA_integer_, set = TRUE) {
  if (set) {
    options(ternRegion = region)
  } else {
    region
  }
}

.RegionCorners <- function(
    region = getOption("ternRegion", ternRegionDefault)
  ) {
  # Check region is valid - may be send directly from TernaryToXY
  if (!inherits(region, "ternRegion")) {
    region <- .SetRegion(region, set = FALSE)
  }
  cbind(a = region[c(2, 3, 5)],
        b = region[c(1, 4, 5)],
        c = region[c(1, 3, 6)])
}

.RegionXY <- function(region = getOption("ternRegion", ternRegionDefault)) {
  apply(
    TernaryToXY(.RegionCorners(region), region = ternRegionDefault),
    1,
    range
  )
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
  
  if (!inherits(region, "ternRegion")) {
    region <- .SetRegion(region, set = FALSE)
  }
  
  if (all(region == ternRegionDefault)) {
    xy
  } else {
    range <- .RegionXY(region)
    fullRange <- .RegionXY(ternRegionDefault)
    c(.Rebase(xy[1], range[, "x"], fullRange[, "x"]),
      .Rebase(xy[2], range[, "y"], fullRange[, "y"]))
  }
}

.UnnormalizeXY <- function(
    x, y,
    region = getOption("ternRegion", ternRegionDefault)) {

  if (!inherits(region, "ternRegion")) {
    region <- .SetRegion(region, set = FALSE)
  }
  
  if (all(region == ternRegionDefault)) {
    list(x, y)
  } else {
    fullRange <- .RegionXY(ternRegionDefault)
    range <- .RegionXY(region)
    list(.Rebase(x, fullRange[, "x"], range[, "x"]),
         .Rebase(y, fullRange[, "y"], range[, "y"]))
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

.MakeRegion <- function(ranges, prettify = NA_integer_, set = TRUE) {
  spans <- ranges[2, ] - ranges[1, ]
  maxSpan <- max(spans)
  if (maxSpan > 100) {
    warning("Largest possible region is (0, 100)")
    return(if (set) {
      options(ternRegion = ternRegionDefault)
    } else {
      ternRegionDefault
    })
  } else if (maxSpan <= 0) {
    warning("Region must have positive size; ignoring")
    return(if (set) {
      options(ternRegion = ternRegionDefault)
      } else {
        ternRegionDefault
      })
  }
  
  .Max <- function(i) {
    100 - sum(ranges[1, -i])
  }
  region <- rbind(min = ranges[1, ], max = vapply(1:3, .Max, double(1)))
  if (!is.na(prettify)) {
    prettyRegion <- .SimpleApply(region, 2, pretty, prettify)
    l <- lengths(prettyRegion)
    longest <- max(l)
    prettyRegion[l != longest] <- 
      lapply(prettyRegion[l != longest], function(i) {
        n <- length(i)
        by <- i[2] - i[1]
        i <- c(i, seq(max(i) + by, by = by, length.out = longest - n))
        i
      })
    prettyRegion <- rbind(min = vapply(prettyRegion, `[`, 0, 1),
                          max = vapply(prettyRegion, `[`, 0, longest))
    if (.RegionIsValid(prettyRegion)) {
      region <- prettyRegion
    }
  }
  
  class(region) <- "ternRegion"
  if (set) {
    options(ternRegion = region)
  } else {
    region
  }
  
}

