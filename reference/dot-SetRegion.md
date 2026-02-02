# Set plotting region

Sets the region of the ternary plot being drawn. Usually called from
within
[`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md);
everyday users are unlikely to need to call this function directly.

## Usage

``` r
.SetRegion(region, prettify = NA_integer_, set = TRUE)
```

## Arguments

- region:

  (optional) Named list of length two specifying the the `min`imum and
  `max`imum values of each ternary axis to be drawn (e.g.
  `list(min = c(40, 0, 0), max = c(100, 60, 60)`); or a set of
  coordinates in a format accepted by
  [`TernaryPoints()`](https://ms609.github.io/Ternary/reference/AddToTernary.md).
  The plotted region will correspond to the smallest equilateral
  triangle that encompasses the specified ranges or coordinates.

- prettify:

  If numeric, the plotting region will be expanded to allow grid lines
  to be produced with `pretty(n = prettify)`. If `NA`, the smallest
  region encompassing `region` will be used.

- set:

  Logical specifying whether to set `options(ternRegion = region)`

## Value

`.SetRegion()` returns the value of `options(ternRegion = region)` if
`set == TRUE`, or the region, otherwise..

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
# XY Coordinates under original plotting region
TernaryToXY(c(1, 2, 3))
#> [1] -0.08333333  0.14433757
previous <- .SetRegion(rbind(min = c(20, 20, 20), max = c(60, 60, 60)))
#> Warning: Region must have positive size; ignoring

# New region options set
getOption("ternRegion")
#>       a   b   c
#> min   0   0   0
#> max 100 100 100
#> attr(,"class")
#> [1] "ternRegion"

# Coordinates under new plotting region
TernaryToXY(c(1, 2, 3))
#> [1] -0.08333333  0.14433757

# Restore previous setting
options(previous)
getOption("ternRegion")
#>       a   b   c
#> min   0   0   0
#> max 100 100 100
#> attr(,"class")
#> [1] "ternRegion"
```
