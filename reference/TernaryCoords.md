# Convert ternary coordinates to Cartesian space

Convert coordinates of a point in ternary space, in the format (*a*,
*b*, *c*), to *x* and *y* coordinates of Cartesian space, which can be
sent to standard functions in the 'graphics' package.

## Usage

``` r
TernaryCoords(
  abc,
  b_coord = NULL,
  c_coord = NULL,
  direction = getOption("ternDirection", 1L),
  region = getOption("ternRegion", ternRegionDefault)
)

# S3 method for class 'matrix'
TernaryToXY(
  abc,
  b_coord = NULL,
  c_coord = NULL,
  direction = getOption("ternDirection", 1L),
  region = getOption("ternRegion", ternRegionDefault)
)

# S3 method for class 'numeric'
TernaryToXY(
  abc,
  b_coord = NULL,
  c_coord = NULL,
  direction = getOption("ternDirection", 1L),
  region = getOption("ternRegion", ternRegionDefault)
)

TernaryToXY(
  abc,
  b_coord = NULL,
  c_coord = NULL,
  direction = getOption("ternDirection", 1L),
  region = getOption("ternRegion", ternRegionDefault)
)
```

## Arguments

- abc:

  A vector of length three giving the position on a ternary plot that
  points in the direction specified by `direction` (1 = up, 2 = right, 3
  = down, 4 = left). `c(100, 0, 0)` will plot in the `direction`-most
  corner; `c(0, 100, 0)` will plot in the corner clockwise of
  `direction`; `c(0, 0, 100)` will plot in the corner anti-clockwise of
  `direction`. Alternatively, the a coordinate can be specified as the
  first parameter, in which case the b and c coordinates must be
  specified via `b_coord` and `c_coord`. Or, a matrix with three rows,
  representing in turn the `a`, `b` and `c` coordinates of points.

- b_coord:

  The b coordinate, if `abc` is a single number.

- c_coord:

  The c coordinate, if `abc` is a single number.

- direction:

  (optional) Integer specifying the direction that the current ternary
  plot should point: 1, up; 2, right; 3, down; 4, left.

- region:

  (optional) Named list of length two specifying the the `min`imum and
  `max`imum values of each ternary axis to be drawn (e.g.
  `list(min = c(40, 0, 0), max = c(100, 60, 60)`); or a set of
  coordinates in a format accepted by
  [`TernaryPoints()`](https://ms609.github.io/Ternary/reference/AddToTernary.md).
  The plotted region will correspond to the smallest equilateral
  triangle that encompasses the specified ranges or coordinates.

## Value

`TernaryCoords()` returns a vector of length two that converts the
coordinates given in `abc` into Cartesian (*x*, *y*) coordinates
corresponding to the plot created by the last call of
[`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md).

## See also

- [`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md)

Other coordinate translation functions:
[`ReflectedEquivalents()`](https://ms609.github.io/Ternary/reference/ReflectedEquivalents.md),
[`TriangleCentres()`](https://ms609.github.io/Ternary/reference/TriangleCentres.md),
[`XYToTernary()`](https://ms609.github.io/Ternary/reference/XYToTernary.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
TernaryCoords(100, 0, 0)
#> [1] 0.0000000 0.8660254
TernaryCoords(c(0, 100, 0))
#> [1] 0.5 0.0

coords <- matrix(1:12, nrow = 3)
TernaryToXY(coords)
#>          [,1]        [,2]        [,3]        [,4]
#> x -0.08333333 -0.03333333 -0.02083333 -0.01515152
#> y  0.14433757  0.23094011  0.25259074  0.26243194
```
