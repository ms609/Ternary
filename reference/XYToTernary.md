# Cartesian coordinates to ternary point

Convert Cartesian (*x*, *y*) coordinates to a point in ternary space.

## Usage

``` r
XYToTernary(
  x,
  y,
  direction = getOption("ternDirection", 1L),
  region = getOption("ternRegion", ternRegionDefault)
)

XYToHoldridge(x, y)

XYToPetPrec(x, y)
```

## Arguments

- x, y:

  Numeric values giving the *x* and *y* coordinates of a point or
  points.

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

`XYToTernary()` Returns the ternary point(s) corresponding to the
specified *x* and *y* coordinates, where a + b + c = 1.

## See also

Other coordinate translation functions:
[`ReflectedEquivalents()`](https://ms609.github.io/Ternary/reference/ReflectedEquivalents.md),
[`TernaryCoords()`](https://ms609.github.io/Ternary/reference/TernaryCoords.md),
[`TriangleCentres()`](https://ms609.github.io/Ternary/reference/TriangleCentres.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
XYToTernary(c(0.1, 0.2), 0.5)
#>        [,1]       [,2]
#> a 0.5773503 0.57735027
#> b 0.3113249 0.41132487
#> c 0.1113249 0.01132487
```
