# Reflected equivalents of points outside the ternary plot

To avoid edge effects, it may be desirable to add the value of a point
within a ternary plot with the value of its 'reflection' across the
nearest axis or corner.

## Usage

``` r
ReflectedEquivalents(x, y, direction = getOption("ternDirection", 1L))
```

## Arguments

- x, y:

  Vectors of *x* and *y* coordinates of points.

- direction:

  (optional) Integer specifying the direction that the current ternary
  plot should point: 1, up; 2, right; 3, down; 4, left.

## Value

`ReflectedEquivalents()` returns a list of the *x*, *y* coordinates of
the points produced if the given point is reflected across each of the
edges or corners.

## See also

Other coordinate translation functions:
[`TernaryCoords()`](https://ms609.github.io/Ternary/reference/TernaryCoords.md),
[`TriangleCentres()`](https://ms609.github.io/Ternary/reference/TriangleCentres.md),
[`XYToTernary()`](https://ms609.github.io/Ternary/reference/XYToTernary.md)

## Examples

``` r
TernaryPlot(axis.labels = FALSE, point = 4)

xy <- cbind(
  TernaryCoords(0.9, 0.08, 0.02),
  TernaryCoords(0.15, 0.8, 0.05),
  TernaryCoords(0.05, 0.1, 0.85)
)
x <- xy[1, ]
y <- xy[2, ]

points(x, y, col = "red", pch = 1:3)
ref <- ReflectedEquivalents(x, y)
points(ref[[1]][, 1], ref[[1]][, 2], col = "blue", pch = 1)
points(ref[[2]][, 1], ref[[2]][, 2], col = "green", pch = 2)
points(ref[[3]][, 1], ref[[3]][, 2], col = "orange", pch = 3)
```
