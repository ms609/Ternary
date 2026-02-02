# Does triangle overlap convex hull of points?

Does triangle overlap convex hull of points?

## Usage

``` r
TriangleInHull(triangles, coordinates, buffer)
```

## Arguments

- triangles:

  Three-row matrix as produced by
  [`TriangleCentres()`](https://ms609.github.io/Ternary/reference/TriangleCentres.md).

- coordinates:

  A matrix with two or three rows specifying the coordinates of points
  in *x, y* or *a, b, c* format.

- buffer:

  Include triangles whose centres lie within `buffer` triangles widths
  (i.e. edge lengths) of the convex hull.

## Value

`TriangleInHull()` returns a list with the elements:

- `$inside`: vector specifying whether each of a set of triangles
  produced by
  [`TriangleCentres()`](https://ms609.github.io/Ternary/reference/TriangleCentres.md)
  overlaps the convex hull of points specified by `coordinates`.

- `$hull`: Coordinates of convex hull of `coordinates`, after expansion
  to cover overlapping triangles.

## See also

Other tiling functions:
[`TriangleCentres()`](https://ms609.github.io/Ternary/reference/TriangleCentres.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
set.seed(0)
nPts <- 50
a <- runif(nPts, 0.3, 0.7)
b <- 0.15 + runif(nPts, 0, 0.7 - a)
c <- 1 - a - b
coordinates <- rbind(a, b, c)

TernaryPlot(grid.lines = 5)
TernaryPoints(coordinates, pch = 3, col = 4)
triangles <- TriangleCentres(resolution = 5)
inHull <- TriangleInHull(triangles, coordinates)
polygon(inHull$hull, border = 4)
values <- rbind(triangles,
                z = ifelse(inHull$inside, "#33cc3333", "#cc333333"))
points(triangles["x", ], triangles["y", ],
       pch = ifelse(triangles["triDown", ], 6, 2),
       col = ifelse(inHull$inside, "#33cc33", "#cc3333"))
ColourTernary(values)
```
