# X and Y coordinates of ternary plotting area

X and Y coordinates of ternary plotting area

## Usage

``` r
TernaryXRange(direction = getOption("ternDirection", 1L))

TernaryYRange(direction = getOption("ternDirection", 1L))
```

## Arguments

- direction:

  (optional) Integer specifying the direction that the current ternary
  plot should point: 1, up; 2, right; 3, down; 4, left.

## Value

`TernaryXRange()` and `TernaryYRange()` return the minimum and maximum X
or Y coordinate of the area in which a ternary plot is drawn, oriented
in the specified direction. Because the plotting area is a square, the
triangle of the ternary plot will not occupy the full range in one
direction. Assumes that the defaults have not been overwritten by
specifying `xlim` or `ylim`.

## Functions

- `TernaryYRange()`: Returns the minimum and maximum Y coordinate for a
  ternary plot in the specified direction.

## See also

Other plot limits:
[`OutsidePlot()`](https://ms609.github.io/Ternary/reference/OutsidePlot.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)
