# Add elements to ternary or Holdridge plot

Plot shapes onto a ternary diagram created with
[`TernaryPlot()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md),
or a Holdridge plot created with
[`HoldridgePlot()`](https://ms609.github.io/Ternary/reference/HoldridgePlot.md).

## Usage

``` r
AddToTernary(PlottingFunction, coordinates, ...)

TernaryArrows(fromCoordinates, toCoordinates = fromCoordinates, ...)

TernaryLines(coordinates, ...)

TernaryPoints(coordinates, ...)

TernaryPolygon(coordinates, ...)

TernarySegments(fromCoordinates, toCoordinates = fromCoordinates, ...)

TernaryText(coordinates, ...)

JoinTheDots(coordinates, ...)

AddToHoldridge(PlottingFunction, pet, prec, ...)

HoldridgeArrows(fromCoordinates, toCoordinates = fromCoordinates, ...)

HoldridgeLines(pet, prec, ...)

HoldridgePoints(pet, prec, ...)

HoldridgePolygon(pet, prec, ...)

HoldridgeText(pet, prec, ...)
```

## Arguments

- PlottingFunction:

  Function to add data to a plot; perhaps one of
  [`points`](https://rdrr.io/r/graphics/points.html),
  [`lines`](https://rdrr.io/r/graphics/lines.html) or
  [`text`](https://rdrr.io/r/graphics/text.html).

- coordinates:

  A list, matrix, data.frame or vector in which each element (or row)
  specifies the three coordinates of a point in ternary space. Each
  element (or row) will be rescaled such that its entries sum to 100.

- ...:

  Additional parameters to pass to `PlottingFunction()`. If using
  `TernaryText()`, this will likely include the parameter `labels`, to
  specify the text to plot. Other useful [graphical
  parameters](https://rdrr.io/r/graphics/par.html) include `srt` to
  rotate text.

- fromCoordinates, toCoordinates:

  For `TernaryArrows()`, coordinates at which arrows should begin and
  end; *cf.* `x0`, `y0`, `x1` and `y1` in
  [arrows](https://rdrr.io/r/graphics/arrows.html). Recycled as
  necessary.

- pet, prec:

  Numeric vectors giving *p*otential *e*vapo*t*ranspiration ratio and
  annual *prec*ipitation (in mm).

## Functions

- `TernaryArrows()`: Add
  [arrows](https://rdrr.io/r/graphics/arrows.html)

- `TernaryLines()`: Add [lines](https://rdrr.io/r/graphics/lines.html)

- `TernaryPoints()`: Add
  [points](https://rdrr.io/r/graphics/points.html)

- `TernaryPolygon()`: Add
  [polygons](https://rdrr.io/r/graphics/polygon.html)

- `TernarySegments()`: Add
  [segments](https://rdrr.io/r/graphics/segments.html)

- `TernaryText()`: Add [text](https://rdrr.io/r/graphics/text.html)

- `JoinTheDots()`: Add points, joined by lines

- `HoldridgeArrows()`: Add
  [arrows](https://rdrr.io/r/graphics/arrows.html) to Holdridge plot

- `HoldridgeLines()`: Add [lines](https://rdrr.io/r/graphics/lines.html)
  to Holdridge plot

- `HoldridgePoints()`: Add
  [points](https://rdrr.io/r/graphics/points.html) to Holdridge plot

- `HoldridgePolygon()`: Add
  [polygons](https://rdrr.io/r/graphics/polygon.html) to Holdridge plot

- `HoldridgeText()`: Add [text](https://rdrr.io/r/graphics/text.html) to
  Holdridge plot

## See also

Other Holdridge plotting functions:
[`HoldridgeHypsometricCol()`](https://ms609.github.io/Ternary/reference/HoldridgeHypsometricCol.md),
[`HoldridgePlot()`](https://ms609.github.io/Ternary/reference/HoldridgePlot.md),
[`holdridge`](https://ms609.github.io/Ternary/reference/holdridge.md),
[`holdridgeClasses`](https://ms609.github.io/Ternary/reference/holdridgeClasses.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
# Data to plot
coords <- list(
  A = c(1, 0, 2),
  B = c(1, 1, 1),
  C = c(1.5, 1.5, 0),
  D = c(0.5, 1.5, 1)
)

# Set up plot
oPar <- par(mar = rep(0, 4), xpd = NA) # reduce margins and write in them
TernaryPlot()

# Add elements to ternary diagram
AddToTernary(lines, coords, col = "darkgreen", lty = "dotted", lwd = 3)
TernaryLines(coords, col = "darkgreen")
TernaryArrows(coords[1], coords[2:4], col = "orange", length = 0.2, lwd = 1)
TernaryText(coords, cex = 0.8, col = "red", font = 2)
seeThruBlue <- rgb(0, 0.2, 1, alpha = 0.8)
TernaryPoints(coords, pch = 1, cex = 2, col = seeThruBlue)
AddToTernary(graphics::points, coords, pch = 1, cex = 3)


# An equivalent syntax applies to Holdridge plots:
HoldridgePlot()
pet <- c(0.8, 2, 0.42)
prec <- c(250, 400, 1337)
HoldridgeText(pet, prec, c("A", "B", "C"))
AddToHoldridge(graphics::points, pet, prec, cex = 3)


# Restore original plotting parameters
par(oPar)
```
