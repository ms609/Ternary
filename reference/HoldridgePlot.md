# Plot life zones on a Holdridge plot

`HoldridgePlot()` creates a blank triangular plot, as proposed by
Holdridge (1947, 1967), onto which potential evapotranspiration (PET)
ratio and annual precipitation data can be plotted (using the
[`AddToHoldridge()`](https://ms609.github.io/Ternary/reference/AddToTernary.md)
family of functions) in order to interpret climatic life zones.

## Usage

``` r
HoldridgePlot(
  atip = NULL,
  btip = NULL,
  ctip = NULL,
  alab = "Potential evapotranspiration ratio",
  blab = "Annual precipitation / mm",
  clab = "Humidity province",
  lab.offset = 0.22,
  lab.col = c("#D81B60", "#1E88E5", "#111111"),
  xlim = NULL,
  ylim = NULL,
  region = NULL,
  lab.cex = 1,
  lab.font = 0,
  tip.cex = lab.cex,
  tip.font = 2,
  tip.col = "black",
  isometric = TRUE,
  atip.rotate = NULL,
  btip.rotate = NULL,
  ctip.rotate = NULL,
  atip.pos = NULL,
  btip.pos = NULL,
  ctip.pos = NULL,
  padding = 0.16,
  col = NA,
  panel.first = NULL,
  panel.last = NULL,
  grid.lines = 8,
  grid.col = c(NA, "#1E88E5", "#D81B60"),
  grid.lty = "solid",
  grid.lwd = par("lwd"),
  grid.minor.lines = 0,
  grid.minor.col = "lightgrey",
  grid.minor.lty = "solid",
  grid.minor.lwd = par("lwd"),
  hex.border = "#888888",
  hex.col = HoldridgeHypsometricCol,
  hex.lty = "solid",
  hex.lwd = par("lwd"),
  hex.cex = 0.5,
  hex.labels = NULL,
  hex.font = NULL,
  hex.text.col = "black",
  axis.cex = 0.8,
  axis.col = c(grid.col[2], grid.col[3], NA),
  axis.font = par("font"),
  axis.labels = TRUE,
  axis.lty = "solid",
  axis.lwd = 1,
  axis.rotate = TRUE,
  axis.pos = NULL,
  axis.tick = TRUE,
  ticks.lwd = axis.lwd,
  ticks.length = 0.025,
  ticks.col = grid.col,
  ...
)

HoldridgeBelts(
  grid.col = "#004D40",
  grid.lty = "dotted",
  grid.lwd = par("lwd")
)

HoldridgeHexagons(
  border = "#004D40",
  hex.col = HoldridgeHypsometricCol,
  lty = "dotted",
  lwd = par("lwd"),
  labels = NULL,
  cex = 1,
  text.col = NULL,
  font = NULL
)
```

## Arguments

- atip, btip, ctip:

  Character string specifying text to title corners, proceeding
  clockwise from the corner specified in `point` (default: top).

- alab, blab, clab:

  Character string specifying text with which to label the corresponding
  sides of the triangle. Left or right-pointing arrows are produced by
  typing `\\U2190` or `\\U2192`, or using `expression('value' %->% '')`.

- lab.offset:

  Numeric specifying distance between midpoint of axis label and the
  axis. The default value is given in the 'Usage' section; a value of
  `0` will position the axis label directly on the axis. Increase
  `padding` if labels are being clipped. Use a vector of length three to
  specify a different offset for each label.

- lab.col:

  Character vector specifying colours for axis labels. Use a vector of
  length three to specify a different colour for each label.

- xlim, ylim:

  Numeric vectors of length two specifying the minimum and maximum *x*
  and *y* limits of the plotted area, to which `padding` will be added.
  The default is to display the complete height or width of the plot.
  Allows cropping to magnified region of the plot. (See vignette for
  diagram.) May be overridden if `isometric = TRUE`; see documentation
  of `isometric` parameter.

- region:

  (optional) Named list of length two specifying the the `min`imum and
  `max`imum values of each ternary axis to be drawn (e.g.
  `list(min = c(40, 0, 0), max = c(100, 60, 60)`); or a set of
  coordinates in a format accepted by
  [`TernaryPoints()`](https://ms609.github.io/Ternary/reference/AddToTernary.md).
  The plotted region will correspond to the smallest equilateral
  triangle that encompasses the specified ranges or coordinates.

- lab.cex, tip.cex:

  Numeric specifying character expansion (font size) for axis labels.
  Use a vector of length three to specify a different value for each
  direction.

- lab.font, tip.font:

  Numeric specifying font style (Roman, bold, italic, bold-italic) for
  axis titles. Use a vector of length three to set a different font for
  each direction.

- isometric:

  Logical specifying whether to enforce an equilateral shape for the
  ternary plot. If only one of `xlim` and `ylim` is set, the other will
  be calculated to maintain an equilateral plot. If both `xlim` and
  `ylim` are set, but have different ranges, then the limit with the
  smaller range will be scaled until its range matches that of the other
  limit.

- atip.rotate, btip.rotate, ctip.rotate:

  Integer specifying number of degrees to rotate label of rightmost
  apex.

- atip.pos, btip.pos, ctip.pos:

  Integer specifying positioning of labels, iff the corresponding
  `xtip.rotate` parameter is set.

- padding:

  Numeric specifying size of internal margin of the plot; increase if
  axis labels are being clipped.

- col:

  The colour for filling the plot; see
  [`polygon`](https://rdrr.io/r/graphics/polygon.html).

- panel.first:

  An expression to be evaluated after the plot axes are set up but
  before any plotting takes place. This can be useful for drawing
  backgrounds, e.g. with
  [`ColourTernary()`](https://ms609.github.io/Ternary/reference/ColourTernary.md)
  or
  [`HorizontalGrid()`](https://ms609.github.io/Ternary/reference/TernaryPlot.md).
  Note that this works by lazy evaluation: passing this argument from
  other plot methods may well not work since it may be evaluated too
  early.

- panel.last:

  An expression to be evaluated after plotting has taken place but
  before the axes and box are added. See the comments about
  `panel.first`.

- grid.lines:

  Integer specifying the number of grid lines to plot. If
  `axis.labels = TRUE`, this will be used as a hint to
  [`pretty()`](https://rdrr.io/r/base/pretty.html).

- grid.col, grid.minor.col:

  Colours to draw the grid lines. Use a vector of length three to set
  different values for each direction.

- grid.lty, grid.minor.lty:

  Character or integer vector; line type of the grid lines. Use a vector
  of length three to set different values for each direction.

- grid.lwd, grid.minor.lwd:

  Non-negative numeric giving line width of the grid lines. Use a vector
  of length three to set different values for each direction.

- grid.minor.lines:

  Integer specifying the number of minor (unlabelled) grid lines to plot
  between each major pair.

- hex.border, hex.lty, hex.lwd:

  Parameters to pass to `HoldridgeHexagons()`. Set to `NA` to suppress
  hexagons.

- hex.col:

  Fill colour for hexagons. Provide a vector specifying a colour for
  each hexagon in turn, reading from left to right and top to bottom, or
  a function that accepts two arguments, numerics `pet` and `prec`, and
  returns a colour in a format accepted by
  [`polygon()`](https://rdrr.io/r/graphics/polygon.html).

- hex.cex, hex.font, hex.text.col:

  Parameters passed to [`text()`](https://rdrr.io/r/graphics/text.html)
  to plot `hex.labels`.

- hex.labels:

  38-element character vector specifying label for each hexagonal class,
  from top left to bottom right.

- axis.cex:

  Numeric specifying character expansion (font size) for axis labels.
  Use a vector of length three to set a different value for each
  direction.

- axis.col, ticks.col, tip.col:

  Colours for the axis line, tick marks and tip labels respectively. Use
  a vector of length three to set a different value for each direction.
  `axis.col = NULL` means to use `par('fg')`, possibly specified inline,
  and `ticks.col = NULL` means to use whatever colour `axis.col`
  resolved to.

- axis.font:

  Font for text. Defaults to `par('font')`.

- axis.labels:

  This can either be a logical value specifying whether (numerical)
  annotations are to be made at the tickmarks, or a character or
  expression vector of labels to be placed at the tick points, or a list
  of length three, with each entry specifying labels to be placed on
  each axis in turn.

- axis.lty:

  Line type for both the axis line and tick marks. Use a vector of
  length three to set a different value for each direction.

- axis.lwd, ticks.lwd:

  Line width for the axis line and tick marks. Zero or negative values
  will suppress the line or ticks. Use a vector of length three to set
  different values for each axis.

- axis.rotate:

  Logical specifying whether to rotate axis labels to parallel grid
  lines, or numeric specifying custom rotation for each axis, to be
  passed as `srt` parameter to
  [`text()`](https://rdrr.io/r/graphics/text.html). Expand margins or
  set `par(xpd = NA)` if labels are clipped.

- axis.pos:

  Vector of length one or three specifying position of axis labels, to
  be passed as `pos` parameter to
  [`text()`](https://rdrr.io/r/graphics/text.html); populated
  automatically if `NULL` (the default).

- axis.tick:

  Logical specifying whether to mark the axes with tick marks.

- ticks.length:

  Numeric specifying distance that ticks should extend beyond the plot
  margin. Also affects position of axis labels, which are plotted at the
  end of each tick. Use a vector of length three to set a different
  length for each direction.

- ...:

  Additional parameters to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html).

- border:

  Colour to use for hexagon borders.

- lty, lwd, cex, font:

  [Graphical parameters](https://rdrr.io/r/graphics/par.html) specifying
  properties of hexagons to be plotted.

- labels:

  Vector specifying labels for life zone hexagons to be plotted.
  Suggested values:
  [`holdridgeClassesUp`](https://ms609.github.io/Ternary/reference/holdridgeClasses.md),
  [`holdridgeLifeZonesUp`](https://ms609.github.io/Ternary/reference/holdridgeClasses.md).

- text.col:

  Colour of text to be printed in hexagons.

## Details

[`HoldridgePoints()`](https://ms609.github.io/Ternary/reference/AddToTernary.md),
[`HoldridgeText()`](https://ms609.github.io/Ternary/reference/AddToTernary.md)
and related functions allow data points to be added to an existing plot;
[`AddToHoldridge()`](https://ms609.github.io/Ternary/reference/AddToTernary.md)
allows plotting using any of the standard plotting functions.

`HoldridgeBelts()` and `HoldridgeHexagons()` plot interpretative lines
and hexagons allowing plotted data to be linked to interpreted climate
settings.

Please cite Tsakalos *et al.* (2023) when using this function.

## References

Holdridge (1947), "Determination of world plant formations from simple
climatic data", *Science* 105:367–368.
[doi:10.1126/science.105.2727.367](https://doi.org/10.1126/science.105.2727.367)

Holdridge (1967), *Life zone ecology*. Tropical Science Center, San José

Tsakalos, Smith, Luebert & Mucina (2023). "climenv: Download, extract
and visualise climatic and elevation data.", *Journal of Vegetation
Science* 6:e13215.
[doi:10.1111/jvs.13215](https://doi.org/10.1111/jvs.13215)

## See also

Other Holdridge plotting functions:
[`AddToTernary()`](https://ms609.github.io/Ternary/reference/AddToTernary.md),
[`HoldridgeHypsometricCol()`](https://ms609.github.io/Ternary/reference/HoldridgeHypsometricCol.md),
[`holdridge`](https://ms609.github.io/Ternary/reference/holdridge.md),
[`holdridgeClasses`](https://ms609.github.io/Ternary/reference/holdridgeClasses.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
data(holdridgeLifeZonesUp, package = "Ternary")
HoldridgePlot(hex.labels = holdridgeLifeZonesUp)
HoldridgeBelts()
```
