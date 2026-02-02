# Annotate points on a ternary plot

`Annotate()` identifies and label individual points on a ternary diagram
in the plot margins.

## Usage

``` r
Annotate(
  coordinates,
  labels,
  side,
  outset = 0.16,
  line.col = col,
  lty = par("lty"),
  lwd = par("lwd"),
  col = par("col"),
  font = par("font"),
  offset = 0.5,
  ...
)
```

## Arguments

- coordinates:

  A list, matrix, data.frame or vector in which each element (or row)
  specifies the three coordinates of a point in ternary space. Each
  element (or row) will be rescaled such that its entries sum to 100.

- labels:

  Character vector specifying text with which to annotate each entry in
  `coordinates`.

- side:

  Optional vector specifying which side of the ternary plot each point
  should be labelled on, using the notation `"a", "b", "c"` or
  `1, 2, 3`. Entries of `"n"` or `0` will not be annotated (but still
  require an entry in `labels`). Entries of `NA` will be allocated a
  side automatically, based on the midpoint of `coordinates`.

- outset:

  Numeric specifying distance from plot margins to labels.

- line.col, lty, lwd:

  parameters to
  [`segments()`](https://rdrr.io/r/graphics/segments.html).

- col, font, offset:

  parameters to [`text()`](https://rdrr.io/r/graphics/text.html).

- ...:

  Further parameters to [`text()`](https://rdrr.io/r/graphics/text.html)
  and [`segments()`](https://rdrr.io/r/graphics/segments.html).

## See also

[Annotation
vignette](https://ms609.github.io/Ternary/dev/articles/annotation.html)
gives further suggestions for manual annotation.

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
# Load some data
data("Seatbelts")
seats <- c("drivers", "front", "rear")
seat <- Seatbelts[month.abb %in% "Oct", seats]
law <- Seatbelts[month.abb %in% "Oct", "law"]

# Set up plot
oPar <- par(mar = c(2, 0, 0, 0))
TernaryPlot(alab = seats[1], blab = seats[2], clab = seats[3])
TernaryPoints(seat, cex = 0.8, col = 2 + law)

# Annotate points by year
Annotate(seat, labels = 1969:1984, col = 2 + law)


# Restore original graphical parameters
par(oPar)  
```
