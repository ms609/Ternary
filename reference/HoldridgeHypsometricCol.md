# Convert a point in evapotranspiration-precipitation space to an appropriate cross-blended hypsometric colour

Used to colour
[`HoldridgeHexagons()`](https://ms609.github.io/Ternary/reference/HoldridgePlot.md),
and may also be used to aid the interpretation of PET + precipitation
data in any graphical context.

## Usage

``` r
HoldridgeHypsometricCol(pet, prec, opacity = NA)
```

## Arguments

- pet, prec:

  Numeric vectors giving *p*otential *e*vapo*t*ranspiration ratio and
  annual *prec*ipitation (in mm).

- opacity:

  Opacity level to be converted to the final two characters of an RGBA
  hexadecimal colour definition, e.g. `#000000FF`. Specify a character
  string, which will be interpreted as a hexadecimal alpha value and
  appended to the six RGB hexadecimal digits; a numeric in the range 0
  (transparent) to 1 (opaque); or `NA`, to return only the six RGB
  digits.

## Value

Character vector listing RGB or (if `opacity != NA`) RGBA values
corresponding to each PET-precipitation value pair.

## References

Palette derived from the hypsometric colour scheme presented at [Shaded
Relief](https://www.shadedrelief.com/hypso/hypso.html).

## See also

Other Holdridge plotting functions:
[`AddToTernary()`](https://ms609.github.io/Ternary/reference/AddToTernary.md),
[`HoldridgePlot()`](https://ms609.github.io/Ternary/reference/HoldridgePlot.md),
[`holdridge`](https://ms609.github.io/Ternary/reference/holdridge.md),
[`holdridgeClasses`](https://ms609.github.io/Ternary/reference/holdridgeClasses.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
HoldridgePlot(hex.col = HoldridgeHypsometricCol)

VeryTransparent <- function(...) HoldridgeHypsometricCol(..., opacity = 0.3)
HoldridgePlot(hex.col = VeryTransparent)
pet <- holdridge$PET
prec <- holdridge$Precipitation
ptCol <- HoldridgeHypsometricCol(pet, prec)
HoldridgePoints(pet, prec, pch = 21, bg = ptCol)
```
