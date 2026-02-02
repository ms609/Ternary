# Names of the 38 classes defined with the Holdridge system

`holdridgeClasses` is a character vector naming, from left to right, top
to bottom, the 38 classes defined by the International Institute for
Applied Systems Analysis (IIASA).

## Usage

``` r
holdridgeClasses

holdridgeLifeZones

holdridgeLifeZonesUp

holdridgeClassesUp
```

## Format

An object of class `character` of length 38.

An object of class `character` of length 33.

An object of class `character` of length 33.

An object of class `character` of length 38.

## Source

Holdridge (1947), "Determination of world plant formations from simple
climatic data", *Science* 105:367–368.
[doi:10.1126/science.105.2727.367](https://doi.org/10.1126/science.105.2727.367)

Holdridge (1967), *Life zone ecology*. Tropical Science Center, San
José.

Leemans, R. (1990), "Possible change in natural vegetation patterns due
to a global warming", *International Institute for Applied Systems
Analysis* Working paper WP-90-08.
<https://pure.iiasa.ac.at/id/eprint/3443/1/WP-90-008.pdf>

## Details

`holdridgeLifeZones` is a character vector naming, from left to right,
top to bottom, the 38 cells of the Holdridge classification plot.

`holdridgeClassesUp` and `holdridgeLifeZonesUp` replace spaces with new
lines, for more legible plotting with
[`HoldridgeHexagons()`](https://ms609.github.io/Ternary/reference/HoldridgePlot.md).

## See also

Other Holdridge plotting functions:
[`AddToTernary()`](https://ms609.github.io/Ternary/reference/AddToTernary.md),
[`HoldridgeHypsometricCol()`](https://ms609.github.io/Ternary/reference/HoldridgeHypsometricCol.md),
[`HoldridgePlot()`](https://ms609.github.io/Ternary/reference/HoldridgePlot.md),
[`holdridge`](https://ms609.github.io/Ternary/reference/holdridge.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)
