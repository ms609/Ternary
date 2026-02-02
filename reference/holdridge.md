# Random sample of points for Holdridge plotting

A stratified random sampling (average of 100 points) using a global
mapping of Holdridge’s scheme.

## Usage

``` r
holdridge
```

## Format

An object of class `data.frame` with 39 rows and 4 columns.

## See also

Other Holdridge plotting functions:
[`AddToTernary()`](https://ms609.github.io/Ternary/reference/AddToTernary.md),
[`HoldridgeHypsometricCol()`](https://ms609.github.io/Ternary/reference/HoldridgeHypsometricCol.md),
[`HoldridgePlot()`](https://ms609.github.io/Ternary/reference/HoldridgePlot.md),
[`holdridgeClasses`](https://ms609.github.io/Ternary/reference/holdridgeClasses.md)

## Author

James Lee Tsakalos

## Examples

``` r
data("holdridge", package = "Ternary")
head(holdridge)
#>         Holdridge_Zones        PET Precipitation Latitude
#> 1         Boreal desert 0.33986757      282.3077 42.63539
#> 2       Boreal dry bush 0.41447706      281.2931 54.61551
#> 3   Boreal moist forest 0.24459577      472.1111 57.34100
#> 4    Boreal rain forest 0.07964139     1502.1304 37.71037
#> 5     Boreal wet forest 0.14545061      790.8526 49.58462
#> 6 Cool temperate desert 1.94162110      133.5135 20.62251
```
