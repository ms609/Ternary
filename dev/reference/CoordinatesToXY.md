# Convert user-specified ternary coordinates into X and Y coordinates

Accepts various formats of input data; extracts ternary coordinates and
converts to X and Y coordinates.

## Usage

``` r
HoldridgeToXY(pet, prec)

CoordinatesToXY(coordinates)
```

## Arguments

- pet, prec:

  Numeric vectors giving *p*otential *e*vapo*t*ranspiration ratio and
  annual *prec*ipitation (in mm).

- coordinates:

  A list, matrix, data.frame or vector in which each element (or row)
  specifies the three coordinates of a point in ternary space. Each
  element (or row) will be rescaled such that its entries sum to 100.

## Value

`CoordinatesToXY()` returns an array of two rows, corresponding to the X
and Y coordinates of `coordinates`.

## Functions

- `HoldridgeToXY()`: Convert from Holdridge coordinates

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)
