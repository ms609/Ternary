#' Palettes compatible with colour blindness
#'
#' Colour palettes recommended for use with colour blind audiences.
#'
#' Since R 4.0, `cbPalette8` is available in base R as `palette.colors(8)`.
#'
#' `cbPalette15` is a [Brewer palette](https://mk.bcgsc.ca/brewer/).
#' Because colours 4 and 7 are difficult to distinguish from colours 13 and 3,
#' respectively, in individuals with tritanopia, `cbPalette13` omits these
#' colours (i.e. `cbPalette13 <- cbPalette15[-c(4, 7)]`).
#'
#' @format Character vectors of lengths 8, 13 and 15.
#'
#' @examples
#' data("cbPalette8")
#' plot.new()
#' plot.window(xlim = c(1, 16), ylim = c(0, 3))
#' text(1:8 * 2, 3, 1:8, col = cbPalette8)
#' points(1:8 * 2, rep(2, 8), col = cbPalette8, pch = 15)
#'
#' data("cbPalette15")
#' text(1:15, 1, col = cbPalette15)
#' text(c(4, 7), 1, "[   ]")
#' points(1:15, rep(0, 15), col = cbPalette15, pch = 15)
#' @source
#' - `cbPalette8`: \cite{Wong B. 2011. Color blindness. Nat. Methods. 8:441.
#' \doi{10.1038/nmeth.1618}}
#'
#' - `cbPalette15`: \url{
#' https://mk.bcgsc.ca/biovis2012/color-blindness-palette.png}
#' 
#' @keywords datasets
#' @name cbPalettes
NULL

#' @rdname cbPalettes
"cbPalette8"
#' @rdname cbPalettes
"cbPalette13"
#' @rdname cbPalettes
"cbPalette15"

#' Random sample of points for Holdridge plotting
#'
#' A stratified random sampling (average of 100 points) using
#' a global mapping of Holdridge’s scheme.
#'
#' @examples
#' data("holdridge", package = "Ternary")
#' head(holdridge)
#' @author James Lee Tsakalos
#' @family Holdridge plotting functions
#' @keywords datasets
"holdridge"

#' Names of the 38 classes defined with the Holdridge system
#'
#' `holdridgeClasses` is a character vector naming, from left to right,
#' top to bottom, the 38 classes defined by the International Institute for
#'  Applied Systems Analysis (IIASA).
#'
#' `holdridgeLifeZones` is a character vector naming, from left to right,
#' top to bottom, the 38 cells of the Holdridge classification plot.
#'
#' `holdridgeClassesUp` and `holdridgeLifeZonesUp` replace spaces with new
#' lines, for more legible plotting with [`HoldridgeHexagons()`].
#'
#' @source
#' Holdridge (1947),
#' "Determination of world plant formations from simple climatic data",
#' _Science_ 105:367&ndash;368. \doi{10.1126/science.105.2727.367}
#'
#' Holdridge (1967), _Life zone ecology_. Tropical Science Center, 
#' San Jos&eacute;.
#' 
#' Leemans, R. (1990),
#' "Possible change in natural vegetation patterns due to a global warming",
#' _International Institute for Applied Systems Analysis_ Working paper
#' WP-90-08.
#' \url{https://pure.iiasa.ac.at/id/eprint/3443/1/WP-90-008.pdf}
#' @encoding UTF-8
#' @family Holdridge plotting functions
#' @template MRS
"holdridgeClasses"


#' @rdname holdridgeClasses
"holdridgeLifeZones"
#' @rdname holdridgeClasses
"holdridgeLifeZonesUp"
#' @rdname holdridgeClasses
"holdridgeClassesUp"
