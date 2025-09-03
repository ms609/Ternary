#' Polygon geometry
#' 
#' These functions have moved to "[PlotTools](
#' https://ms609.github.io/PlotTools/)" and are deprecated here.
#' @param \dots Parameters to PlotTools function
#' @name Polygon-Geometry
#' @importFrom PlotTools PolygonArea PolygonCenter GrowPolygon
#' @keywords internal
#' @export
PolygonArea <- function(...) {
  .Deprecated("PlotTools::PolygonArea", "PlotTools")
  PlotTools::PolygonArea(...)
}

#' @rdname Polygon-Geometry
#' @export
PolygonCenter <- function(...) {
  .Deprecated("PlotTools::PolygonCenter", "PlotTools")
  PlotTools::PolygonCenter(...)
}

#' @rdname Polygon-Geometry
#' @export
PolygonCentre <- function(...) {
  .Deprecated("PlotTools::PolygonCentre", "PlotTools")
  PlotTools::PolygonCenter(...)
}

#' @rdname Polygon-Geometry
#' @export
GrowPolygon <- function(...) {
  # TODO when we stop exporting, replace explicit link to PlotTools::GrowPolygon
  # in other functions.
  .Deprecated("PlotTools::GrowPolygon", "PlotTools")
  PlotTools::GrowPolygon(...)
}
