#' Polygon geometry
#' 
#' These functions have moved to "[PlotTools](
#' https://ms609.github.io/PlotTools)" and are deprecated here.
#' @param \dots Parameters to PlotTools function
#' @name Polygon-Geometry
#' @importFrom PlotTools PolygonArea PolygonCenter GrowPolygon
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
  .Deprecated("PlotTools::GrowPolygon", "PlotTools")
  PlotTools::GrowPolygon(...)
}
