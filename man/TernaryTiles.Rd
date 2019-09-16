% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Contours.R
\name{TernaryTiles}
\alias{TernaryTiles}
\alias{TernaryUpTiles}
\alias{TernaryDownTiles}
\alias{TernaryLeftTiles}
\alias{TernaryRightTiles}
\title{Paint tiles on ternary plot}
\usage{
TernaryTiles(x, y, down, resolution, col,
  direction = getOption("ternDirection"))
}
\arguments{
\item{x, y}{Numeric vectors specifying \emph{x} and \emph{y} coordinates of centres of each triangle.}

\item{down}{Logical vector specifying \code{TRUE} if each triangle should point down (or right),
\code{FALSE} otherwise.}

\item{resolution}{The number of triangles whose base should lie on the longest
axis of the triangle.  Higher numbers will result in smaller subdivisions and smoother
colour gradients, but at a computational cost.}

\item{col}{Vector specifying the colour with which to fill each triangle.}

\item{direction}{(optional) Integer specifying the direction that the current ternary plot
should point: 1, up; 2, right; 3, down; 4, left.}
}
\description{
Function to fill a ternary plot with coloured tiles.  Useful in combination with
\code{\link{TernaryPointValues}} and \code{\link{TernaryContour}}.
}
\examples{

FunctionToContour <- function (a, b, c) {
  a - c + (4 * a * b) + (27 * a * b * c)
}

TernaryPlot()

values <- TernaryPointValues(FunctionToContour, resolution=24L)
ColourTernary(values)
TernaryContour(FunctionToContour, resolution=36L)


}
\author{
Martin R. Smith
}