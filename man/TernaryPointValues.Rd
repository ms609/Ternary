% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Contours.R
\name{TernaryPointValues}
\alias{TernaryPointValues}
\alias{TernaryDensity}
\title{Evaluate function over a grid}
\usage{
TernaryPointValues(
  Func,
  resolution = 48L,
  direction = getOption("ternDirection", 1L),
  ...
)

TernaryDensity(
  coordinates,
  resolution = 48L,
  direction = getOption("ternDirection", 1L)
)
}
\arguments{
\item{Func}{Function that takes three arguments named \code{a}, \code{b} and \code{c}, and
returns a numeric vector of length \emph{n}.
\code{a}, \code{b} and \code{c} will each be a vector of length \emph{n}. Together, they
specify the series of coordinates at which the function should be evaluated.}

\item{resolution}{The number of triangles whose base should lie on the longest
axis of the triangle.  Higher numbers will result in smaller subdivisions and smoother
colour gradients, but at a computational cost.}

\item{direction}{(optional) Integer specifying the direction that the
current ternary plot should point: 1, up; 2, right; 3, down; 4, left.}

\item{\dots}{Additional parameters to \code{Func()}.}

\item{coordinates}{A list, matrix, data.frame or vector in which each
element (or row) specifies
the three coordinates of a point in ternary space.}
}
\value{
\code{TernaryPointValues()} returns a matrix whose rows correspond to:
\itemize{
\item \strong{x}, \strong{y}: co-ordinates of the centres of smaller triangles
\item \strong{z}: The value of \code{Func(a, b, c, ...)}, where \code{a}, \code{b} and \code{c} are the
ternary coordinates of \code{x} and \code{y}.
\item \strong{down}: \code{0} if the triangle concerned points upwards (or right),
\code{1} otherwise
}
}
\description{
Intended to facilitate coloured contour plots with \code{\link[=ColourTernary]{ColourTernary()}},
\code{TernaryPointValue()} evaluates a function at points on a triangular grid;
\code{TernaryDensity()} calculates the density of points in each grid cell.
}
\examples{
TernaryPointValues(function (a, b, c) a * b * c, resolution = 2)

TernaryPlot(grid.lines = 4)
cols <- TernaryPointValues(rgb, resolution = 4)
text(as.numeric(cols["x", ]), as.numeric(cols["y", ]),
     labels =  ifelse(cols["down", ] == "1", "v", "^"),
     col = cols["z", ])

TernaryPlot(axis.labels = seq(0, 10, by = 1))

nPoints <- 4000L
coordinates <- cbind(abs(rnorm(nPoints, 2, 3)),
                     abs(rnorm(nPoints, 1, 1.5)),
                     abs(rnorm(nPoints, 1, 0.5)))

density <- TernaryDensity(coordinates, resolution = 10L)
ColourTernary(density, legend = TRUE, bty = "n", title = "Density")
TernaryPoints(coordinates, col = "red", pch = ".")
}
\seealso{
Other contour plotting functions: 
\code{\link{ColourTernary}()},
\code{\link{TernaryContour}()},
\code{\link{TernaryDensityContour}()}
}
\author{
\href{https://orcid.org/0000-0001-5660-1727}{Martin R. Smith}
(\href{mailto:martin.smith@durham.ac.uk}{martin.smith@durham.ac.uk})
}
\concept{contour plotting functions}
