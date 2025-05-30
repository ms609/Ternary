% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Holdridge.R
\name{HoldridgeHypsometricCol}
\alias{HoldridgeHypsometricCol}
\title{Convert a point in evapotranspiration-precipitation space to an appropriate
cross-blended hypsometric colour}
\usage{
HoldridgeHypsometricCol(pet, prec, opacity = NA)
}
\arguments{
\item{pet, prec}{Numeric vectors giving \emph{p}otential \emph{e}vapo\emph{t}ranspiration
ratio and annual \emph{prec}ipitation (in mm).}

\item{opacity}{Opacity level to be converted to the final two characters
of an \acronym{RGBA} hexadecimal colour definition, e.g. \verb{#000000FF}.
Specify a character string, which will be interpreted as a hexadecimal
alpha value and appended to the six \acronym{RGB} hexadecimal digits;
a numeric in the range 0 (transparent) to 1 (opaque);
or \code{NA}, to return only the six \acronym{RGB} digits.}
}
\value{
Character vector listing \acronym{RGB} or (if \code{opacity != NA})
\acronym{RGBA} values corresponding to each PET-precipitation value pair.
}
\description{
Used to colour \code{HoldridgeHexagons()}, and may also be used to aid the
interpretation of PET + precipitation data in any graphical context.
}
\examples{
HoldridgePlot(hex.col = HoldridgeHypsometricCol)
VeryTransparent <- function(...) HoldridgeHypsometricCol(..., opacity = 0.3)
HoldridgePlot(hex.col = VeryTransparent)
pet <- holdridge$PET
prec <- holdridge$Precipitation
ptCol <- HoldridgeHypsometricCol(pet, prec)
HoldridgePoints(pet, prec, pch = 21, bg = ptCol)
}
\references{
Palette derived from the hypsometric colour scheme presented at
\href{https://www.shadedrelief.com/hypso/hypso.html}{Shaded Relief}.
}
\seealso{
Other Holdridge plotting functions: 
\code{\link{AddToTernary}()},
\code{\link{HoldridgePlot}()},
\code{\link{holdridge}},
\code{\link{holdridgeClasses}}
}
\author{
\href{https://orcid.org/0000-0001-5660-1727}{Martin R. Smith}
(\href{mailto:martin.smith@durham.ac.uk}{martin.smith@durham.ac.uk})
}
\concept{Holdridge plotting functions}
