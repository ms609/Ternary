#' Eight-colour palette compatible with colour blindness
#' 
#' An eight-colour palette recommended for use with colour blind audiences.
#' 
#' @examples {
#' data('cbPalette8')
#' dev.new(width=8, height=1, units='cm')
#' par(mar=rep(0, 4))
#' plot(0, type='n', xlim=c(1, 8), ylim=c(0, 1), axes=FALSE)
#' points(1:8, rep(0, 8), col=cbPalette8, pch=15)
#' text(1:8, 0.5, col=cbPalette8)
#' }
#' 
#' @source \cite{Wong B. 2011. Color blindness. Nat. Methods. 8:441. \doi{10.1038/nmeth.1618}}
#' @seealso [cbPalette15]
#' @keywords datasets
"cbPalette8"

#' Fifteen-colour palette compatible with colour blindness
#' 
#' A fifteen-colour [Brewer palette](http://mkweb.bcgsc.ca/brewer/) comprehensible
#' by colour blind viewers.
#' 
#' Note that colour 4 is difficult to distinguish from colour 13 in individuals with tritanopia.
#' Likewise, colour 7 is difficult to distinguish from colour 3.
#' You may wish to use `cbPalette13 <- cbPalette15[-c(4, 7)]`.
#' 
#' @examples {
#' data('cbPalette15')
#' dev.new(width=8, height=1, units='cm')
#' par(mar=rep(0, 4))
#' plot(0, type='n', xlim=c(1, 15), ylim=c(0, 1), axes=FALSE)
#' points(1:15, rep(0, 15), col=cbPalette15, pch=15)
#' text(1:15, 0.5, col=cbPalette15)
#' }
#' 
#' @seealso [cbPalette8]
#' @source \url{http://mkweb.bcgsc.ca/biovis2012/color-blindness-palette.png}
#' @keywords datasets
"cbPalette15"

