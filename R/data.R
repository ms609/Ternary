#' Eight-colour palette compatible with colour blindness
#' 
#' An eight-colour palette recommended for use with colour blind audiences.
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
#' @seealso [cbPalette8]
#' @source http://mkweb.bcgsc.ca/biovis2012/color-blindness-palette.png
#' @keywords datasets
"cbPalette15"

