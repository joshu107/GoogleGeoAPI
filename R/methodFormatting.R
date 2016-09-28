#'Formatting of address or coordinates.
#'
#'Returns a formatted string of an \code{adrs} or \code{coord} object.
#'
#'@param x Object of class \code{adrs} or \code{coord}.
#'
#'@examples
#'x <- adrs('Linköping University, Linköping')
#'formatting(x)
#'
#'y <- coord(50.231, -17.8239)
#'formatting(y)
#'
#'@export
formatting <- function(x) UseMethod('formatting', x)

#'@export
formatting.coord <- function(x) {
  paste0('latlng=',paste(x$lat, x$lng, sep = ','))
}

#'@export
formatting.adrs <- function(x) {
  x <- gsub(',', replacement = '', x)
  paste0('address=', gsub('[[:space:]]', replacement = '+', x))
}
