#'A S3 class to represent the address for a geocoding request.
#'
#'Creates an object of the class \code{adrs}. The \code{adrs} object contains a
#'single string representing the address of a place.
#'
#'@param x A string with address. Address should be specified in the format used
#'  by the national postal service of the country concerned.
#'
#'@field address Address (string).
#'
#'@examples
#'x <- adrs('Linköpings Universitet, 58183 Linköping')
#'print(x)
#'
#'@seealso
#'\href{https://developers.google.com/maps/documentation/#'geocoding/
#'intro?hl=de}{Google Geo API Description}
#'
#'@export
adrs <- function(a) {
  'Function for creating objects of class address.'

  #Check if all arguments were set
  if(missing(a)) {
    stop('Please specify all arguments.')
  }

  # Check if the arguments have a valid type.
  if(!is.character(a)) {
    stop('Please specify all arguments as a string.')
  }

  structure(
    list(
      address = a
    ),
    class = 'adrs'
  )
}

#'@export
print.adrs <- function(x, ...) {
  cat('Adress:\n')
  cat('---\n')
  cat(x$address)
}
