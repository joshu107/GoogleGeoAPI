#' Lorem ipsulum
#'
#' Lorem ipsulum
#'
#'@param street A string
#'@param housenumber A string
#'@param state A string
#'
#'
#'@export
adrs <- function(street, housenumber, state) {
  'Function for creating objects of class address.'

  # Check if the arguments have a valid type.
  if(!any(sapply(c(street, housenumber, state), is.character))) {
    stop('Please specify all arguments as a string.')
  }

  structure(
    list(
      street = street,
      housenumber = housenumber,
      state = state
    ),
    class = 'adrs'
  )
}

#'@export
print.adrs <- function(x, ...) {
  cat('Address:\n')
  cat('---\n')
  cat(paste(paste(x$street, x$housenumber, sep = ' '), x$state, sep = ', '))
}

#'@export
formatting <- function(x, ...) UseMethod('formatting', x)

#'@export
formatting.adrs <- function(x, ...) {
  paste(paste(x$housenumber, x$street, sep = '+'), x$state, sep =',+')
}
