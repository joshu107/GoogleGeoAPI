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
    stop('Test')
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

#'
#'@export
print.adrs <- function(x, ...) {
  cat('Address:\n')
  cat('---\n')
  cat(paste(paste(x$street, x$housenumber, sep = ' '), x$state, sep = ', '))
}
