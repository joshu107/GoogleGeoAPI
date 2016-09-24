address <- function(street, housenumber, state) {
  'Function for creating objects of class address.'
  structure(
    list(
      street = street,
      housenumber = housenumber,
      state = state
    ),
    class = 'address'
  )
}

print.address <- function(x, ...) {
  cat('Address:\n')
  cat('---\n')
  cat(paste(paste(x$street, x$housenumber, sep = ' '), x$state, sep = ', '))
}
