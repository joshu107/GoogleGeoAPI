#'A S3 class to represent the address for geocoding
#'
#'The S3 class address (\code{adrs}) is used to store the street, housenumber,
#'zip code and the state of an \code{adrs} object.
#'
#'@param street A string
#'@param housenumber A string
#'@param zip A string
#'@param state A string
#'
#'@export
adrs <- function(street, housenumber, state, zip) {
  'Function for creating objects of class address.'

  #Check if all arguments were set
  if(any(missing(street), missing(housenumber),
         missing(state), missing(zip))) {
    stop('Please specify all arguments.')
  }

  # Check if the arguments have a valid type.
  if(!any(sapply(c(street, housenumber, state, zip), is.character))) {
    stop('Please specify all arguments as a string.')
  }

  structure(
    list(
      street = street,
      housenumber = housenumber,
      state = state,
      zip = zip
    ),
    class = 'adrs'
  )
}

#'@export
print.adrs <- function(x, ...) {
  cat('Adress:\n')
  cat('---\n')
  cat(paste(paste(x$street, x$housenumber, sep = ' '),
            paste(x$zip, x$state, sep = ' ') , sep = ', '))
}
