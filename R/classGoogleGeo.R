#'@export
googlegeo_api <- function(address, ...) {
'Generates an object of the class googlegeo_api.'

  devtools::use_package('httr')
  devtools::use_package('jsonlite')

  url <- paste('https://maps.googleapis.com/maps/api/geocode/json?address=',
               formatting(address),
               '&key=AIzaSyBcfnmk2l4u4kDwz3Uq-xlOB_Z9l4OZZTE',
               sep = '')

  resp <- httr::GET(url)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  structure(
    list(
      content = parsed,
      path = url,
      response = resp
    ),
    class = 'googlegeo_api'
  )
}

#'@export
print.googlegeo_api <- function(x, ...) {
  str(x$content)
}

#'@export

coord <- function(x, ...) {
  UseMethod('coord', x)
}

#'@export
coord.googlegeo_api <- function(x, ...) {
  cat('Coordinates of the address:\n')
  cat('----\n')
  cat('Latitude:', x$content$results[[1]]$geometry$location$lat,
      '\nLongitude:', x$content$results[[1]]$geometry$location$lng,
      sep = ' ')
}
