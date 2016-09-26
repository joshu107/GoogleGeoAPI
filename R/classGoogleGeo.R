#'@export
googlegeo_api <- function(address = NULL, coord = NULL) {
'Generates an object of the class googlegeo_api.'

  devtools::use_package('httr')
  devtools::use_package('jsonlite')

  type = NULL

  if (!any(missing(address), missing(coord))) {
    stop('Please specify an adress or coordinates.')
  }

  else if (class(address) == 'adrs' && is.null(coord)) {
    url <- paste('https://maps.googleapis.com/maps/api/geocode/json?address=',
                 formatting(address),
                 '&key=AIzaSyBcfnmk2l4u4kDwz3Uq-xlOB_Z9l4OZZTE',
                 sep = '')

    resp <- httr::GET(url)
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
    type = 'geo'
  }

  else if (class(coord) == 'coord' && is.null(address)) {
    url <- paste('https://maps.googleapis.com/maps/api/geocode/json?latlng=',
                 formatting(coord),
                 '&key=AIzaSyBcfnmk2l4u4kDwz3Uq-xlOB_Z9l4OZZTE',
                 sep = '')

    resp <- httr::GET(url)
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
    type = 'revgeo'
  }
  else {
    stop('Test')
  }

  structure(
    list(
      content = parsed,
      path = url,
      response = resp,
      type = type
    ),
    class = 'googlegeo_api'
  )
}

#'@export
print.googlegeo_api <- function(x) {
  str(x$content)
}

#'@export
geocode <- function(x) UseMethod('geocode', x)

#'@export
geocode.googlegeo_api <- function(x) {
  if(x$type == 'geo') {
    cat('Coordinates of the address:\n')
    cat('----\n')
    cat('Latitude:', x$content$results[[1]]$geometry$location$lat,
        '\nLongitude:', x$content$results[[1]]$geometry$location$lng,
        sep = ' ')
  }

  else if (x$type == 'revgeo') {
    cat('Address of the coordinates:\n')
    cat('----\n')
    cat(x$content$results[[1]]$formatted_address)
  }

  else {
    stop('Type not specified.')
  }

}

