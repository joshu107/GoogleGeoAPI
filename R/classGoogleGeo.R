#'@export
googlegeo_api <- function(x) {
'Generates an object of the class googlegeo_api.'

  suppressMessages(devtools::use_package('httr'))
  suppressMessages(devtools::use_package('jsonlite'))

  type = NULL

  if (!any(class(x) == 'adrs', class(x) == 'coord')) {
    stop('Please specify an object of class adrs or coord.')
  }

  if (class(x) == 'adrs') {
    type= 'geo'
  }
  else {
    type = 'rev_geo'
  }

  url <- paste('https://maps.googleapis.com/maps/api/geocode/json?',
               formatting(x),
               '&key=AIzaSyBcfnmk2l4u4kDwz3Uq-xlOB_Z9l4OZZTE',
               sep = '')

  resp <- httr::GET(url)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)


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


#'(Reverse) Geocoding of an address (coordinates).
#'
#'This function returns the result of (reverse) geocoding. (Reverse) Geocoding
#'describes the process of finding the coordinates to a given address
#'(the address which belongs to given coordinates).
#'
#'@param x Object of class \code{googlegeo_api}.
#'
#'@examples
#'x <- adrs('Linköping Universitetet, Linköping')
#'y <- googlegeo_api(x)
#'geocode(y)
#'
#'@export
geocode <- function(x) UseMethod('geocode', x)

#'@export
geocode.googlegeo_api <- function(x) {
  if(x$type == 'geo' && x$content$status != 'ZERO_RESULTS') {
    coords <- c(x$content$results[[1]]$geometry$location$lat,
                x$content$results[[1]]$geometry$location$lng)
    return(coord(coords[1], coords[2]))
  }

  else if (x$type == 'rev_geo' && x$content$status != 'ZERO_RESULTS') {
    address <- adrs(c(x$content$results[[1]]$formatted_address))
    return(address)
  }

  else {
    stop('Address or coordinates were not found.')
  }
}

#'@export
geocode.adrs <- function(address) {
  # Returns coord object.
  #
  # Args:
  #   address: adrs object with address
  #
  # Returns:
  #   A coord object processed via Google Geo API.
  jsonObject <- googlegeo_api(address)

  coordObject <- geocode(jsonObject)

  return(coordObject)
}

