#'Runs Google Geo API.
#'
#'This function runs Runs Google Geo API on \code{\link{adrs}} or
#'\code{\link{coord}} object.
#'
#'@param x a \code{\link{adrs}} or \code{\link{coord}} object
#'
#'@return \code{googlegeo_api} object that contains result returned by Google
#'Geo API.
#'@export
googlegeo_api <- function(x) {
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
#'@param x Object of class \code{\link{adrs}} or \code{\link{coord}} (or
#'  \code{googlegeo_api}).
#'
#'@examples
#'x <- adrs('Linköping Universitetet, Linköping')
#'geocode(x)
#'y <- coord(58.40208, 15.57901)
#'geocode(y)
#'
#'@export
geocode <- function(x) UseMethod('geocode', x)

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
#'geocode(googlegeo_api(x))
#'y <- coord(58.40208, 15.57901)
#'geocode(googlegeo_api(y))
#'@export
geocode.googlegeo_api <- function(x) {
  if (x$content$status == 'INVALID_REQUEST') {
    stop(paste(x$content$error_message))
  }

  else if (x$content$status == 'ZERO_RESULTS') {
    stop('Address or coordinates were not found.')
  }

  else if(x$type == 'geo') {
    coords <- c(x$content$results[[1]]$geometry$location$lat,
                x$content$results[[1]]$geometry$location$lng)
    return(coord(coords[1], coords[2]))
  }

  else if (x$type == 'rev_geo') {
    address <- adrs(c(x$content$results[[1]]$formatted_address))
    return(address)
  }



  else {
    stop('Unknown error')
  }
}


#'Geocoding of an address.
#'
#'This function returns the result of geocoding. Geocoding
#'describes the process of finding the coordinates to a given address.
#'
#'@param x Object of class \code{\link{adrs}}.
#'
#'@examples
#'x <- adrs('Linköping Universitetet, Linköping')
#'geocode(x)
#'
#'@export
geocode.adrs <- function(address) {
  jsonObject <- googlegeo_api(address)

  coordObject <- geocode(jsonObject)

  return(coordObject)
}

#'Reverse geocoding of coordinates.
#'
#'This function returns the result of reverse geocoding. Reverse geocoding
#'describes the process of finding the address which belongs to given
#'coordinates.
#'
#'@param x Object of class \code{\link{coord}}.
#'
#'@examples
#'x <- adrs('Linköping Universitetet, Linköping')
#'geocode(x)
#'y <- coord(58.40208, 15.57901)
#'geocode(y)
#'
#'@export
geocode.coord <- function(coordinates) {
  jsonObject <- googlegeo_api(coordinates)

  adrsObject <- geocode(jsonObject)

  return(adrsObject)
}



