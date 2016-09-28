#'A S3 class to represent the coordinate for a reverse geocoding request.
#'
#'Creates an object of the class \code{coord}. The latitude and the longitude
#'can only be specified within a specific range of real values. Please use
#'\code{.} to seperate the decimal values.
#'
#'@param lat A real value
#'@param lng A real value
#'
#'@examples
#'x <- coord(50.483, -20.827)
#'print(x)
#'
#'@seealso \href{https://developers.google.com/maps/documentation/geocoding/intro?hl=de}{Google Geo API Description}
#'
#'@export

coord <- function(lat, lng) {
  'Function for creating objects of class coord.'

  #Check if arguments have the right format
  if(!any(is.double(lat), is.double(lng))) {
    stop('Latitude and longitude have to be values of the type real.')
    stop()
  }

  #Check if arguments are in the correct range
  if(any(lat < -90, lat > 90)) {
    stop('Please specify the latitude in the range of -90 to 90 degrees.')
  }

  if(any(lng < -180, lng > 180)) {
    stop('Please specify the longitude in the range of -180 to 180 degrees.')
  }

  structure(
    list(
      lat = lat,
      lng = lng
    ),
    class = 'coord'
  )
}

#'@export
print.coord <- function(x, ...) {
  cat('Coordinates:\n')
  cat('---\n')
  cat('Latitude: ', x$lat)
  cat('\nLongitude: ', x$lng)
}
