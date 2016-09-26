#'A S3 class to represent the coordinate for reverse geocoding

#'@export

coord <- function(lat, lng) {

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
