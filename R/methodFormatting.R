#'@export
formatting <- function(x) UseMethod('formatting', x)

#'@export
formatting.coord <- function(x) {
  paste(x$lat, x$lng, sep = ',')
}

#'@export
formatting.adrs <- function(x) {
  paste(paste(gsub("[[:space:]]", "", x$housenumber),
              gsub("[[:space:]]", "", x$street), sep = '+'),
        paste(gsub("[[:space:]]", "", x$zip),
              gsub("[[:space:]]", "", x$state), sep = '+'),
        sep = ',+')
}
