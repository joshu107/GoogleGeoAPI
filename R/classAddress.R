address <- function(street, housenumber, state) {

  structure(
    list(
      street = street,
      housenumber = housenumber,
      state = state
    ),
    class = 'address'
  )
}
