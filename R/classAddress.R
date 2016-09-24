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
