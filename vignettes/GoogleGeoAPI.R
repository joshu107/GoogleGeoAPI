## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github(repo = "https://github.com/Rchieve/GoogleGeoAPI",
#                           build_vignette = TRUE)

## ------------------------------------------------------------------------
library(GoogleGeoAPI)

## ------------------------------------------------------------------------
liuAdrs <- adrs("Linköpings Universitet, 58183 Linköping")

## ------------------------------------------------------------------------
print(liuAdrs)

## ------------------------------------------------------------------------
liuGeocoded <- geocode(liuAdrs)

## ------------------------------------------------------------------------
print(liuGeocoded)

## ---- eval = FALSE-------------------------------------------------------
#  liuGeocoded

## ------------------------------------------------------------------------
liuGeocoded$lat
liuGeocoded$lon

## ------------------------------------------------------------------------
liuCoord <- coord(58.40208, 15.57901)

## ------------------------------------------------------------------------
liuRGeocoded <- geocode(liuCoord)

## ------------------------------------------------------------------------
print(liuRGeocoded)

## ------------------------------------------------------------------------
liuRGeocoded$address

