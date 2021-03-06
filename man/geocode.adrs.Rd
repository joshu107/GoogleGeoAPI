% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classGoogleGeo.R
\name{geocode.adrs}
\alias{geocode.adrs}
\title{Geocoding of an address.}
\usage{
\method{geocode}{adrs}(address)
}
\arguments{
\item{x}{Object of class \code{\link{adrs}}.}
}
\description{
This function returns the result of geocoding. Geocoding
describes the process of finding the coordinates to a given address.
}
\examples{
x <- adrs('Linköping Universitetet, Linköping')
geocode(x)

}

