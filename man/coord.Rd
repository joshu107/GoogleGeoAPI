% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classCoord.R
\name{coord}
\alias{coord}
\title{A S3 class to represent the coordinate for a reverse geocoding request.}
\usage{
coord(lat, lng)
}
\arguments{
\item{lat}{latitude (real value)}

\item{lng}{longitude (real value)}
}
\description{
Creates an object of the class \code{coord}. The latitude and the longitude
can only be specified within a specific range of real values. Please use
\code{.} to seperate the decimal values.
}
\section{Fields}{

\describe{
\item{\code{lat}}{Latitude (real value)}

\item{\code{lng}}{Longitude (real value)}
}}
\examples{
x <- coord(50.483, -20.827)
print(x)

}
\seealso{
\href{https://developers.google.com/maps/documentation/geocoding/intro?hl=de}{Google Geo API Description}
}

