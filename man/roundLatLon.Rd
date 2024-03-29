% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Round_Lat_Lon.R
\name{roundLatLon}
\alias{roundLatLon}
\title{Round lon/lat coordinates at the same resolution as the desired grid}
\usage{
roundLatLon(
  x,
  origin = 0,
  cellsize = 1,
  where = c("bottom-Left", "middle", "top-right")
)
}
\arguments{
\item{x}{Integer or numeric vector containing the lon/lat information.}

\item{origin}{Integer or character vector containing target nodes IDs.}

\item{cellsize}{A non-negative integer or numeric vector containing the edges weights.}

\item{where}{Character indicating where the rounding has to occur with respect to the origin or grid cells (default 'Bottom-Left').}
}
\description{
The \code{roundLatLon} function rounds lon/lat coordinates at the same resolution as the desired grid.
It can be used to aggregate spatial point data
}
\examples{
 lon = seq(1, 5, 0.01)
 lat = seq(1, 5, 0.01)
 roundLatLon(lon, origin = 0, cellsize = .123, where = "middle")
 roundLatLon(lat, origin = 0, cellsize = .123, where = "middle")

}
