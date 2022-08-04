#' To find the if points are within a polygon shapefile given a set of coordinates
#'
#' @param lon A numeric vector with longitude information.
#' @param lat A numeric vector with latitude information.
#' @param lands Layer from which the geometries or attributes are queried. This arguments allows 'SpatialPolygons' or 'SpatialPolygonsDataFrame' class objects.
#' @param projection Character string describing a projection and datum in the PROJ.4 format.
#' @return This function return 'TRUE' or 'FALSE' label based on whether the point is on land or not.
#' @examples
#' \dontrun{# Create an artificial data set
#' x = data.table(lat=1:10, lon=1:10)
#' world = rnaturalearthdata::countries50
#' x$label = pointsOnLand(x$lon, x$lon, world)
#' head(x)
#' }
pointsOver = function(lon, lat, lands, projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") {

  as = NULL # due to NSE notes in R CMD check

  if (class(lands) == "SpatialPolygonsDataFrame")
    lands = as(lands, "SpatialPolygons")
  if (!inherits(lands, "SpatialPolygons"))
    stop("'lands' must be specified as either class 'SpatialPolygons' or 'SpatialPolygonsDataFrame'")
  spPoint = SpatialPoints(data.frame(x = lon, y = lat), proj4string = CRS(projection))
  if (is.na(proj4string(lands)))
    proj4string(lands) = projection
  if (proj4string(spPoint) != proj4string(lands))
    lands = spTransform(lands, CRS(projection))
  idx = sp::over(spPoint, lands)
  label = !is.na(idx)
  return(label)
}
