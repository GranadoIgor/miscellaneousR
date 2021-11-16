#' The \code{roundLatLon} function rounds lon/lat coordinates at the same resolution as the desired grid.
#' It can be used to aggregate spatial point data
#'
#' @title Round lon/lat coordinates at the same resolution as the desired grid
#' @param x Integer or numeric vector containing the lon/lat information.
#' @param origin Integer or character vector containing target nodes IDs.
#' @param cellsize A non-negative integer or numeric vector containing the edges weights.
#' @param where Character indicating where the rounding has to occur with respect to the origin or grid cells (default 'Bottom-Left').
#' @examples
#'  lon = seq(1, 5, 0.01)
#'  lat = seq(1, 5, 0.01)
#'  roundLatLon(lon, origin = 0, cellsize = .123, where = "middle")
#'  roundLatLon(lat, origin = 0, cellsize = .123, where = "middle")
#'
#' @export
roundLatLon = function(x,origin=0,cellsize=1, where=c("bottom-Left", "middle", "top-right")) {
  where = match.arg(where)
  if (where == "bottom-Left") {
    origin + cellsize * (floor((x - origin) / cellsize))
  } else if (where == "middle") {
    cellsize/2 + origin + cellsize * (floor((x- origin) / cellsize))
  } else {
    cellsize + origin + cellsize * (floor((x - origin) / cellsize))
  }
}
