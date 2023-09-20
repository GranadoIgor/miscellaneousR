#' @title To get the season given a set of dates
#'
#' @usage getSeason(date)
#'
#' @param date A date vector with a '%Y-%m-%d' format.
#' @return This function return 'TRUE' or 'FALSE' label based on whether the point is on land or not.
#'
#' @export
getSeason <- function(date) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Fall Equinox

  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(date, format="2012-%m-%d"))

  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Autumn")))
}
