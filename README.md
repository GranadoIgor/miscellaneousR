[![R-CMD-check](https://github.com/GranadoIgor/miscellaneousR/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/GranadoIgor/miscellaneousR/actions/workflows/R-CMD-check.yml)

# miscellaneousR
A collection of miscellaneous R functions.

**Installation**

```R
devtools::install_github("GranadoIgor/miscellaneousR")
library(miscellaneousR)
```

## List of functions
- `downloadCopernicus()`, download products from Copernicus Marine Environment Monitoring Service (CMEMS).
- `naCbind()` and `naRbind()`, combine R Objects with different length by Rows or Columns. If the length of the R objects differ, NAs are added.
- `pointsOver()`, finds if points are within a polygon given a set of coordinates.
- `roundLatLon()`, rounds lon/lat coordinates at the same resolution as the desired grid. It can be used to aggregate spatial point data.
