[![R-CMD-check](https://github.com/GranadoIgor/miscellaneousR/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/GranadoIgor/miscellaneousR/actions/workflows/R-CMD-check.yml)

# miscellaneousR
A collection of miscellaneous R functions.

**Installation**

```R
devtools::install_github("GranadoIgor/miscellaneousR")
library(miscellaneousR)
```

## List of functions
- `roundLatLon()`, rounds lon/lat coordinates at the same resolution as the desired grid. It can be used to aggregate spatial point data.
- `naCbind()` and `naRbind()`, combine R Objects with different length by Rows or Columns. If the length of the R objects differ, NAs are added.
- `downloadCopernicus()`, download products from Copernicus Marine Environment Monitoring Service (CMEMS).
