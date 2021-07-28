#' Historical climate shapefiles
#' 
#' @description These shapefiles contain worldwide climate classifications for
#'   different year intervals.
#' @source These shapes originate from [Rubel & Kottek
#' 2010](http://dx.doi.org/10.1127/0941-2948/2010/0430), except the last one,
#' with data from 1980 to 2016, which is based on [Beck et al.
#' 2018](https://doi.org/10.1038/sdata.2018.214). 
#' @format `observed` is a list of 5 spatialpolygonsdataframes: 
#' - `1901-1925`: observed climate data from 1901 up to 1925
#' - `1925-1950`: observed climate data from 1926 up to 1950
#' - `1950- 1975`: observed climate data from 1951 up to 1975
#' - `1976-2000`: observed climate data from 1976 up to 2000
#' - `1980-2016`: observed climate data from 1980 up to 2016
#' 
#' Each spatialpolygonsdataframe contains 2 variables:
#' - `ID`: polygon identifier
#' - `GRIDCODE`: grid value corresponding to a climate zone
#' @family climate_shapefiles
"observed"