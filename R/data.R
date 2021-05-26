#' Climate shapefiles
#' 
#' @description These shapefiles contain worldwide climate classifications for a
#'   given period and are grouped according to their period.
#'   
#' Historical/observed climate classification shapes are grouped in the
#' `observed` datapackage. These shapes originate from [Rubel & Kottek
#' 2010](http://dx.doi.org/10.1127/0941-2948/2010/0430), except the last one,
#' with data from 1980 to 2016, which is based on [Beck et al.
#' 2018](https://doi.org/10.1038/sdata.2018.214). 
#' 
#' Future scenarios are dependant on several variables like pollution levels.
#' [Rubel & Kottek 2010.](http://dx.doi.org/10.1127/0941-2948/2010/0430) and
#' [Beck et al. 2018](https://doi.org/10.1038/sdata.2018.214) describe several
#' scenarios for the possible future climates. Currently the `future`
#' datapackage contains the following scenarios:
#' - A1FI: from Rubel & Kottek 2010. Quick economic and technological growth
#' through intensive use of fossil fuel
#' - Beck: from Beck et al. 2018. high emissions
#' 
#' Legends are stored in the `legend` datapackage
#' @family climate_shapefiles
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
#' @name climate_shapes
"observed"
#' @rdname climate_shapes
#' @family climate_shapefiles
#' @format `future` is a list of 5 spatialpolygonsdataframes: 
#' - `2001-2025-A1F1`: A1FI scenario for the possible climate between 2001 and 2025
#' - `2026-2050-A1FI`: A1FI scenario for the possible climate between 2026 and 2050
#' - `2051-2075-A1FI`: A1FI scenario for the possible climate between 2051 and 2075
#' - `2076-2100-A1FI`: A1FI scenario for the possible climate between 2076 and 2100
#' - `2071-2100_Beck`: Beck scenario for the possible climate between 2071 and 2100
#' 
#' Each spatialpolygonsdataframe contains 2 variables:
#' - `ID`: polygon identifier
#' - `GRIDCODE`: grid value corresponding to a climate zone
"future"
#' @rdname climate_shapes
#' @family climate_shapefiles
#' @format `legend` contains two data.frames, `KG_A1FI` and `KG_Beck_Legend`,
#'   matching [Köppen-Geiger climate
#'   zones](https://en.wikipedia.org/wiki/K%C3%B6ppen_climate_classification) to
#'   A1FI and Beck scenarios respectively. Each data.frame contains two columns:
#' - `GRIDCODE`: (numeric) grid value corresponding to a climate zone
#' - `Classification`: (character) Köppen–Geiger climate classification value

"legend"
