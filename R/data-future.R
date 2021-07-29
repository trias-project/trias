#' Future climate shapefiles
#' 
#' @description These shapefiles contain future worldwide climate
#'   classifications for different year intervals.
#'   
#' Future scenarios are dependent on several variables like pollution levels.
#' Currently the `future`
#' datapackage contains the following scenarios:
#' - A1FI: from Rubel & Kottek 2010, quick economic and technological growth
#' through intensive use of fossil fuel
#' - Beck: from Beck et al. 2018, high emissions
#' @source [Rubel & Kottek 2010](http://dx.doi.org/10.1127/0941-2948/2010/0430) and
#' [Beck et al. 2018](https://doi.org/10.1038/sdata.2018.214).
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
#' @family climate data
"future"
