#' Legends for climate shapefiles
#' 
#' @format `legend` contains two data.frames, `KG_A1FI` and `KG_Beck`,
#'   matching Koppen-Geiger climate zones to A1FI and Beck scenarios
#'   respectively. 
#'   
#'   Each data.frame contains two columns: 
#'   - `GRIDCODE`: (numeric) grid value corresponding to a climate zone 
#'   - `Classification`: (character) Koppen-{G}eiger climate classification value
#'   - `Description`: (character) verbose description of the Koppen-{G}eiger
#'   climate zone, e.g. "Tropical rainforest climate"
#'   - `Group`: (character) group the Koppen-{G}eiger climate zone belongs to,
#'   e.g. "Tropical"
#'   - `Precipitation Type`: (character) Type of precipitations associated to
#'   the climate zone, e.g. "Rainforest"
#'   - `Level of Heat`: (character) Heat level associated to the climate zone,
#'   e.g. "Cold"
#' @source [Koppen-Geiger climate zones](https://hess.copernicus.org/articles/11/1633/2007/)
#' @family climate data
"legends"
