#' Pathways of introduction as defined by CBD
#'
#' @description A tibble containing all CBD pathways of introduction at level 1 
#' (`pathway_level1`) and level 2 (`pathway_level2`). Added pathway `unknown` 
#' at level 1 and level 2 for classifying taxa without pathway (at level 1 or 
#' level 2) information.
#'
#' @format A tibble with 51 rows and 2 variables:
#' - `pathway_level1`: (character) CBD pathway level 1
#' - `pathway_level2`: (character) CBD pathway level 2
#' 
#' @source [CBD Standard](https://www.cbd.int/)
#' @family "checklist data"
"pathways_cbd"
