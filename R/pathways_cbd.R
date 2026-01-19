#' Pathways of introduction as defined by CBD
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' It is deprecated as of trias 3.1.3. Please use the data frame
#' [pathwayscbd]() directly instead.
#'
#' @return A tibble data.frame with 2 columns: `pathway_level1` and
#'   `pathway_level2`.
#' @export
#' @usage pathways_cbd()
pathways_cbd <- function() {
  lifecycle::deprecate_warn(
    "3.1.3",
    "pathways_cbd()",
    details = "Please use the data frame `pathwayscbd` directly instead."
  )
  pathwayscbd
}