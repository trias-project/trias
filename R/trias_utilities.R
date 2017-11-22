#' Small function Utilities within trias package.
#'
#' Collection of short functions used in other R scripts within trias package.

split_path <- function(path) {
  rev(setdiff(strsplit(path,"/|\\\\")[[1]], ""))
}

extension <- function(path) {
  path_file <- split_path(path)
  rev(setdiff(strsplit(path_file[[1]],"[.]")[[1]], ""))
}
