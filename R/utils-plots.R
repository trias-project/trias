#' Generate nice sequence with slightly different cut values compared to \code{\link[base]{seq}}
#' 
#' The inner values are forced to be a multiple of the stepsize
#' 
#' @param start_year (integer) The min cut value.
#' @param end_year (integer) The max cut value.
#' @param step_size (integer) The max distance between two cut values.
#' 
#' @return (integer vector) All cut values.
#' 
#' @noRd
nice_seq <- function(start_year, end_year, step_size) {
  
  # Calculate the first "nice" cut point (round up to the nearest multiple of step_size)
  first_nice_cut <- ceiling(start_year / step_size) * step_size
  nice_cuts <- seq(from = first_nice_cut, to = end_year, by = step_size)
  
  c(
    start_year, 
    nice_cuts,
    if (end_year > utils::tail(nice_cuts, n = 1)) end_year 
  )
}
