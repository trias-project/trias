#' Interactively get & store credentials in the system environment
#'
#' @importFrom svDialogs dlgInput
#' @param x character. Name of the system environment variable to get
#'   and store.
#'   
#' @return character. Vector containing the value(s) of the system variable.
#' 
#' @noRd
get_cred <- function(x) {
  cred <- Sys.getenv(x)

  if (cred == "") {
    input <- dlgInput(paste0("What is your ", x, "?"))
    cred <- input$res
    do.call(Sys.setenv, as.list(purrr::set_names(cred, x)))
  }
  return(cred)
}
