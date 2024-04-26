#' Interactively get & store credentials in the system environment
#'
#' @param x a character with the name of the system environment variable to get
#'   and store
#' @return a character vector containing the value of the system variable
#' @export
#' @importFrom svDialogs dlgInput
get_cred <- function(x){
  
  cred <- Sys.getenv(x)
  
  if(cred == ""){
    input <- dlgInput(paste0("What is your ", x, "?"))
    cred <- input$res
    do.call(Sys.setenv, as.list(purrr::set_names(cred, x)))
  }
  return(cred)
}