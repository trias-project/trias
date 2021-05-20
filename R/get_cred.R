get_cred <- function(x){
  library(svDialogs)
  
  cred <- Sys.getenv(x)
  
  if(cred == ""){
    input <- dlgInput(paste0("What is your ", x, "?"))
    cred <- input$res
    do.call(Sys.setenv, as.list(setNames(cred, x)))
  }
  return(cred)
}