#' Write a query as JSON object for downloading GBIF occurrences
#'
#' This function writes a body query (JSON object) for triggering 
#' a GBIF occurrence download.
#'
#' @param creator GBIF username
#' @param notification_address GBIF e-mail
#' @param taxonKeys vector with taxonKeys
#' @param country vector with countries
#' @return body as JSON object
#'
#' @export
#' @importFrom jsonlite unbox
#' @importFrom jsonlite toJSON

predicates <- function(taxonKey = taxonKeys, countries = countries, creator = NULL, 
                       notification_address = NULL) {
  
  if (is.null(creator)) {
    creator <- unbox(readline(prompt="GBIF username: ")[1])
  }
  if (is.null(notification_address)) {
    notification_address <-  list(readline(prompt="GBIF e-mail: "))
  }

  type3 <- unbox("equals")
  type2 <- unbox("or")
  type1 <- unbox("and")
  types <- c(type1, type2, type3)
  keys <- c("TAXON_KEY","COUNTRY")
  # countries <- list(country = c("BE", "SW")) " to test it with several countries
  values <- c(taxonKeys,countries)
  
  predicate <- list(
    type = types[1], 
    predicates = lapply(c(1:length(values)), 
                        function(i) list(type  = types[2], 
                                         predicates = lapply(c(1:length(values[[i]])),
                                                             function(j) list(type = types[3], key = unbox(keys[i]), 
                                                                              value = unbox(values[[i]][j]))))))
  request.body <- toJSON(list(creator = creator, notification_address = notification_address, 
                              predicate = predicate), pretty = TRUE, auto_unbox = TRUE)
  return(request.body)
}