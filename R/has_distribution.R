#' Compare desired distribution information with actual one.
#' 
#' This function compares distribution information based on a single taxon key with 
#' user requests and returns a logical (TRUE or FALSE).
#' It is a function built on rgbif function `name_usage()`.
#' @param taxon_key (single numeric or character) a single taxon key.  
#' @param ... one or more distribution properties and related values. 
#' Tested to support at least the following properties: 
#' country, status and establishedMeans (the last one doesn't show up among 
#' distribution properties in GBIF, but it is well present if accessed 
#' via verbatim). 
#' @return a logical, TRUE or FALSE.
#' @examples 
#' # numeric taxonKey, atomic parameters
#' has_distribution(134086954, country = "BE", status = "DOUBTFUL")
#' 
#' #character taxonKey, distribution properties as vectors
#' has_distribution("134086954", country = c("NL","BE"), 
#'                  status = c("PRESENT", "DOUBTFUL"))
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rgbif name_usage
#' @importFrom lazyeval interp
#' @importFrom dplyr mutate_all select intersect
#' @importFrom purrr map map_df cross_df
#' @importFrom stringr str_split
has_distribution <- function(taxon_key, ...) {
  # df with all possible combinations of user's distribution properties values
  GBIF_distr_terms <- c("country", 
                        "status","establishmentMeans")
  user_properties <- list(...) 
  
  assertthat::assert_that(all(length(names(user_properties)) == 
                                length(unique(names(user_properties)))),
                          msg = paste("Duplicates in property assignments.", 
                                      "Use vectors for multiple assignments."))
  
  # remove duplicates (country <-> countryStatus status <-> occurrenceStatus)
  if (all(c("country", "countryCode") %in% names(user_properties))) {
    warning(paste("country and countryCode are alternative names of the", 
                  "same property. countryCode removed."))
    user_properties$countryCode <- NULL
  }
  if (all(c("status", "occurrenceStatus") %in% names(user_properties))) {
    warning(paste("status and occurrenceStatus are alternative names of the", 
                  "same property. occurrenceStatus removed."))
    user_properties$occurrenceStatus <- NULL
  }
  
  # accept but rename the alternative name countryCode and occurrenceStatus
  if ("countryCode" %in% names(user_properties)) {
    warning("countryCode renamed to country")
    names(user_properties) <- replace(names(user_properties), 
                                      names(user_properties) == "countryCode", 
                                      "country")
  }
  
  if ("occurrenceStatus" %in% names(user_properties)) {
    warning("occurrenceStatus renamed to status")
    names(user_properties) <- replace(names(user_properties), 
                                      names(user_properties) == "occurrenceStatus", 
                                      "status")
  }
  
  assertthat::assert_that(
    all(names(user_properties) %in% GBIF_distr_terms),
    msg = "Invalid distribution properties.")
  
  # create all possible combinations of distribution properties
  user_properties %<>% purrr::cross_df()
  # set all characters uppercase
  user_properties %<>% dplyr::mutate_all(funs(toupper))
  
  # retrieve distribution properties from GBIF
  distr_properties <- rgbif::name_usage(key = as.integer(taxon_key),
                                        return = "data",
                                        data = "distribution") %>% 
    dplyr::select(names(user_properties))
  
  # df -> list -> split all properties values by "," -> df with all combinations
  distr_properties %<>% as.list() %>% 
    purrr::map(~stringr::str_split(., pattern = ",")) %>% 
    unlist(recursive = FALSE) %>% expand.grid() %>% 
    set_colnames(names(distr_properties)) %>% purrr::map_df(~as.character(.))
  return(dplyr::intersect(user_properties, distr_properties) %>% nrow > 0)
}
