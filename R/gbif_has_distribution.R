#' Compare desired distribution information with actual one.
#' 
#' This function compares GBIF distribution information based on a single taxon key with 
#' user requests and returns a logical (TRUE or FALSE).
#' It is a function built on rgbif function `name_usage()`.
#' @param taxon_key (single numeric or character) a single taxon key.  
#' @param ... one or more GBIF distribution properties and related values. 
#' Up to now it supports the following properties: 
#' country (and its synoynym: countryCode), status (and its synonym: 
#' occurrenceStatus) and establishmentMeans.
#' @return a logical, TRUE or FALSE.
#' @examples 
#' # numeric taxonKey, atomic parameters
#' gbif_has_distribution(134086954, country = "BE", status = "DOUBTFUL")
#' 
#' # character taxonKey, distribution properties as vectors
#' gbif_has_distribution("134086954", country = c("NL","BE"), 
#'                  status = c("PRESENT", "DOUBTFUL"))
#'                  
#' # use alternative names: countryCode, occurrenceStatus. 
#' Function works. Warning is given.
#' gbif_has_distribution("134086954", countryCode = c("NL","BE"), 
#'                  occurrenceStatus = c("PRESENT", "DOUBTFUL"))
#'                  
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rgbif name_usage
#' @importFrom dplyr mutate_all select intersect
#' @importFrom purrr map map_df cross_df
#' @importFrom stringr str_split
gbif_has_distribution <- function(taxon_key, ...) {
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

  # retrieve distribution properties from GBIF
  distr_properties <- rgbif::name_usage(key = as.integer(taxon_key),
                                        return = "data",
                                        data = "distribution") 

  # no ditribution properties values specified by user
  if (is.null(names(user_properties)))
    return(nrow(distr_properties) > 0)
  else {
    # taxa has no distribution
    if (nrow(distr_properties) == 0) return(FALSE)
    else {
      # make all combinations of distribution properties allowed by user
      user_properties %<>% purrr::cross_df()
      user_properties %<>% dplyr::mutate_all(funs(toupper))
      user_properties %<>% dplyr::select(names(user_properties))
      
      distr_properties %<>% dplyr::select(names(user_properties))
      distr_properties_exp <- data.frame()
      # row->list->split all properties values by ","->df with all combinations
      # ->add to expanded df 
      for (i in 1:nrow(distr_properties)) {
        distr_properties[i,] %>% as.list() %>% 
          purrr::map(~stringr::str_split(., pattern = ",")) %>% 
          unlist(recursive = FALSE) %>% expand.grid() %>% 
          set_colnames(names(distr_properties)) %>% 
          purrr::map_df(~as.character(.)) %>%
          bind_rows(distr_properties_exp,.)
      }
      return(dplyr::intersect(user_properties, distr_properties) %>% nrow > 0)
    }
  }
}