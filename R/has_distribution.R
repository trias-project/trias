#' Compare desired distribution information with actual one.
#' This function compares distribution information based on a single taxon key with 
#' user requests and returns a logical (TRUE or FALSE).
#' It is a function built on rgbif function `name_usage()`.
#' @param taxon_key (single numeric or character) a single taxon key.
#' @param ... one or more distribution properties and related values listed by GBIF:
#' http://rs.gbif.org/extension/gbif/1.0/distribution.xml.
#' @return A data.frame with taxon_keys as first column and a second column 
#' (logical: TRUE or FALSE) based on match with user arguments
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rgbif name_usage
#' @importFrom lazyeval interp
#' @importFrom dplyr mutate_all select intersect
#' @importFrom purrr map map_df cross_df
#' @importFrom stringr str_split
has_distribution <- function(taxon_key, ...) {
  # GBIF_distr_terms <- c("locationID", "locality", "country", "lifeStage",
  #                       "occurrenceStatus", "threatStatus", "establishmentMeans",
  #                       "appendixCITES", "eventDate", "startDayOfYear",
  #                       "endDayOfYear", "source", "occurrenceRemarks",
  #                       "datasetID")
  
  # df with all possible combinations of user's distribution properties values
  user_properties <- list(...) %>% purrr::cross_df()
  # set all characters uppercase
  user_properties %<>% dplyr::mutate_all(funs(toupper))
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
