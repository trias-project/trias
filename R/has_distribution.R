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
