#' Check keys against GBIF Backbone Taxonomy
#'
#' This function performs three checks: \itemize{\item{\code{keys} are valid
#' GBIF taxon keys (https://www.gbif.org/species/key_value returns a page
#' related to a taxa)} \item{\code{keys} are taxon keys of the GBIF Backbone
#' Taxonomy checklist (https://www.gbif.org/species/key_value returns a page
#' related to a taxa of the GBIF Backbone)} \item{\code{keys} are synonyms
#' (neither \code{ACCEPTED} nor \code{DOUBTFUL})}}.
#' @param keys: a vector, a list, or a data.frame with column \code{key}.
#'
#' @return a dataframe with the following columns: \itemize{\item{\code{key}}{:
#'   (numeric) invalid keys} \item{\code{is_taxonKey}} {: (logical) is the key
#'   a valid GBIF taxon key?} \item{\code{is_from_gbif_backbone}} {: (logical)
#'   is the key a valid taxon key from GBIF Backbone Taxonomy checklist?}
#'   \item{\code{is_synonym}} {: (logical) is the key related to a synonym (not
#'   \code{ACCEPTED} or \code{DOUBTFUL})?}}
#' @examples
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr pull mutate filter left_join full_join rename select
#' @importFrom rgbif name_usage
#' @importFrom purrr map_dfr reduce map_df
#' @importFrom tidyr gather
gbif_verify_keys <- function(keys) {
  assert_that(is.data.frame(keys) | is.vector(keys), 
              msg = "keys should be a vector, a named list or a data.frame.")
  if (is.data.frame(keys)) {
    name_cols <- c("key")
    assert_that(all(name_cols %in% names(keys)), 
                msg = "Columns 'key' not found.")
    keys <- keys %>% pull(name_cols)
  }
  keys <- keys[!is.na(keys)]
  if (length(keys) == 0) {
    return(NULL)
  }
  names(keys) <- as.character(keys)
  gbif_info <- keys %>%
    map(~ try(name_usage(., return = "data")[1,]))
  check_keys <- map_df(gbif_info, ~ is.error(.) == FALSE) %>%
    gather(key = key, value = is_taxonKey) %>%
    mutate(key = as.numeric(key))
  invalid_keys <- check_keys %>%
    filter(is_taxonKey == FALSE)
  
  valid_keys <- gbif_info[which(! names(gbif_info) %in% invalid_keys$key)]
  valid_keys_df <- valid_keys %>% 
    reduce(bind_rows) %>%
    mutate(is_from_gbif_backbone = ifelse(datasetKey == uuid_backbone,
                                          TRUE, FALSE))
  # valid_keys_df$is_from_gbif_backbone[2] <- FALSE
  check_keys <- check_keys %>%
    left_join(valid_keys_df %>% 
                select(key, is_from_gbif_backbone),
              by = "key")
  invalid_keys <- invalid_keys %>% 
    full_join(check_keys %>%
                filter(is_from_gbif_backbone == FALSE),
              by = c("key", "is_taxonKey"))
  
  valid_keys_df <- valid_keys_df %>% 
    filter(is_from_gbif_backbone == TRUE) %>%
    mutate(is_synonym = ifelse(! taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL"),
                               TRUE, FALSE))
  check_keys <- check_keys %>%
    left_join(valid_keys_df %>% 
                select(key, is_synonym),
              by = "key")
  invalid_keys <- invalid_keys %>% 
    full_join(check_keys %>%
                filter(is_synonym == TRUE),
              by = c("key", "is_taxonKey", "is_from_gbif_backbone"))
  return(invalid_keys)
}

is.error <- function(x) inherits(x, "try-error")
uuid_backbone = "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"