#' Check keys against GBIF Backbone Taxonomy
#'
#' This function performs three checks: \itemize{\item{\code{keys} are valid
#' GBIF taxon keys. That means that adding a key at the end of the URL
#' https://www.gbif.org/species/ returns a GBIF page related to a taxa.}
#' \item{\code{keys} are taxon keys of the GBIF Backbone Taxonomy checklist.
#' That means that adding a key at the end of the URL
#' https://www.gbif.org/species/ returns a GBIF page related to a taxa of the
#' GBIF Backbone.)} \item{\code{keys} are synonyms of other taxa
#' (taxonomicStauts neither \code{ACCEPTED} nor \code{DOUBTFUL}).}}.
#' @param keys: (character or numeric) a vector, a list, or a data.frame with
#'   column \code{key}.
#'
#' @return a dataframe with the following columns: \itemize{\item{\code{key}}{:
#'   (numeric) keys as input keys.} \item{\code{is_taxonKey}} {: (logical) is
#'   the key a valid GBIF taxon key?} \item{\code{is_from_gbif_backbone}} {:
#'   (logical) is the key a valid taxon key from GBIF Backbone Taxonomy
#'   checklist?} \item{\code{is_synonym}} {: (logical) is the key related to a
#'   synonym (not \code{ACCEPTED} or \code{DOUBTFUL})?}} If a key didn't pass
#'   the first check (\code{is_taxonKey = FALSE}) then \code{NA} for other two
#'   columns. If a key didn't pass the second check (\code{is_from_gbif_backbone
#'   = FALSE}) then \code{is_synonym} = \code{NA}.
#'
#' @examples
#' # input is a vector
#' keys1 = c("12323785387253", # is not a GBIF taxonKey
#'           "128545334", # is not a taxonKey from Backbone, but Euglenes nitidifrons (Thomson, 1886) in "Checklist of Danish Beetles (Coleoptera)"
#'           "1000693", # is a synonym: Pterodina calcaris Langer, 1909. Synonym of Testudinella parva (Ternetz, 1892)
#'           "1000310", # is an accepted taxon: Pyrococcus woesei Zillig, 1988
#'           NA, NA)
#' # input is a df
#' keys2 <- data.frame(key = keys1,
#'                     other_col = sample.int(40, size = length(keys1)),
#'                     stringsAsFactors = FALSE)
#' # input is a named list
#' keys3 <- keys1
#' library(purrr)
#' names(keys3) <- purrr::map_chr(c(1:length(keys3)),
#'                                ~paste(sample(c(0:9, letters, LETTERS),3),
#'                                       collapse=""))
#' # input keys are numeric
#' keys4 <- as.numeric(keys1)
#' 
#' gbif_verify_keys(keys1)
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr pull mutate filter left_join full_join select
#' @importFrom rgbif name_usage
#' @importFrom purrr reduce map_df
#' @importFrom tidyr gather
gbif_verify_keys <- function(keys) {
  assert_that(is.data.frame(keys) | is.vector(keys), 
              msg = "keys should be a vector, a named list or a data.frame.")
  if (is.data.frame(keys)) {
    name_cols <- c("key")
    assert_that(all(name_cols %in% names(keys)), 
                msg = "Columns 'key' not found.")
    # extract vector of keys from df
    keys <- keys %>% pull(name_cols)
  }
  keys <- keys[!is.na(keys)]
  if (length(keys) == 0) {
    return(NULL)
  } else {
    assert_that(all(keys != "") == TRUE,
                msg = paste("Invalid keys:", 
                            paste(rep("\"\"\"\"", length(keys[which(keys == "")])), 
                                  collapse = ","), "."))
    assert_that(all(!grepl("\\D", keys)) == TRUE, 
                msg = paste("Invalid keys:",
                            paste(keys[which(!grepl("\\D", keys) == FALSE)],
                                  collapse = ","),
                            "."))
  }
  names(keys) <- as.character(keys)
  gbif_info <- keys %>%
    map(~ try(name_usage(., return = "data")[1,]))
  check_keys <- map_df(gbif_info, ~ is.error(.) == FALSE) %>%
    gather(key = key, value = is_taxonKey) %>%
    mutate(key = as.numeric(key))
  valid_keys_df <- check_keys %>%
    filter(is_taxonKey == TRUE)
  valid_keys <- gbif_info[which(names(gbif_info) %in% valid_keys_df$key)]
  if (length(valid_keys) > 0) {
    valid_keys_df <- valid_keys %>% 
      reduce(bind_rows) %>%
      mutate(is_from_gbif_backbone = ifelse(datasetKey == uuid_backbone,
                                            TRUE, FALSE))
    check_keys <- check_keys %>%
      left_join(valid_keys_df %>% 
                  select(key, is_from_gbif_backbone),
                by = "key")
    valid_keys_df <- valid_keys_df %>% 
      filter(is_from_gbif_backbone == TRUE) %>%
      mutate(is_synonym = ifelse(! taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL"),
                                 TRUE, FALSE))
    check_keys <- check_keys %>%
      left_join(valid_keys_df %>% 
                  select(key, is_synonym),
                by = "key")
  } else {
    check_keys <- check_keys %>%
      mutate(is_from_gbif_backbone = NA,
             is_synonym = NA)
  }
  return(check_keys)
}

is.error <- function(x) inherits(x, "try-error")
uuid_backbone = "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"
