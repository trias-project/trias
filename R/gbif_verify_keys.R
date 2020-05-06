#' Check keys against GBIF Backbone Taxonomy
#'
#' This function performs three checks:
#' \itemize{
#'   \item{\code{keys} are valid GBIF taxon keys. That means that adding a key
#'   at the end of the URL https://www.gbif.org/species/ returns a GBIF page
#'   related to a taxa.}
#'   \item{\code{keys} are taxon keys of the GBIF Backbone Taxonomy checklist.
#'   That means that adding a key at the end of the URL
#'   https://www.gbif.org/species/ returns a GBIF page related to a taxa of the
#'   GBIF Backbone.)}
#'   \item{\code{keys} are synonyms of other taxa (taxonomicStauts neither
#'   \code{ACCEPTED} nor \code{DOUBTFUL}).}
#'   }.
#' @param keys (character or numeric) a vector, a list, or a data.frame
#'   containing the keys to verify.
#' @param col_keys (character) name of column containing keys in case
#'   \code{keys} is a data.frame.
#'
#' @return a data.frame with the following columns:
#'   \itemize{
#'     \item{\code{key}}{:(numeric) keys as input keys.}
#'     \item{\code{is_taxonKey}} {: (logical) is the key a valid GBIF taxon
#'     key?}
#'   \item{\code{is_from_gbif_backbone}} {:(logical) is the key a valid taxon
#'   key from GBIF Backbone Taxonomy checklist?}
#'   \item{\code{is_synonym}} {: (logical) is the key related to a synonym (not
#'   \code{ACCEPTED} or \code{DOUBTFUL})?}
#'   }
#'   If a key didn't pass the first check (\code{is_taxonKey = FALSE}) then
#'   \code{NA} for other two columns. If a key didn't pass the second check
#'   (\code{is_from_gbif_backbone = FALSE}) then \code{is_synonym} = \code{NA}.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr pull mutate filter left_join full_join select .data enquo
#' @importFrom purrr reduce map_df
#' @importFrom rgbif name_usage
#' @importFrom rlang !!
#' @importFrom tidyr gather
#' @importFrom tidyselect vars_pull
#' @examples
#' # input is a vector
#' keys1 <- c(
#'   "12323785387253", # invalid GBIF taxonKey
#'   "128545334", # valid taxonKey, not a GBIF Backbone key
#'   "1000693", # a GBIF Backbone key, synonym
#'   "1000310", # a GBIF Backbone key, accepted
#'   NA, NA
#' )
#' # input is a df
#' keys2 <- data.frame(
#'   keys = keys1,
#'   other_col = sample.int(40, size = length(keys1)),
#'   stringsAsFactors = FALSE
#' )
#' # input is a named list
#' keys3 <- keys1
#' library(purrr)
#' names(keys3) <- purrr::map_chr(
#'   c(1:length(keys3)),
#'   ~ paste(sample(c(0:9, letters, LETTERS), 3),
#'     collapse = ""
#'   )
#' )
#' # input keys are numeric
#' keys4 <- as.numeric(keys1)
#'
#' gbif_verify_keys(keys1)
#' gbif_verify_keys(keys2, col_keys = "keys")
#' gbif_verify_keys(keys3)
#' gbif_verify_keys(keys4)
gbif_verify_keys <- function(keys, col_keys = "key") {
  assert_that(is.data.frame(keys) | is.vector(keys),
    msg = "keys should be a vector, a named list or a data.frame."
  )
  if (is.data.frame(keys)) {
    assert_that(col_keys %in% names(keys),
      msg = paste(
        "Column with keys not found.",
        "Did you forget maybe to pass",
        "the right column name to col_keys?"
      )
    )
    name_col <- vars_pull(names(keys), !!enquo(col_keys))
    # extract vector of keys from df
    keys <-
      keys %>%
      pull(name_col)
  }
  keys <- keys[!is.na(keys)]
  if (length(keys) == 0 | isTRUE(all(keys == ""))) {
    return(NULL)
  } else {
    assert_that(all(keys != "") == TRUE,
      msg = paste(
        "Invalid keys:",
        paste(rep("\"\"\"\"", length(keys[which(keys == "")])),
          collapse = ","
        ), "."
      )
    )
    assert_that(all(!grepl("\\D", keys)) == TRUE,
      msg = paste(
        "Invalid keys:",
        paste(keys[which(!grepl("\\D", keys) == FALSE)],
          collapse = ","
        ),
        "."
      )
    )
  }
  names(keys) <- as.character(keys)
  gbif_info <-
    keys %>%
    map(~ tryCatch(name_usage(., return = "data")[1, ],
      error = function(e) {
        print(paste("Key", ., "is an invalid GBIF taxon key."))
      }
    ))
  check_keys <-
    map_df(gbif_info, ~ is.character(.) == FALSE)
  check_keys <-
    check_keys %>%
    gather(key = "key", value = "is_taxonKey") %>%
    mutate(key = as.numeric(.data$key))
  valid_keys_df <-
    check_keys %>%
    filter(.data$is_taxonKey == TRUE)
  valid_keys <- gbif_info[which(names(gbif_info) %in% valid_keys_df$key)]
  if (length(valid_keys) > 0) {
    valid_keys_df <-
      valid_keys %>%
      reduce(bind_rows) %>%
      mutate(is_from_gbif_backbone = ifelse(.data$datasetKey == uuid_backbone,
        TRUE, FALSE
      ))
    check_keys <-
      check_keys %>%
      left_join(valid_keys_df %>%
        select(.data$key, .data$is_from_gbif_backbone),
      by = "key"
      )
    valid_keys_df <-
      valid_keys_df %>%
      filter(.data$is_from_gbif_backbone == TRUE) %>%
      mutate(is_synonym = ifelse(!.data$taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL"),
        TRUE, FALSE
      ))
    check_keys <-
      check_keys %>%
      left_join(valid_keys_df %>%
        select(.data$key, .data$is_synonym),
      by = "key"
      )
  } else {
    check_keys <-
      check_keys %>%
      mutate(
        is_from_gbif_backbone = NA,
        is_synonym = NA
      )
  }
  return(check_keys)
}

is.error <- function(x) inherits(x, "try-error")
uuid_backbone <- "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"
