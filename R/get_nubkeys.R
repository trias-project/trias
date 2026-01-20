#' Retrieve unique GBIF Backbone taxon keys from a species checklist
#' 
#' This function is a wrapper around `rgbif::name_usage()` to retrieve the (unique) GBIF
#' Backbone taxon keys (nubKeys) from a species checklist identified by its
#' datasetKey. It allows to choose whether to return synonyms or accepted taxa
#' only. When `allow_synonyms`is FALSE, the function makes individual API calls for each nubKey in a loop. For checklists with many taxa, this could result in a large number of sequential API requests and it can take a long time to complete.
#' 
#' Synonym relationships within the checklist itself are not taken into account.
#' @param datasetKey (character) Unique identifier of a GBIF species checklist.
#' @param allow_synonyms (logical) Default: TRUE. If `FALSE`, the accepted taxa
#'   the synonyms refers to are returned instead of the synonyms themselves.
#' @return A (unique) vector of GBIF Backbone taxon keys (nubKeys). If
#'   `allow_synonyms` is `TRUE`, the keys are retrieved from
#'   `rgbif::name_usage()$data` directly. If `allow_synonyms` is `FALSE`, the
#'   accepted taxa keys are retrieved by calling `rgbif::name_usage()` for each
#'   synonym key obtained from
#'   `rgbif::name_usage()$data` and extracting the accepted taxa keys from 
#'   them. The final keys are returned as unique values.
#' @family checklist functions
#' @export
#' @importFrom dplyr %>% .data
#' @examples
#' dataset_key <- "e082b10e-476f-43c1-aa61-f8d92f33029a"
#' # Retrieve all GBIF Backbone taxon keys: synonyms are included
#' get_nubkeys(datasetKey = dataset_key)
#' # Retrieve accepted taxa GBIF Backbone taxon keys
#' get_nubkeys(datasetKey = dataset_key, allow_synonyms = FALSE)
get_nubkeys <- function(datasetKey, allow_synonyms = TRUE) {
  # Return error if `datasetKey` is NULL
  assertthat::assert_that(
    !is.null(datasetKey),
    msg = "`datasetKey` should be a single (scalar) non-NA character value."
  )
  # Return error if `datasetKey` is not a scalar character, no NA allowed
  assertthat::assert_that(
    all(purrr::is_scalar_character(datasetKey) & !is.na(datasetKey)),
    msg = "`datasetKey` should be a single (scalar) non-NA character value."
  )
  # Return error if `datasetKey` contains spaces
  assertthat::assert_that(
    stringr::str_detect(datasetKey, "\\s") == FALSE,
    msg = "`datasetKey` should not contain spaces."
  )
  # Return error if `allow_synonyms` is NULL
  assertthat::assert_that(
    !is.null(allow_synonyms),
    msg = "`allow_synonyms` should be a single (scalar) non-NA logical value."
  )
  # Return error if `allow_synonyms` is not a scalar logical, no NA allowed
  assertthat::assert_that(
    all(purrr::is_scalar_logical(allow_synonyms) & !is.na(allow_synonyms)),
    msg = "`allow_synonyms` should be a single (scalar) non-NA logical value."
  )
  
  # Retrieve nubKeys from the checklist
  df <- rgbif::name_usage(datasetKey = datasetKey, limit = 99999)$data %>%
    dplyr::filter(.data$origin == "SOURCE")
  # Throw a message if some taxa are not matched to GBIF Backbone (nubKey is NA)
  # and remove them from the list of nub_keys
  if (any(is.na(df$nubKey))) {
    warning(paste(
      sum(is.na(df$nubKey)),
      "taxa from the checklist with datasetKey",
      datasetKey,
      "are not matched to GBIF Backbone and are ignored."
    ))
    df <- df %>%
      dplyr::filter(!is.na(.data$nubKey))
  }
  nub_keys <- df %>%
    dplyr::pull(.data$nubKey) %>%
    unique()
  if (allow_synonyms) {
    return(nub_keys)
  } else {
    # Safely retrieve name usage data for each nubKey so that a failure for a
    # single key does not abort the entire operation.
    safe_name_usage <- purrr::possibly(rgbif::name_usage, otherwise = NULL)

    usage_data <- nub_keys %>%
      purrr::map(function(x) {
        res <- safe_name_usage(x)
        if (is.null(res) || is.null(res$data)) {
          warning(paste("Failed to retrieve taxonomic information for nubKey", x, "- skipping."))
          return(NULL)
        }
        res$data
      }) %>%
      purrr::compact()

    if (length(usage_data) == 0) {
      # If all lookups failed, return an empty integer vector instead of erroring.
      return(integer(0))
    }

    usage_data %>%
      purrr::list_rbind() %>%
      # Choose the accepted taxa instead of synonyms
      dplyr::mutate(
        accepted_taxa = dplyr::coalesce(.data$acceptedKey, .data$key)
      ) %>%
      dplyr::pull(.data$accepted_taxa) %>%
      unique()
  }
}
