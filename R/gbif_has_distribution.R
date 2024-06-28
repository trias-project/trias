#' Compare desired distribution information with actual one.
#'
#' This function compares GBIF distribution information based on a single taxon
#' key with user requests and returns a logical (TRUE or FALSE). Comparison is
#' case insensitive. User properties for each term are treated as OR.
#' It is a function built on rgbif function `name_usage()`.
#' @param taxon_key (single numeric or character) a single taxon key.
#' @param ... one or more GBIF distribution properties and related values.
#' Up to now it supports the following properties:
#' country (and its synonym: countryCode), status (and its synonym:
#' occurrenceStatus) and establishmentMeans.
#' @return a logical, TRUE or FALSE.
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' # IMPORTANT! 
#' # examples could fail as long as `status` (`occurrenceStatus`) is used due to
#' # an issue of the GBIF API: see https://github.com/gbif/gbif-api/issues/94
#' 
#' # numeric taxonKey, atomic parameters
#' gbif_has_distribution(145953242,
#'   country = "BE",
#'   status = "PRESENT",
#'   establishmentMeans = "INTRODUCED"
#' )
#'
#' # character taxonKey, distribution properties as vectors, treated as OR
#' gbif_has_distribution("145953242",
#'   country = c("NL", "BE"),
#'   status = c("PRESENT", "DOUBTFUL")
#' )
#'
#' # use alternative names: countryCode, occurrenceStatus.
#' # Function works. Warning is given.
#' gbif_has_distribution("145953242",
#'   countryCode = c("NL", "BE"),
#'   occurrenceStatus = c("PRESENT", "DOUBTFUL")
#' )
#'
#' # Case insensitive
#' gbif_has_distribution("145953242",
#'   country = "be",
#'   status = "PRESENT",
#'   establishmentMeans = "InTrOdUcEd"
#' )
#' }
gbif_has_distribution <- function(taxon_key, ...) {
  # df with all possible combinations of user's distribution properties values
  user_properties <- list(...)

  assertthat::assert_that(all(length(names(user_properties)) ==
    length(unique(names(user_properties)))),
  msg = paste(
    "Duplicates in property assignments.",
    "Use vectors for multiple assignments."
  )
  )

  # remove duplicates (country <-> countryStatus status <-> occurrenceStatus)
  if (all(c("country", "countryCode") %in% names(user_properties))) {
    warning(paste(
      "country and countryCode are alternative names of the",
      "same property. countryCode removed."
    ))
    user_properties$countryCode <- NULL
  }
  if (all(c("status", "occurrenceStatus") %in% names(user_properties))) {
    warning(paste(
      "status and occurrenceStatus are alternative names of the",
      "same property. occurrenceStatus removed."
    ))
    user_properties$occurrenceStatus <- NULL
  }

  # accept but rename the alternative name countryCode and occurrenceStatus
  if ("countryCode" %in% names(user_properties)) {
    warning("countryCode renamed to country")
    names(user_properties) <- replace(
      names(user_properties),
      names(user_properties) == "countryCode",
      "country"
    )
  }

  if ("occurrenceStatus" %in% names(user_properties)) {
    warning("occurrenceStatus renamed to status")
    names(user_properties) <- replace(
      names(user_properties),
      names(user_properties) == "occurrenceStatus",
      "status"
    )
  }

  assertthat::assert_that(
    all(names(user_properties) %in% GBIF_distr_terms),
    msg = "Invalid distribution properties."
  )

  # retrieve distribution properties from GBIF
  distr_properties <- rgbif::name_usage(
    key = as.integer(taxon_key),
    data = "distribution"
  )$data

  # no ditribution properties values specified by user
  if (is.null(names(user_properties))) {
    has_distr <- nrow(distr_properties) > 0
    return(has_distr)
  }
  else {
    # taxa has no distribution
    if (nrow(distr_properties) == 0) {
      return(FALSE)
    } else {
      # taxa has less distribution properties than specified by user
      if (any(!names(user_properties) %in% colnames(distr_properties))) {
        return(FALSE)
      } else {
        # Avoid mismatch due to any upper/lowercase difference
        user_properties <- purrr::map(user_properties, ~ toupper(.))
        # Check whether at least one property defined by user is present
        has_distr <- dplyr::intersect(
          user_properties %>%
            purrr::cross_df(),
          distr_properties %>%
            dplyr::select(names(user_properties)) %>%
            dplyr::distinct() %>%
            dplyr::mutate(dplyr::across(.cols = dplyr::everything(), toupper))
        ) %>%
          nrow() > 0
        return(has_distr)
      }
    }
  }
}

#' Accepted GBIF distribution terms
#' @keywords internal
GBIF_distr_terms <- c(
  "country",
  "status", "establishmentMeans"
)
