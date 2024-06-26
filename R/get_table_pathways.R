#' Pathway count indicator table
#'
#' Function to get number of taxa introduced by different pathways. Possible
#' breakpoints: taxonomic (kingdom + vertebrates/invertebrates), temporal (lower
#' limit year).
#' @param df df.
#' @param category NULL or character. One of the kingdoms as given in GBIF:
#'   \itemize{ \item{"Plantae"} \item{"Animalia"} \item{"Fungi"}
#'   \item{"Chromista"} \item{"Archaea"} \item{"Bacteria"} \item{"Protozoa"}
#'   \item{"Viruses"} \item{"incertae sedis"} } It can also be one of the
#'   following not kingdoms: #'\itemize{\item{Chordata} \item{Not Chordata}.
#'   Default: NULL.}
#' @param from NULL or numeric. Year trade-off: if not \code{NULL} select only
#'   pathways related to taxa introduced during or after this year. Default:
#'   \code{NULL}.
#' @param n_species numeric. The maximum number of species to return as examples
#'   per pathway. For groups with less species than \code{n_species}, all
#'   species are given. Default: 5.
#' @param kingdom_names character. Name of the column of \code{df} containing
#'   information about kingdom. Default: \code{"kingdom"}.
#' @param phylum_names character. Name of the column of \code{df} containing
#'   information about phylum. This parameter is used only if \code{category} is
#'   one of:  \code{"Chordata"}, \code{"Not Chordata"}.  Default: \code{"phylum"}.
#' @param first_observed character. Name of the column of \code{df}
#'   containing information about year of introduction. Default:
#'   \code{"first_observed"}.
#' @param species_names character. Name of the column of \code{df} containing
#'   information about species names. Default: \code{"canonicalName"}.
#'
#' @return a data.frame with 4 columns: `pathway_level1`, `pathway_level2`, `n`
#'   (number of taxa) and `examples`.
#' @export
#' @importFrom dplyr %>% .data
#' @examples
#' \dontrun{
#' library(readr)
#' datafile <- paste0(
#'   "https://raw.githubusercontent.com/trias-project/indicators/master/data/",
#'   "interim/data_input_checklist_indicators.tsv"
#' )
#' data <- read_tsv(datafile,
#'   na = "NA",
#'   col_types = cols(
#'     .default = col_character(),
#'     key = col_double(),
#'     nubKey = col_double(),
#'     speciesKey = col_double(),
#'     acceptedKey = col_double(),
#'     first_observed = col_double(),
#'     last_observed = col_double()
#'   )
#' )
#' get_table_pathways(data)
#' # Specify kingdom
#' get_table_pathways(data, "Plantae")
#' # with special categories, `Chordata` or `not Chordata`
#' get_table_pathways(data, "Chordata")
#' get_table_pathways(data, "Not Chordata")
#' # From 2000
#' get_table_pathways(data, from = 2000, first_observed = "first_observed")
#' # Specify number of species to include in examples
#' get_table_pathways(data, "Plantae", n_species = 8)
#' # Specify columns containing kingdom and species names
#' get_table_pathways(data,
#'   "Plantae",
#'   n_species = 8,
#'   kingdom_names = "kingdom",
#'   species_names = "canonicalName"
#' )
#' }
get_table_pathways <- function(df,
                               category = NULL,
                               from = NULL,
                               n_species = 5,
                               kingdom_names = "kingdom",
                               phylum_names = "phylum",
                               first_observed = "first_observed",
                               species_names = "canonicalName") {
  categories <- c(
    "Plantae",
    "Animalia",
    "Fungi",
    "Chromista",
    "Archaea",
    "Bacteria",
    "Protozoa",
    "Viruses",
    "incertae sedis",
    "Chordata",
    "Not Chordata"
  )
  # initial input checks
  assertthat::assert_that(is.data.frame(df), msg = "df is not a data frame.")
  if (!is.null(category)) {
    assertthat::assert_that(is.character(category),
      msg = paste0(
        "Category has to be a character. One of: ",
        paste(categories, collapse = ", "),
        "."
      )
    )
    assertthat::assert_that(category %in% categories,
      msg = paste0(
        "Category not correct. Choose one of: ",
        paste(categories, collapse = ", "),
        "."
      )
    )
  }
  assertable::assert_colnames(df, kingdom_names, only_colnames = FALSE)
  assertthat::assert_that(is.character(kingdom_names),
    msg = "Parameter 'kingdom_names' should be a character."
  )
  assertthat::assert_that(is.numeric(n_species),
    msg = "Parameter 'n_species' should be a number."
  )
  assertthat::assert_that(n_species > 0,
    msg = "Parameter 'n_species' should be a positive number."
  )
  assertthat::assert_that(n_species == as.integer(n_species),
    msg = "Parameter 'n_species' should be an integer."
  )
  if (!is.null(from)) {
    assertthat::assert_that(is.numeric(from),
      msg = "Parameter 'from' should be a number (year)."
    )
    assertthat::assert_that(from > 0,
      msg = "Parameter 'from' should be a positive number."
    )
    assertthat::assert_that(from == as.integer(from),
      msg = "Parameter 'from' should be an integer."
    )
    assertthat::assert_that(from <= as.numeric(substr(Sys.Date(), start = 1, stop = 4)),
      msg = paste0(
        "Invalid year in 'from'. ",
        "Choose a year smaller than ",
        substr(Sys.Date(), start = 1, stop = 4)
      )
    )
    assertthat::assert_that(is.character(first_observed),
      msg = "Column 'first_observed' should be a character."
    )
    assertable::assert_colnames(df, first_observed, only_colnames = FALSE)
  }

  assertable::assert_colnames(df, species_names, only_colnames = FALSE)
  assertthat::assert_that(is.character(species_names),
    msg = "Parameter 'species_names' should be a character."
  )

  # convert factors to characters (in case stringsAsFactors = TRUE)
  if (any(purrr::map_chr(names(df), ~ class(df[[.]])) == "factor")) {
    warning("Factors are converted to characters.")
    df <-
      df %>%
      dplyr::mutate_if(is.factor, as.character)
  }
  # rename to default column name
  df <-
    df %>%
    dplyr::rename(group = !!kingdom_names) %>%
    dplyr::rename(taxa_names = !!species_names)
  if (!is.null(from)) {
    df <-
      df %>%
      dplyr::rename(first_observed = !!first_observed)
  }
  # handle asymmetric category system (Chordata, Not Chordta are not kingdoms)
  if (!is.null(category)) {
    if (!category %in% c("Chordata", "Not Chordata")) {
      filtered_data <- df %>% dplyr::filter(.data$group == category)
    } else {
      # check parameter phylum
      assertthat::assert_that(is.character(phylum_names),
        msg = "Parameter 'phylum_names' should be a character."
      )
      assert_colnames(df, phylum_names, only_colnames = FALSE)
      df <-
        df %>%
        dplyr::rename(phylum_group = !!phylum_names)
      if (category == "Chordata") {
        filtered_data <- df %>% dplyr::filter(.data$phylum_group == category)
      } else {
        filtered_data <- df %>%
          dplyr::filter(.data$group == "Animalia") %>%
          dplyr::filter(.data$phylum_group != "Chordata")
      }
    }
  } else {
    filtered_data <- df
  }
  # Apply cut-off on year of introduction if given
  if (!is.null(from)) {
    filtered_data <-
      filtered_data %>%
      dplyr::filter(.data$first_observed >= from)
  }
  # Handle NAs, "unknown" and hierarchy (1st and 2nd level)
  preprocess_data <-
    filtered_data %>%
    # Handle NAs, "unknown" and hierarchy (1st and 2nd level)
    dplyr::mutate(pathway_level1 = ifelse(is.na(.data$pathway_level1) |
      .data$pathway_level1 == "",
    "unknown",
    .data$pathway_level1
    )) %>%
    dplyr::mutate(pathway_level2 = ifelse(.data$pathway_level1 != "unknown" &
      !is.na(.data$pathway_level2) & .data$pathway_level2 != "",
    .data$pathway_level2,
    "unknown"
    ))
  # Create groups based on pathway level1 and level2
  preprocess_data <-
    preprocess_data %>%
    dplyr::distinct(
      .data$taxa_names, .data$pathway_level1, .data$pathway_level2
    ) %>%
    dplyr::group_by(.data$pathway_level1, .data$pathway_level2)

  # Assess size of sample per group
  pathway_data <-
    preprocess_data %>%
    dplyr::count() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(size_sample = ifelse(n > n_species,
      n_species, n
    ))
  # Make df with sample species
  samples <-
    purrr::pmap_dfr(
      list(
        pathway_data$pathway_level1,
        pathway_data$pathway_level2,
        pathway_data$size_sample
      ),
      function(p1, p2, s) {
        set_species <-
          preprocess_data %>%
          dplyr::filter(.data$pathway_level1 == p1) %>%
          dplyr::filter(.data$pathway_level2 == p2)
        if (s < nrow(preprocess_data)) {
          examples <- dplyr::sample_n(set_species, s)
        } else {
          examples <- set_species
        }
        examples <-
          examples %>%
          dplyr::pull(.data$taxa_names)

        dplyr::tibble(examples = stringr::str_c(examples, collapse = ", ")) %>%
          dplyr::mutate(
            pathway_level1 = as.character(p1),
            pathway_level2 = as.character(p2)
          )
      }
    )
  # No samples
  if (length(names(samples)) == 0) {
    samples <- dplyr::tibble(
      examples = character(),
      pathway_level1 = character(),
      pathway_level2 = character()
    )
  }
  # Join pathways and samples together
  if (nrow(pathway_data) == 0) {
    dplyr::tibble(
      pathway_level1 = character(0),
      pathway_level2 = character(0),
      n = integer(0),
      examples = character(0)
    )
  } else {
    pathway_data %>%
      dplyr::left_join(samples,
        by = c("pathway_level1", "pathway_level2")
      ) %>%
      dplyr::select(-"size_sample") %>%
      dplyr::ungroup()
  }
}
