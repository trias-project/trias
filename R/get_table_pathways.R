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
#' @param year_introduction character. Name of the column of \code{df}
#'   containing information about year of introduction. Default:
#'   \code{"first_observed"}.
#' @param species_names character. Name of the column of \code{df} containing
#'   information about species names. Default: \code{"canonicalName"}.
#'
#' @return a data.frame
#' @export
#' @importFrom dplyr %>% filter distinct mutate mutate_if group_by count ungroup
#'   rowwise sample_n pull select rename_at
#' @importFrom assertthat assert_that
#' @importFrom assertable assert_colnames
#' @importFrom stringr str_c
#' @importFrom purrr pmap_dfr
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' library(readr)
#' datafile <- paste0(
#'   "https://raw.githubusercontent.com/trias-project/indicators/master/data/",
#'   "interim/test_data_output_checklist_indicators.tsv"
#' )
#' data <- read_tsv(datafile, na = "", guess_max = 10000)
#' get_table_pathways(data)
#' # Specify kingdom
#' get_table_pathways(data, "Plantae")
#' # with special categories, `Chordata` or `not Chordata`
#' get_table_pathways(data, "Chordata")
#' get_table_pathways(data, "Not Chordata")
#' # From 2000
#' get_table_pathways(data, from = 2000, year_introduction = "first_observed")
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
                               year_introduction = "first_observed",
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
  assert_that(is.data.frame(df), msg = "df is not a data frame.")
  if (!is.null(category)) {
    assert_that(is.character(category),
      msg = paste0(
        "Category has to be a character. One of: ",
        paste(categories, collapse = ", "),
        "."
      )
    )
    assert_that(category %in% categories,
      msg = paste0(
        "Category not correct. Choose one of: ",
        paste(categories, collapse = ", "),
        "."
      )
    )
  }
  assert_colnames(df, kingdom_names, only_colnames = FALSE)
  assert_that(is.character(kingdom_names),
    msg = "Parameter 'kingdom_names' should be a character."
  )
  assert_that(is.numeric(n_species),
    msg = "Parameter 'n_species' should be a number."
  )
  assert_that(n_species > 0,
    msg = "Parameter 'n_species' should be a positive number."
  )
  assert_that(n_species == as.integer(n_species),
    msg = "Parameter 'n_species' should be an integer."
  )
  if (!is.null(from)) {
    assert_that(is.numeric(from),
      msg = "Parameter 'from' should be a number (year)."
    )
    assert_that(from > 0,
      msg = "Parameter 'from' should be a positive number."
    )
    assert_that(from == as.integer(from),
      msg = "Parameter 'from' should be an integer."
    )
    assert_that(from <= as.numeric(substr(Sys.Date(), start = 1, stop = 4)),
      msg = paste0(
        "Invalid year in 'from'. ",
        "Choose a year smaller than ",
        substr(Sys.Date(), start = 1, stop = 4)
      )
    )
    assert_that(is.character(year_introduction),
      msg = "Column 'year_of_introduction' should be a character."
    )
    assert_colnames(df, year_introduction, only_colnames = FALSE)
  }

  assert_colnames(df, species_names, only_colnames = FALSE)
  assert_that(is.character(species_names),
    msg = "Parameter 'species_names' should be a character."
  )
  # rename to default column name
  df <-
    df %>%
    rename_at(vars(kingdom_names), ~"group") %>%
    rename_at(vars(species_names), ~"taxa_names")
  if (!is.null(from)) {
    df <-
      df %>%
      rename_at(vars(year_introduction), ~"first_observed")
  }
  # handle asymmetric category system (Chordata, Not Chordta are not kingdoms)
  if (!is.null(category)) {
    if (!category %in% c("Chordata", "Not Chordata")) {
      filtered_data <- df %>% filter(.data$group == category)
    } else {
      # check parameter phylum
      assert_that(is.character(phylum_names),
        msg = "Parameter 'phylum_names' should be a character."
      )
      assert_colnames(df, phylum_names, only_colnames = FALSE)
      df <-
        df %>%
        rename_at(vars(phylum_names), ~"phylum_group")
      if (category == "Chordata") {
        filtered_data <- df %>% filter(.data$phylum_group == category)
      } else {
        filtered_data <- df %>%
          filter(.data$group == "Animalia") %>%
          filter(.data$phylum_group != "Chordata")
      }
    }
  } else {
    filtered_data <- df
  }
  # Apply cut-off on year of introduction if given
  if (!is.null(from)) {
    filtered_data <-
      filtered_data %>%
      filter(.data$first_observed >= from)
  }
  # Handle NAs, "unknown" and hierarchy (1st and 2nd level)
  preprocess_data <-
    filtered_data %>%
    # Handle NAs, "unknown" and hierarchy (1st and 2nd level)
    mutate(pathway_level1 = ifelse(!is.na(.data$pathway_level1),
      .data$pathway_level1,
      "unknown"
    )) %>%
    mutate(pathway_level2 = ifelse(.data$pathway_level1 != "unknown" &
      !is.na(.data$pathway_level2),
    .data$pathway_level2,
    ""
    ))
  # Create groups based on pathway level1 and level2
  preprocess_data <-
    preprocess_data %>%
    distinct(
      .data$taxa_names, .data$pathway_level1, .data$pathway_level2
    ) %>%
    group_by(.data$pathway_level1, .data$pathway_level2)

  # Assess size of sample per group
  pathway_data <-
    preprocess_data %>%
    count() %>%
    rowwise() %>%
    mutate(size_sample = ifelse(n > n_species,
      n_species, n
    ))
  # Make df with sample species
  samples <-
    pmap_dfr(
      list(
        pathway_data$pathway_level1,
        pathway_data$pathway_level2,
        pathway_data$size_sample
      ),
      function(p1, p2, s) {
        set_species <-
          preprocess_data %>%
          filter(.data$pathway_level1 == p1) %>%
          filter(.data$pathway_level2 == p2)
        if (s < nrow(preprocess_data)) {
          examples <- sample_n(set_species, s)
        } else {
          examples <- set_species
        }
        examples <-
          examples %>%
          pull(.data$taxa_names)

        tibble(examples = str_c(examples, collapse = ", ")) %>%
          mutate(
            pathway_level1 = as.character(p1),
            pathway_level2 = as.character(p2)
          )
      }
    )
  # No samples
  if (length(names(samples)) == 0) {
    samples <- tibble(
      examples = character(),
      pathway_level1 = character(),
      pathway_level2 = character()
    )
  }
  # Join pathways and samples together
  pathway_data <-
    pathway_data %>%
    # if pathway_data is empty
    mutate_if(is.logical, as.character) %>%
    left_join(samples,
      by = c("pathway_level1", "pathway_level2")
    ) %>%
    select(-.data$size_sample) %>%
    ungroup()

  pathway_data
}
