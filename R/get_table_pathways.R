#' Pathway count indicator figure
#'
#' Function to get number of taxa introduced by different pathways. Possible
#' breakpoint: kingdom.
#' @param df df.
#' @param category NULL or character. One of the kingdoms as given in GBIF: \itemize{
#'   \item{"Plantae"} \item{"Animalia"} \item{"Fungi"} \item{"Chromista"}
#'   \item{"Archaea"} \item{"Bacteria"} \item{"Protozoa"} \item{"Viruses"}
#'   \item{"incertae sedis"} } It can also be one of the following not kingdoms:
#'   #'\itemize{\item{Chordata} \item{Not Chordata}. Default: NULL.}
#' @param n_species integer The maximum number of species to return as examples
#'   per pathway. For groups with less species than \code{n_species}, all
#'   species are given. Default: 5.
#' @param kingdom character. Name of the column of \code{df} containing
#'   information about kingdom. Default: \code{"kingdom"}.
#'
#' @return a data.frame
#' @export
#' @importFrom dplyr %>% filter distinct mutate group_by count ungroup rowwise
#'   sample_n pull select rename_at
#' @importFrom assertthat assert_that
#' @importFrom assertable assert_colnames
#' @importFrom stringr str_c
#' @importFrom purrr pmap_dfr
#' @importFrom tibble as_tibble
#'
#' @examples
#' library(readr)
#' datafile <- paste0(
#'   "https://raw.githubusercontent.com/trias-project/indicators/master/data/",
#'   "output/data_input_checklist_indicators.tsv"
#' )
#' data <- read_tsv(datafile)
#' get_table_pathways(data)
#' # Specify kingdom
#' get_table_pathways(data, "Plantae")
#' # with no kingdom
#' get_table_pathways(data, "Chordata")
#' get_table_pathways(data, "Not Chordata")
#' # Specify number of species to include in examples
#' get_table_pathways(data, "Plantae", n_species = 8)
get_table_pathways <- function(df,
                               category = NULL,
                               n_species = 5,
                               kingdom = "kingdom") {
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
  assert_that(is.data.frame(df))
  if (!is.null(category)) {
    assert_that(is.character(category))
    assert_that(category %in% categories)
  }
  assert_colnames(df, kingdom, only_colnames = FALSE)
  # rename to default column name
  df <-
    df %>%
    rename_at(vars(kingdom), ~"kingdom")

  # handle asymmetric category system (Chordata, Not Chordta are not kingdoms)
  if (!is.null(category)) {
    if (!category %in% c("Chordata", "Not Chordata")) {
      filtered_data <- df %>% filter(kingdom == category)
    } else {
      if (category == "Chordata") {
        filtered_data <- df %>% filter(phylum == category)
      } else {
        filtered_data <- df %>%
          filter(kingdom == "Animalia") %>%
          filter(phylum != category)
      }
    }
  } else {
    filtered_data <- df
  }



  # Handle NAs, "unknown" and hierarchy (1st and 2nd level)
  preprocess_data <-
    filtered_data %>%
    # Handle NAs, "unknown" and hierarchy (1st and 2nd level)
    mutate(pathway_level1 = ifelse(!is.na(pathway_level1),
      pathway_level1,
      "unknown"
    )) %>%
    mutate(pathway_level2 = ifelse(pathway_level1 != "unknown" &
      !is.na(pathway_level2),
    pathway_level2,
    ""
    ))
  # Create groups based on pathway level1 and level2
  preprocess_data <-
    preprocess_data %>%
    distinct(
      scientificName, pathway_level1, pathway_level2
    ) %>%
    group_by(pathway_level1, pathway_level2)

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
          filter(pathway_level1 == p1) %>%
          filter(pathway_level2 == p2)
        if (s < nrow(preprocess_data)) {
          examples <- sample_n(set_species, s)
        } else {
          examples <- set_species
        }
        examples <-
          examples %>%
          pull(scientificName)

        tibble(examples = str_c(examples, collapse = ", ")) %>%
          mutate(
            pathway_level1 = p1,
            pathway_level2 = p2
          )
      }
    )

  # Join pathways and samples together
  pathway_data <-
    pathway_data %>%
    left_join(samples,
      by = c("pathway_level1", "pathway_level2")
    ) %>%
    select(-size_sample)

  # Create output table (untidy)
  pathway_data <-
    pathway_data %>%
    # mutate(examples = ifelse(!is.na(examples),
    #                          examples,
    #                          str_c(sample_n(preprocess_data %>%
    #                                           filter(is.na(pathway_level1) |
    #                                                    is.na(pathway_level1)),
    #                                         n)$species,
    #                                collapse = ", ")
    #                          )) %>%
    ungroup()

  # pathway_data$pathway_level1[
  #   duplicated(pathway_data$pathway_level1) == TRUE
  # ] <- ""

  pathway_data
}
