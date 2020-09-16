#' Plot number of introduced taxa for CBD pathways level 2
#'
#' Function to plot bar graph with number of taxa introduced by different
#' pathways at level 2, given a pathway level 1. Possible breakpoints: taxonomic
#' (kingdoms + vertebrates/invertebrates) and temporal (lower limit year).
#'
#' @param df df.
#' @param chosen_pathway_level1 character. A pathway level 1. If CBD standard is
#'   followed (see argument `cbd_standard`), one of the level 1 pathways from
#'   `pathways_cbd()`.
#' @param category NULL or character. One of the kingdoms as given in GBIF and
#'   `Chordata` (the phylum), `Not Chordata` (all other phyla of `Animalia`): 1.
#'   `Plantae` 2. `Animalia` 3. `Fungi` 4. `Chromista` 5. `Archaea` 6.
#'   `Bacteria` 7. `Protozoa` 8. `Viruses` 9. `incertae sedis` 10. `Chordata`
#'   11. `Not Chordata` Default: `NULL`.
#' @param from NULL or numeric. Year trade-off: if not \code{NULL} select only
#'   pathways related to taxa introduced during or after this year. Default:
#'   `NULL`.
#' @param facet_column NULL or character. The column to use to create additional
#'   facet wrap bar graphs underneath the main graph. When NULL, no facet graph
#'   are created. One of `family`, `order`, `class`, `phylum`, `locality`,
#'   `native_range`, `habitat`. If column has another name, rename it before
#'   calling this function. Default: `NULL`.
#' @param pathway_level1_names character. Name of the column of \code{df}
#'   containing information about pathways at level 1. Default:
#'   `pathway_level1`.
#' @param pathway_level2_names character. Name of the column of \code{df}
#'   containing information about pathways at level 2. Default:
#'   `pathway_level2`.
#' @param pathways character. Vector with pathways level 2 to visualize. The
#'   pathways are displayed following the order as in this vector.
#' @param taxon_names character. Name of the column of \code{df} containing
#'   information about taxa. This parameter is used to uniquely identify taxa.
#' @param kingdom_names character. Name of the column of \code{df} containing
#'   information about kingdom. Default: \code{"kingdom"}.
#' @param phylum_names character. Name of the column of \code{df} containing
#'   information about phylum. This parameter is used only if \code{category} is
#'   one of:  \code{"Chordata"}, \code{"Not Chordata"}.  Default:
#'   \code{"phylum"}.
#' @param first_observed character. Name of the column of \code{df} containing
#'   information about year of introduction. Default: \code{"first_observed"}.
#' @param cbd_standard logical. If TRUE the values of pathway level 2 are
#'   checked based on CBD standard as returned by `pathways_cbd()`. Error is
#'   returned if unmatched values are found. If FALSE, a warning is returned.
#'   Default: TRUE.
#' @param title NULL or character. Title of the graph. Default: NULL.
#' @param x_lab NULL or character. x-axis label. Default: "Number of introduced
#'   taxa".
#' @param y_lab NULL or character. Title of the graph. Default: "Pathways".
#' @return A ggplot2 object (or egg object if facets are used). NULL if there are
#'  no data to plot.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertable assert_colnames
#' @importFrom dplyr %>% anti_join distinct filter if_else mutate pull rename_at
#'   sym
#' @importFrom ggplot2 ggplot geom_bar theme ggtitle xlab ylab coord_flip
#'   facet_wrap
#' @importFrom tidyselect all_of
#' @importFrom forcats fct_rev
#' @importFrom rlang !!
#' @importFrom egg ggarrange
#'
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
#' # All taxa
#' visualize_pathways_level2(t, chosen_pathway_level1 = "escape")
#'
#' # Animalia
#' visualize_pathways_level2(t,
#'   chosen_pathway_level1 = "escape",
#'   category = "Animalia"
#' )
#'
#' # Chordata
#' visualize_pathways_level2(
#'   df = t,
#'   chosen_pathway_level1 = "escape",
#'   category = "Chordata"
#' )
#'
#' # facet phylum
#' visualize_pathways_level2(
#'   df = t,
#'   chosen_pathway_level1 = "escape",
#'   category = "Animalia",
#'   facet_column = "phylum"
#' )
#'
#' # facet habitat
#' visualize_pathways_level2(
#'   df = t,
#'   chosen_pathway_level1 = "escape",
#'   facet_column = "habitat"
#' )
#'
#' # Only taxa introduced from 1950
#' visualize_pathways_level2(
#'   df = t,
#'   chosen_pathway_level1 = "escape",
#'   from = 1950
#' )
#'
#' # Add a title
#' visualize_pathways_level2(
#'   df = t,
#'   chosen_pathway_level1 = "escape",
#'   category = "Plantae",
#'   from = 1950,
#'   title = "Pathway level 2 (escape): Plantae, from 1950"
#' )
#'
#' # Personalize axis labels
#' visualize_pathways_level2(
#'   df = t,
#'   chosen_pathway_level1 = "escape",
#'   x_lab = "Aantal taxa",
#'   y_lab = "pathways"
#' )
#' }
visualize_pathways_level2 <- function(df,
                                      chosen_pathway_level1,
                                      category = NULL,
                                      from = NULL,
                                      facet_column = NULL,
                                      pathways = NULL,
                                      pathway_level1_names = "pathway_level1",
                                      pathway_level2_names = "pathway_level2",
                                      taxon_names = "key",
                                      kingdom_names = "kingdom",
                                      phylum_names = "phylum",
                                      first_observed = "first_observed",
                                      cbd_standard = TRUE,
                                      title = NULL,
                                      x_lab = "Number of introduced taxa",
                                      y_lab = "Pathways") {

  # initial input checks
  # Check df
  assert_that(is.data.frame(df), msg = "`df` must be a data frame.")
  # Check pathway_level1_names
  assert_that(is.character(pathway_level1_names),
    msg = "`pathway_level1_names` must be a character."
  )
  assert_colnames(df, pathway_level1_names, only_colnames = FALSE)
  # Check pathway_level2_names
  assert_that(is.character(pathway_level2_names),
    msg = "`pathway_level2_names` must be a character."
  )
  assert_colnames(df, pathway_level2_names, only_colnames = FALSE)
  # Check category
  if (!is.null(category)) {
    assert_that(is.character(category),
      msg = paste0(
        "`category` must be a character. One of: ",
        paste(categories, collapse = ", "),
        "."
      )
    )
    assert_that(category %in% categories,
      msg = paste0(
        "`category` is not correct. Choose one of: ",
        paste0(categories, collapse = ", "),
        "."
      )
    )
  }
  # Check facet_column
  assert_that(is.null(facet_column) | is.character(facet_column),
    msg = "Argument facet_column has to be NULL or a character."
  )
  if (is.character(facet_column)) {
    assert_colnames(df, facet_column, only_colnames = FALSE)
  }
  # check for valid facet options
  valid_facet_options <- c(
    "family", "order", "class", "phylum",
    "locality", "native_range", "habitat"
  )
  if (is.character(facet_column)) {
    facet_column <- match.arg(facet_column, valid_facet_options)
    assert_that(is.null(category) || !(category == "Chordata" &
      facet_column == "phylum"),
    msg = "You cannot use phylum as facet with category Chordata."
    )
  }
  # Check chosen_pathway_level1
  assert_that(is.character(chosen_pathway_level1),
    msg = "Argument `chosen_pathway_level1` must be a character."
  )
  pathways_level1 <- unique(df[[pathway_level1_names]])
  assert_that(chosen_pathway_level1 %in% pathways_level1,
    msg = paste0(
      "chosen_pathway_level1 ",
      chosen_pathway_level1,
      " not present in column ",
      pathway_level1_names,
      "."
    )
  )
  # Check pathways
  if (!is.null(pathways)) {
    assert_that(is.character(pathways),
      msg = "`pathways` must be a vector of characters."
    )
    invalid_pathways <- pathways[!pathways %in%
      df[[pathway_level2_names]]]
    assert_that(length(invalid_pathways) == 0,
      msg = paste0(
        "Pathways in `pathways` not present in ",
        "data.frame: ",
        paste(invalid_pathways, collapse = ","),
        "."
      )
    )
  }
  # Check taxon_names
  assert_that(is.character(taxon_names),
    msg = "`taxon_names` must be a character."
  )
  assert_colnames(df, taxon_names, only_colnames = FALSE)
  # Check kingdom_names
  assert_that(is.character(kingdom_names),
    msg = "`kingdom_names` must be a character."
  )
  assert_colnames(df, kingdom_names, only_colnames = FALSE)
  # check parameter phylum
  assert_that(is.character(phylum_names),
    msg = "`phylum_names` must be a character."
  )
  assert_colnames(df, phylum_names, only_colnames = FALSE)
  # Check from
  if (!is.null(from)) {
    assert_that(is.numeric(from),
      msg = "`from` must be a number (year)."
    )
    assert_that(from > 0,
      msg = "`from` must be a positive number."
    )
    assert_that(from == as.integer(from),
      msg = "`from` must be an integer."
    )
    assert_that(from <= as.numeric(substr(Sys.Date(), start = 1, stop = 4)),
      msg = paste0(
        "`from` must be less than ",
        format(Sys.Date(), "%Y"),
        "."
      )
    )
  }
  # Check first_observed
  assert_that(is.character(first_observed),
    msg = "`first_observed` must be a character."
  )
  assert_colnames(df, first_observed, only_colnames = FALSE)
  # Check title and labels
  if (!is.null(title)) {
    assert_that(is.character(title),
      msg = "`title` must be a character or NULL."
    )
  }
  if (!is.null(x_lab)) {
    assert_that(is.character(x_lab),
      msg = "`x_lab` must be a character or NULL."
    )
  }
  if (!is.null(y_lab)) {
    assert_that(is.character(y_lab),
      msg = "`y_lab` must be a character or NULL."
    )
  }
  # rename to default column name
  df <-
    df %>%
    rename_at(vars(all_of(kingdom_names)), ~"group") %>%
    rename_at(vars(all_of(taxon_names)), ~"taxonKey") %>%
    rename_at(vars(all_of(first_observed)), ~"first_observed") %>%
    rename_at(vars(all_of(pathway_level1_names)), ~"pathway_level1") %>%
    rename_at(vars(all_of(pathway_level2_names)), ~"pathway_level2")
  # Select data with the chosen pathway level 1
  df <-
    df %>%
    filter(.data$pathway_level1 == chosen_pathway_level1)
  # handle asymmetric category system (Chordata, Not Chordta are not kingdoms)
  if (!is.null(category)) {
    if (!category %in% c("Chordata", "Not Chordata")) {
      df <- df %>% filter(.data$group == category)
    } else {
      df <-
        df %>%
        rename_at(vars(phylum_names), ~"phylum_group")
      if (category == "Chordata") {
        df <- df %>% filter(.data$phylum_group == category)
      } else {
        df <-
          df %>%
          filter(.data$group == "Animalia") %>%
          filter(.data$phylum_group != "Chordata")
      }
    }
  }
  # Apply cut-off on year of introduction if given
  if (!is.null(from)) {
    df <-
      df %>%
      filter(.data$first_observed >= from)
  }
  # Handle NAs and ""
  nas_or_empty_pathway_level2 <-
    df %>%
    filter(is.na(.data$pathway_level2) |
      .data$pathway_level2 == "") %>%
    distinct(.data$taxonKey)
  if (nrow(nas_or_empty_pathway_level2) > 0) {
    message_warning <- paste(
      nrow(nas_or_empty_pathway_level2),
      "taxa have no information about pathway level 2.",
      "Set to 'unknown'."
    )
    warning(message_warning)
  }
  df <-
    df %>%
    # Handle NAs and "unknown"
    mutate(pathway_level2 = if_else(is.na(.data$pathway_level2) |
      .data$pathway_level2 == "",
    "unknown",
    .data$pathway_level2
    ))
  # Import all CBD pathways level 2 within chosen pathway level 1
  pathways_level2_all <-
    pathways_cbd() %>%
    filter(.data$pathway_level1 == chosen_pathway_level1) %>%
    distinct(.data$pathway_level2)
  # Select pathways
  if (!is.null(pathways)) {
    pathways <- replace(pathways, is.na(pathways) | pathways == "", "unknown")
    pathways <- unique(pathways)
    df <-
      df %>%
      filter(.data$pathway_level2 %in% pathways)
  } else {
    if (cbd_standard == TRUE) {
      pathways <- pathways_level2_all %>% pull()
    } else {
      pathways <- unique(df$pathway_level2)
    }
  }
  # Check values in column with pathways level 2
  invalid_pathways <-
    df %>%
    anti_join(pathways_level2_all,
      by = "pathway_level2"
    ) %>%
    distinct(.data$pathway_level2) %>%
    pull()
  message_invalid_pathways <-
    paste0(
      "No CBD standard pathways level 2 value(s) in column `",
      pathway_level2_names,
      "`: ",
      paste0(invalid_pathways, collapse = ", "),
      ". Valid pathways values: ",
      paste0(unique(pathways_level2_all$pathway_level2), collapse = ", "),
      "."
    )
  if (cbd_standard == TRUE) {
    assert_that(length(invalid_pathways) == 0,
      msg = message_invalid_pathways
    )
  } else {
    warning(message_invalid_pathways)
  }
  # Distinct taxa
  if (!is.null(facet_column)) {
    df <-
      df %>%
      distinct(.data$taxonKey, .data$pathway_level2, !!sym(facet_column))
  }
  # Transform pathway level 2 column to factor to make ordering in graph easily
  df <-
    df %>%
    mutate(pathway_level2 = factor(.data$pathway_level2, levels = pathways))
  # Distinct taxa without facet
  df_top_graph <-
    df %>%
    distinct(
      .data$taxonKey,
      .data$pathway_level2
    )
  # Plot number of taxa per pathway_level1
  top_graph <- NULL
  if (nrow(df_top_graph) > 0) {
    top_graph <-
      ggplot(
        df_top_graph,
        aes(x = fct_rev(.data$pathway_level2))
      ) +
      geom_bar() +
      xlab(y_lab) +
      ylab(x_lab) +
      coord_flip() +
      ggtitle(title)
  }
  if (is.null(facet_column)) {
    return(top_graph)
  } else {
    facet_graph <- NULL
    if (nrow(df) > 0) {
      facet_graph <-
        ggplot(
          df,
          aes(x = fct_rev(.data$pathway_level2))
        ) +
        geom_bar() +
        xlab(y_lab) + # invert labels to get them right after flipping
        ylab(x_lab) +
        coord_flip() +
        ggtitle(title) +
        facet_wrap(facet_column)
    }
    if (all(!is.null(top_graph), !is.null(facet_graph))) {
      ggarrange(top_graph, facet_graph)
    }
    else {
      NULL
    }
  }
}
