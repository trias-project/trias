#' Plot number of introduced taxa for CBD pathways level 1
#'
#' Function to plot bar graph with number of taxa introduced by different
#' pathways at level 1. Possible breakpoints: taxonomic (kingdoms +
#' vertebrates/invertebrates) and temporal (lower limit year). Facets can be
#' added (see argument `facet_column`).
#'
#' @param df A data frame.
#' @param category `NULL` or character. One of the kingdoms as given in GBIF and
#'   `Chordata` (the phylum), `Not Chordata` (all other phyla of `Animalia`): 1.
#'   `Plantae` 2. `Animalia` 3. `Fungi` 4. `Chromista` 5. `Archaea` 6.
#'   `Bacteria` 7. `Protozoa` 8. `Viruses` 9. `incertae sedis` 10. `Chordata`
#'   11. `Not Chordata` Default: `NULL`.
#' @param from `NULL` or numeric. Year trade-off: select only pathways related
#'   to taxa introduced during or after this year. Default: `NULL`.
#' @param facet_column `NULL` (default) or character. The column to use to
#'   create additional facet wrap bar graphs underneath the main graph. When
#'   `NULL`, no facet graph are created. One of `family`, `order`, `class`,
#'   `phylum`, `locality`, `native_range`, `habitat`. If column has another
#'   name, rename it before calling this function. Facet `phylum` is not allowed
#'   in combination with `category` equal to `"Chordata"` or `"Not Chordata"`.
#'   Facet `kingdom` is allowed only with category equal to `NULL`.
#' @param pathway_level1_names character. Name of the column of `df`
#'   containing information about pathways at level 1. Default:
#'   `pathway_level1`.
#' @param pathways character. Vector with pathways level 1 to visualize. The
#'   pathways are displayed following the order as in this vector. It may
#'   contain pathways not present in the column given by `pathway_level1_names`.
#' @param taxon_names character. Name of the column of `df` containing
#'   information about taxa. This parameter is used to uniquely identify taxa.
#' @param kingdom_names character. Name of the column of `df` containing
#'   information about kingdom. Default: `"kingdom"`.
#' @param phylum_names character. Name of the column of `df` containing
#'   information about phylum. This parameter is used only if `category` is
#'   one of: `"Chordata"`, `"Not Chordata"`.  Default:
#'   `"phylum"`.
#' @param first_observed character. Name of the column of `df` containing
#'   information about year of introduction. Default: `"first_observed"`.
#' @param cbd_standard logical. If TRUE the values of pathway level 1 are
#'   checked based on CBD standard as returned by `pathways_cbd()`. Error is
#'   returned if unmatched values are found. If FALSE, a warning is returned.
#'   Default: TRUE.
#' @param title `NULL` or character. Title of the graph. Default: `NULL`.
#' @param x_lab `NULL` or character. x-axis label. Default: `"Number of
#'   introduced taxa"`.
#' @param y_lab `NULL` or character. Title of the graph. Default: `"Pathways"`.
#' @return A list with three slots:
#' - `plot`: ggplot2 object (or egg object if facets are used). `NULL` if there
#' are no data to plot.
#' - `data_top_graph`: data.frame (tibble) with data used for the main plot (top
#' graph) in `plot`.
#' - `data_facet_graph`: data.frame (tibble) with data used for the faceting
#' plot in `plot`. `NULL` is returned if `facet_column` is `NULL`.
#'
#' @export
#' @importFrom dplyr %>% .data
#' @importFrom rlang !!
#'
#' @examples
#' \dontrun{
#' library(readr)
#' datafile <- paste0(
#'   "https://raw.githubusercontent.com/trias-project/indicators/master/data/",
#'   "interim/data_input_checklist_indicators.tsv"
#' )
#' data <- read_tsv(datafile,
#'   na = "",
#'   col_types = cols(
#'     .default = col_character(),
#'     key = col_double(),
#'     nubKey = col_double(),
#'     speciesKey = col_double(),
#'     first_observed = col_double(),
#'     last_observed = col_double()
#'   )
#' )
#' # All taxa
#' visualize_pathways_level1(data)
#'
#' # Animalia
#' visualize_pathways_level1(data, category = "Animalia")
#'
#' # Chordata
#' visualize_pathways_level1(data, category = "Chordata")
#'
#' # facet phylum
#' visualize_pathways_level1(
#'   data,
#'   category = "Animalia",
#'   facet_column = "phylum"
#' )
#'
#' # facet habitat
#' visualize_pathways_level1(data, facet_column = "habitat")
#'
#' # Only taxa introduced from 1950
#' visualize_pathways_level1(data, from = 1950)
#'
#' # Only taxa with pathways "corridor" and "escape"
#' visualize_pathways_level1(data, pathways = c("corridor", "escape"))
#' 
#' # Pathways not present in data can also being shown if specified in
#' `pathways`
#' visualize_pathways_level1(
#'   data,
#'   category = "Fungi",
#'   pathways = c("corridor", "escape", "unknown")
#' )
#'  
#' # Add a title
#' visualize_pathways_level1(
#'   data,
#'   category = "Plantae",
#'   from = 1950,
#'   title = "Plantae - Pathway level 1 from 1950"
#' )
#'
#' # Personalize axis labels
#' visualize_pathways_level1(data, x_lab = "Aantal taxa", y_lab = "pathways")
#' }
visualize_pathways_level1 <- function(df,
                                      category = NULL,
                                      from = NULL,
                                      facet_column = NULL,
                                      pathways = NULL,
                                      pathway_level1_names = "pathway_level1",
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
  assertthat::assert_that(is.data.frame(df), msg = "`df` must be a data frame.")

  # Check pathway_level1_names
  assertthat::assert_that(is.character(pathway_level1_names),
    msg = "`pathway_level1_names` must be a character."
  )
  assertable::assert_colnames(df, pathway_level1_names, only_colnames = FALSE)

  # Check category
  if (!is.null(category)) {
    assertthat::assert_that(is.character(category),
      msg = paste0(
        "`category` must be a character. One of: ",
        paste(categories, collapse = ", "),
        "."
      )
    )
    assertthat::assert_that(length(category) == 1)
    assertthat::assert_that(category %in% categories,
      msg = paste0(
        "`category` is not correct. Choose one of: ",
        paste0(categories, collapse = ", "),
        "."
      )
    )
  }
  assertthat::assert_that(is.null(facet_column) | is.character(facet_column),
    msg = "Argument facet_column has to be NULL or a character."
  )
  if (is.character(facet_column)) {
    assertthat::assert_that(length(facet_column) == 1)
    assertable::assert_colnames(df, facet_column, only_colnames = FALSE)
    # check facet column
    valid_facets <- c(
      "family", "order", "class", "phylum", "kingdom",
      "locality", "native_range", "habitat"
    )
    facet_column <- match.arg(facet_column, valid_facets)
    assertthat::assert_that(
      is.null(category) || !(category == "Chordata" & facet_column == "phylum"),
      msg = "You cannot use phylum as facet with category Chordata."
    )
    assertthat::assert_that(
      is.null(category) || 
        !(category == "Not Chordata" & facet_column == "phylum"),
      msg = "You cannot use phylum as facet with category Not Chordata."
    )
    assertthat::assert_that(
      is.null(category) || 
        !(!is.null(category) & facet_column == "kingdom"),
      msg = "You cannot use kingdom as facet if category is selected."
    )
  }
  # Check pathways
  if (!is.null(pathways)) {
    assertthat::assert_that(is.character(pathways),
      msg = "`pathways` must be a vector of characters."
    )
  }
  # Check taxon_names
  assertthat::assert_that(is.character(taxon_names),
    msg = "`taxon_names` must be a character."
  )
  assertthat::assert_that(length(taxon_names) == 1)
  assertable::assert_colnames(df, taxon_names, only_colnames = FALSE)
  # Check kingdom_names
  assertthat::assert_that(is.character(kingdom_names),
    msg = "`kingdom_names` must be a character."
  )
  assertthat::assert_that(length(kingdom_names) == 1)
  assertable::assert_colnames(df, kingdom_names, only_colnames = FALSE)
  # check parameter phylum
  assertthat::assert_that(is.character(phylum_names),
    msg = "`phylum_names` must be a character."
  )
  assertthat::assert_that(length(phylum_names) == 1)
  assertable::assert_colnames(df, phylum_names, only_colnames = FALSE)
  # Check from
  if (!is.null(from)) {
    assertthat::assert_that(is.numeric(from),
      msg = "`from` must be a number (year)."
    )
    assertthat::assert_that(length(from) == 1)
    assertthat::assert_that(from > 0,
      msg = "`from` must be a positive number."
    )
    assertthat::assert_that(from == as.integer(from),
      msg = "`from` must be an integer."
    )
    assertthat::assert_that(
      from <= as.numeric(substr(Sys.Date(), start = 1, stop = 4)),
      msg = paste0(
        "`from` must be less than ",
        format(Sys.Date(), "%Y"),
        "."
      )
    )
  }
  # Check first_observed
  assertthat::assert_that(is.character(first_observed),
    msg = "`first_observed` must be a character."
  )
  assertthat::assert_that(length(first_observed) == 1)
  assertable::assert_colnames(df, first_observed, only_colnames = FALSE)
  # Check title and labels
  assertthat::assert_that(is.null(title) | is.character(title),
                          msg = "`title` must be a character or NULL."
  )
  if (!is.null(title)) {
    assertthat::assert_that(length(title) == 1)
  }
  assertthat::assert_that(is.character(x_lab),
                          msg = "`x_lab` must be a character or NULL."
  )
  if (!is.null(x_lab)) {
    assertthat::assert_that(length(x_lab) == 1)
  }
  assertthat::assert_that(is.character(y_lab),
                          msg = "`y_lab` must be a character or NULL."
  )
  if (!is.null(y_lab)) {
    assertthat::assert_that(length(y_lab) == 1)
  }
  # Rename to default column name
  df <-
    df %>%
    dplyr::rename_at(dplyr::vars(tidyselect::all_of(kingdom_names)), ~"group") %>%
    dplyr::rename_at(dplyr::vars(tidyselect::all_of(taxon_names)), ~"taxonKey") %>%
    dplyr::rename_at(dplyr::vars(tidyselect::all_of(first_observed)), ~"first_observed") %>%
    dplyr::rename_at(dplyr::vars(tidyselect::all_of(pathway_level1_names)), ~"pathway_level1")
  # Handle asymmetric category system (Chordata, Not Chordta are not kingdoms)
  if (!is.null(category)) {
    if (!category %in% c("Chordata", "Not Chordata")) {
      df <- df %>% dplyr::filter(.data$group == category)
    } else {
      df <-
        df %>%
        dplyr::rename_at(dplyr::vars(phylum_names), ~"phylum_group")
      if (category == "Chordata") {
        df <- df %>% dplyr::filter(.data$phylum_group == category)
      } else {
        df <-
          df %>%
          dplyr::filter(.data$group == "Animalia") %>%
          dplyr::filter(.data$phylum_group != "Chordata")
      }
    }
  }
  # Apply cut-off on year of introduction if given
  if (!is.null(from)) {
    df <-
      df %>%
      dplyr::filter(.data$first_observed >= from)
  }
  
  if (!is.null(facet_column)) {
    if (facet_column == "kingdom") {
      # Category NULL by assertion
      df$kingdom <- df$group
    }
  } 
  
  # Handle NAs and ""
  nas_or_empty_pathway_level1 <-
    df %>%
    dplyr::filter(is.na(.data$pathway_level1) |
      .data$pathway_level1 == "") %>%
    dplyr::distinct(.data$taxonKey)
  if (nrow(nas_or_empty_pathway_level1) > 0) {
    message_warning <- paste(
      nrow(nas_or_empty_pathway_level1),
      "taxa have no information about pathway level 1.",
      "Set to 'unknown'."
    )
    warning(message_warning)
  }
  df <-
    df %>%
    # Handle NAs and "unknown"
    dplyr::mutate(pathway_level1 = dplyr::if_else(is.na(.data$pathway_level1) |
      .data$pathway_level1 == "",
    "unknown",
    .data$pathway_level1
    ))
  # Import all CBD pathways level 1
  pathways_level1_all <-
    pathways_cbd() %>%
    dplyr::distinct(.data$pathway_level1)
  # Select pathways
  if (!is.null(pathways)) {
    pathways <- replace(pathways, is.na(pathways) | pathways == "", "unknown")
    pathways <- unique(pathways)
    df <-
      df %>%
      dplyr::filter(.data$pathway_level1 %in% pathways)
  } else {
    if (cbd_standard == TRUE) {
      pathways <- pathways_level1_all %>% dplyr::pull()
    } else {
      pathways <- unique(df$pathway_level1)
    }
  }
  # Check values in column with pathways level 1
  invalid_pathways <-
    df %>%
    dplyr::anti_join(pathways_level1_all,
      by = "pathway_level1"
    ) %>%
    dplyr::distinct(.data$pathway_level1) %>%
    dplyr::pull()
  message_invalid_pathways <-
    paste0(
      "No CBD standard pathways level 1 value(s) in column `",
      pathway_level1_names,
      "`: ",
      paste0(invalid_pathways, collapse = ", "),
      ". Valid pathways values: ",
      paste0(unique(pathways_level1_all$pathway_level1), collapse = ", "),
      "."
    )
  if (cbd_standard == TRUE) {
    assertthat::assert_that(length(invalid_pathways) == 0,
      msg = message_invalid_pathways
    )
  } else {
    warning(message_invalid_pathways)
  }
  # Distinct taxa
  if (!is.null(facet_column)) {
    df <-
      df %>%
      dplyr::distinct(.data$taxonKey, .data$pathway_level1, !!dplyr::sym(facet_column))
  }
  # Transform pathway level 1 column to factor to make ordering in graph easily
  df <-
    df %>%
    dplyr::mutate(
      pathway_level1 = factor(.data$pathway_level1, levels = pathways)) %>%
    dplyr::as_tibble()
  # Distinct taxa without facet
  data_top_graph <-
    df %>%
    dplyr::distinct(.data$taxonKey, .data$pathway_level1)
  # Plot number of taxa per pathway_level1
  top_graph <- NULL
  if (nrow(data_top_graph) > 0) {
    top_graph <-
      ggplot2::ggplot(
        data_top_graph
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(x = forcats::fct_rev(.data$pathway_level1), y = "count"),
        stat = "identity"
      ) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::xlab(y_lab) +
      ggplot2::ylab(x_lab) +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(title)
  } else {
    data_top_graph <- NULL
  }
  if (is.null(facet_column)) {
    return(list(plot = top_graph,
                data_top_graph = data_top_graph,
                data_facet_graph = NULL))
  } else {
    facet_graph <- NULL
    if (nrow(df) > 0) {
      facet_graph <-
        ggplot2::ggplot(
          df,
          ggplot2::aes(x = forcats::fct_rev(.data$pathway_level1))
        ) +
        ggplot2::geom_bar() +
        ggplot2::xlab(y_lab) +
        ggplot2::ylab(x_lab) +
        ggplot2::coord_flip() +
        ggplot2::ggtitle(title) +
        ggplot2::facet_wrap(facet_column)
    } else {
      df <- NULL
    }
    if (all(!is.null(top_graph), !is.null(facet_graph))) {
      return(list(plot = egg::ggarrange(top_graph, facet_graph),
                  data_top_graph = data_top_graph,
                  data_facet_graph = df))
    }
    else {
      return(list(plot = NULL, data_top_graph = NULL, data_facet_graph = NULL))
    }
  }
}
