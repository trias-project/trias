#' Plot number of introduced taxa over time for pathways level 2
#'
#' Function to plot a line graph with number of taxa introduced over time
#' through different CBD pathways level 2 for a specific CBD pathway level 1.
#' Time expressed in years. Possible breakpoints: taxonomic (kingdoms +
#' vertebrates/invertebrates).
#' 
#' @param df A data frame.
#' @param chosen_pathway_level1 character. Selected pathway level 1.
#' @param bin numeric. Time span in years to use for agggregation. Default:
#'   `10`.
#' @param from numeric. Year trade-off: taxa introduced before this year are
#'   grouped all together. Default: `1950`.
#' @param category `NULL` (default) or character. One of the kingdoms as given
#'   in GBIF or `Chordata` (the phylum) or `Not Chordata` (all other phyla of
#'   `Animalia`):
#'   1. `Plantae`
#'   2. `Animalia`
#'   3. `Fungi`
#'   4. `Chromista`
#'   5. `Archaea` 
#'   6. `Bacteria`
#'   7. `Protozoa`
#'   8. `Viruses`
#'   9. `incertae sedis`
#'   10. `Chordata`
#'   11. `Not Chordata`
#' @param facet_column `NULL` (default) or character. The column to use to
#'   create additional facet wrap bar graphs underneath the main graph. When
#'   `NULL`, no facet graph are created. One of `family`, `order`, `class`,
#'   `phylum`, `kingdom`, `locality`, `native_range`, `habitat`. If column has
#'   another name, rename it before calling this function. Facet `phylum` is not
#'   allowed in combination with `category` equal to `"Chordata"` or `"Not
#'   Chordata"`. Facet `kingdom` is allowed only with category equal to `NULL`.
#' @param pathway_level1_names character. Name of the column of `df` containing
#'   information about pathways at level 1. Default: `"pathway_level1"`.
#' @param pathway_level2_names character. Name of the column of `df` containing
#'   information about pathways at level 2. Default: `"pathway_level2"`.
#' @param pathways character. Vector with pathways level 1 to visualize. The
#'   pathways are displayed following the order as in this vector.
#' @param taxon_names character. Name of the column of `df` containing
#'   information about taxa. This parameter is used to uniquely identify taxa.
#' @param kingdom_names character. Name of the column of `df` containing
#'   information about kingdom. Default: `"kingdom"`.
#' @param phylum_names character. Name of the column of `df` containing
#'   information about phylum. This parameter is used only if `category` is
#'   one of: `"Chordata"`, `"Not Chordata"`.  Default: `"phylum"`.
#' @param first_observed character. Name of the column of `df` containing
#'   information about year of introduction. Default: `"first_observed"`.
#' @param cbd_standard logical. If `TRUE` the values of pathway level 1 are
#'   checked based on CBD standard as returned by `pathways_cbd()`. Error is
#'   returned if unmatched values are found. If `FALSE`, a warning is returned.
#'   Default: `TRUE`.
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
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape"
#' )
#'
#' # Animalia
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape",
#'   category = "Animalia"
#' )
#'
#' # Chordata
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape",
#'   category = "Chordata"
#' )
#'
#' # Group by 20 years
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape",
#'   bin = 20
#' )
#'
#' # Group taxa introudced before 1970 alltogether
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape",
#'   from = 1970
#' )
#'
#' # facet locality
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape",
#'   category = "Not Chordata",
#'   facet_column = "locality"
#' )
#'
#' # facet habitat
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape",
#'   facet_column = "habitat"
#' )
#'
#' # Only taxa with pathways "horticulture" and "pet"
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape",
#'   pathways = c("horticulture", "pet")
#' )
#'
#' # Add a title
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape",
#'   category = "Plantae",
#'   from = 1950,
#'   title = "Plantae - Pathway level 1"
#' )
#'
#' # Personalize axis labels
#' visualize_pathways_year_level2(
#'   data,
#'   chosen_pathway_level1 = "escape",
#'   x_lab = "Jaar",
#'   y_lab = "Aantal geintroduceerde taxa"
#' )
#' }
visualize_pathways_year_level2 <- function(
    df,
    chosen_pathway_level1,
    bin = 10,
    from = 1950,
    category = NULL,
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
    x_lab = "Time period",
    y_lab = "Number of introduced taxa") {
  # initial input checks
  # Check df
  assertthat::assert_that(is.data.frame(df), msg = "`df` must be a data frame.")
  # Check bin
  assertthat::assert_that(is.numeric(bin), msg = "`bin` must be a number.")
  assertthat::assert_that(length(bin) == 1)
  assertthat::assert_that(
    bin == as.integer(bin),
    msg = "`bin` must be an integer."
  )
  # Check pathway_level1_names
  assertthat::assert_that(is.character(pathway_level1_names),
    msg = "`pathway_level1_names` must be a character."
  )
  assertthat::assert_that(length(pathway_level1_names) == 1)
  assertable::assert_colnames(df, pathway_level1_names, only_colnames = FALSE)
  # Check pathway_level2_names
  assertthat::assert_that(is.character(pathway_level2_names),
    msg = "`pathway_level2_names` must be a character."
  )
  assertthat::assert_that(length(pathway_level2_names) == 1)
  assertable::assert_colnames(df, pathway_level2_names, only_colnames = FALSE)
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
    # Check for valid facet options
    valid_facet_options <- c(
      "kingdom", "family", "order", "class", "phylum",
      "locality", "native_range", "habitat"
    )
    facet_column <- match.arg(facet_column, valid_facet_options)
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
  # Check chosen_pathway_level1
  assertthat::assert_that(is.character(chosen_pathway_level1),
    msg = "Argument `chosen_pathway_level1` must be a character."
  )
  assertthat::assert_that(length(chosen_pathway_level1) == 1)
  pathways_level1 <- unique(df[[pathway_level1_names]])
  assertthat::assert_that(chosen_pathway_level1 %in% pathways_level1,
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
    assertthat::assert_that(is.character(pathways),
      msg = "`pathways` must be a vector of characters."
    )
    invalid_pathways <- pathways[!pathways %in%
      df[[pathway_level2_names]]]
    assertthat::assert_that(length(invalid_pathways) == 0,
      msg = paste0(
        "Pathways in `pathways` not present in ",
        "data.frame: ",
        paste(invalid_pathways, collapse = ","),
        "."
      )
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
  if (!is.null(title)) {
    assertthat::assert_that(is.character(title),
      msg = "`title` must be a character or NULL."
    )
    assertthat::assert_that(length(title) == 1)
  }
  if (!is.null(x_lab)) {
    assertthat::assert_that(is.character(x_lab),
      msg = "`x_lab` must be a character or NULL."
    )
    assertthat::assert_that(length(x_lab) == 1)
  }
  if (!is.null(y_lab)) {
    assertthat::assert_that(is.character(y_lab),
      msg = "`y_lab` must be a character or NULL."
    )
    assertthat::assert_that(length(y_lab) == 1)
  }
  # rename to default column name
  df <-
    df %>%
    dplyr::rename_at(vars(tidyselect::all_of(kingdom_names)), ~"group") %>%
    dplyr::rename_at(vars(tidyselect::all_of(taxon_names)), ~"taxonKey") %>%
    dplyr::rename_at(vars(tidyselect::all_of(first_observed)), ~"first_observed") %>%
    dplyr::rename_at(vars(tidyselect::all_of(pathway_level1_names)), ~"pathway_level1") %>%
    dplyr::rename_at(vars(tidyselect::all_of(pathway_level2_names)), ~"pathway_level2")
  # Select data with the chosen pathway level 1
  df <-
    df %>%
    dplyr::filter(.data$pathway_level1 == chosen_pathway_level1)
  # handle asymmetric category system (Chordata, Not Chordta are not kingdoms)
  if (!is.null(category)) {
    if (!category %in% c("Chordata", "Not Chordata")) {
      df <- df %>% dplyr::filter(.data$group == category)
    } else {
      df <-
        df %>%
        dplyr::rename_at(vars(phylum_names), ~"phylum_group")
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
  # Handle NAs and ""
  nas_or_empty_pathway_level2 <-
    df %>%
    dplyr::filter(is.na(.data$pathway_level2) |
      .data$pathway_level2 == "") %>%
    dplyr::distinct(.data$taxonKey)
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
    dplyr::mutate(pathway_level2 = dplyr::if_else(is.na(.data$pathway_level2) |
      .data$pathway_level2 == "",
    "unknown",
    .data$pathway_level2
    ))
  # Import all CBD pathways level 2 within chosen pathway level 1
  pathways_level2_all <-
    pathways_cbd() %>%
    dplyr::filter(.data$pathway_level1 == chosen_pathway_level1) %>%
    dplyr::distinct(.data$pathway_level2)
  # Select pathways
  if (!is.null(pathways)) {
    pathways <- replace(pathways, is.na(pathways) | pathways == "", "unknown")
    pathways <- unique(pathways)
    df <-
      df %>%
      dplyr::filter(.data$pathway_level2 %in% pathways)
  } else {
    if (cbd_standard == TRUE) {
      pathways <- pathways_level2_all %>% dplyr::pull()
    } else {
      pathways <- unique(df$pathway_level2)
    }
  }
  # Check values in column with pathways level 2
  invalid_pathways <-
    df %>%
    dplyr::anti_join(pathways_level2_all,
      by = "pathway_level2"
    ) %>%
    dplyr::distinct(.data$pathway_level2) %>%
    dplyr::pull()
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
    assertthat::assert_that(length(invalid_pathways) == 0,
      msg = message_invalid_pathways
    )
  } else {
    warning(message_invalid_pathways)
  }
  # Throw warning if there are taxa without first_observed
  n_first_observed_na <-
    df %>%
    dplyr::filter(is.na(first_observed)) %>%
    nrow()
  if (n_first_observed_na > 0) {
    warning(
      paste0(
        n_first_observed_na,
        " rows without year of introduction in column `",
        first_observed,
        "` removed."
      )
    )
    df <-
      df %>%
      dplyr::filter(!is.na(first_observed))
  }
  # dplyr::distinct taxa
  if (is.null(facet_column)) {
    df <-
      df %>%
      dplyr::distinct(
        .data$taxonKey,
        .data$first_observed,
        .data$pathway_level2
      )
  } else {
    df <-
      df %>%
      dplyr::distinct(
        .data$taxonKey,
        .data$first_observed,
        .data$pathway_level2,
        !!dplyr::sym(facet_column)
      )
  }
  df <-
    df %>%
    dplyr::mutate(
      bins_first_observed =
        floor((.data$first_observed - from) / bin) * bin + from
    ) %>%
    dplyr::mutate(bins_first_observed = dplyr::if_else(
      .data$bins_first_observed < from,
      paste("before", from),
      paste(
        as.character(.data$bins_first_observed),
        "-",
        as.character(.data$bins_first_observed + bin - 1)
      )
    ))

  # Set order of year first_observed based on bin and from
  levels_first_observed <-
    levels(ordered(unique(df$bins_first_observed)))
  levels_first_observed <-
    c(
      levels_first_observed[length(levels_first_observed)],
      levels_first_observed[1:length(levels_first_observed) - 1]
    )
  df <-
    df %>%
    dplyr::mutate(bins_first_observed = factor(.data$bins_first_observed,
      levels = levels_first_observed
    ))
  # Transform pathway level 1 column to factor to make ordering in graph easily
  df <-
    df %>%
    dplyr::mutate(pathway_level2 = factor(.data$pathway_level2, levels = pathways))

  # dplyr::count number of taxa per pathway_level2 over time
  data_top_graph <-
    df %>%
    dplyr::group_by(
      .data$bins_first_observed,
      .data$pathway_level2
    ) %>%
    dplyr::count() %>%
    dplyr::ungroup()
  max_n <- max(data_top_graph$n)
  # Plot number of taxa per pathway_level2 over time
  top_graph <- NULL
  if (nrow(data_top_graph) > 0) {
    top_graph <-
      ggplot2::ggplot(data_top_graph) +
      ggplot2::geom_line(aes(
        x = .data$bins_first_observed,
        y = .data$n,
        group = .data$pathway_level2,
        color = .data$pathway_level2
      )) +
      ggplot2::geom_point(aes(
        x = .data$bins_first_observed,
        y = .data$n,
        group = .data$pathway_level2,
        color = .data$pathway_level2
      )) +
      ggplot2::ylim(0, max_n) +
      ggplot2::xlab(x_lab) +
      ggplot2::ylab(y_lab) +
      ggplot2::ggtitle(title)
  } else {
    data_top_graph <- NULL
  }
  if (is.null(facet_column)) {
    return(list(plot = top_graph,
                data_top_graph = data_top_graph,
                data_facet_graph = NULL))
  } else {
    # dplyr::count number of taxa per pathway_level2 per facet over time
    data_facet_graph <-
      df %>%
      dplyr::group_by(
        .data$bins_first_observed,
        .data$pathway_level2,
        !!dplyr::sym(facet_column)
      ) %>%
      dplyr::count() %>%
      dplyr::ungroup()
    max_n <- max(data_facet_graph$n)
    # Plot number of taxa per pathway_level2 per facet over time
    facet_graph <- NULL
    if (nrow(data_facet_graph) > 0) {
      facet_graph <-
        ggplot2::ggplot(data_facet_graph) +
        ggplot2::geom_line(aes(
          x = .data$bins_first_observed,
          y = .data$n,
          group = .data$pathway_level2,
          color = .data$pathway_level2
        )) +
        ggplot2::geom_point(aes(
          x = .data$bins_first_observed,
          y = .data$n,
          group = .data$pathway_level2,
          color = .data$pathway_level2
        )) +
        ggplot2::ylim(0, max_n) +
        ggplot2::xlab(x_lab) +
        ggplot2::ylab(y_lab) +
        ggplot2::ggtitle(title) +
        ggplot2::facet_wrap(facet_column)
    }
    if (all(!is.null(top_graph), !is.null(facet_graph))) {
      return(list(plot = egg::ggarrange(top_graph, facet_graph, draw = FALSE),
                  data_top_graph = data_top_graph,
                  data_facet_graph = data_facet_graph))
    }
    else {
      return(list(plot = NULL, data_top_graph = NULL, data_facet_graph = NULL))
    }
  }
}
