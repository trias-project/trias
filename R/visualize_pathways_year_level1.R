#' Plot number of introduced taxa over time for pathways level 1
#'
#' Function to plot a line graph with number of taxa introduced over time
#' through different CBD pathways level 1. Time expressed in years. Possible
#' breakpoints: taxonomic (kingdoms + vertebrates/invertebrates).
#' @param df df.
#' @param category NULL or character. One of the kingdoms as given in GBIF and
#'   `Chordata` (the phylum), `Not Chordata` (all other phyla of `Animalia`): 1.
#'   `Plantae` 2. `Animalia` 3. `Fungi` 4. `Chromista` 5. `Archaea` 6.
#'   `Bacteria` 7. `Protozoa` 8. `Viruses` 9. `incertae sedis` 10. `Chordata`
#'   11. `Not Chordata` Default: `NULL`.
#' @param from numeric. Year trade-off: taxa introduced before this year are
#'   grouped all together. Default: 1950.
#' @param facet_column NULL or character. The column to use to create additional
#'   facet wrap bar graphs underneath the main graph. When NULL, no facet graph
#'   are created. One of `family`, `order`, `class`, `phylum`, `locality`,
#'   `native_range`, `habitat`. If column has another name, rename it before
#'   calling this function. Default: `NULL`.
#' @param pathway_level1_names character. Name of the column of \code{df}
#'   containing information about pathways at level 1. Default:
#'   `pathway_level1`.
#' @param pathways character. Vector with pathways level 1 to visualize. The
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
#' @param cbd_standard logical. If TRUE the values of pathway level 1 are
#'   checked based on CBD standard as returned by `pathways_cbd()`. Error is
#'   returned if unmatched values are found. If FALSE, a warning is returned.
#'   Default: TRUE.
#' @param title NULL or character. Title of the graph. Default: NULL.
#' @param x_lab NULL or character. x-axis label. Default: "Number of introduced
#'   taxa".
#' @param y_lab NULL or character. Title of the graph. Default: "Pathways".
#' @return A ggplot2 object (or egg object if facets are used).
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertable assert_colnames
#' @importFrom dplyr %>% anti_join count distinct filter group_by if_else mutate
#'   pull rename_at sym ungroup
#' @importFrom egg ggarrange
#' @importFrom ggplot2 facet_wrap geom_line geom_point ggplot ggtitle xlab ylab
#' @importFrom rlang !!
#' @importFrom tidyselect all_of
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
#'     acceptedKey = col_double(),
#'     first_observed = col_double(),
#'     last_observed = col_double()
#'   )
#' )
#' # All taxa
#' visualize_pathways_year_level1(data)
#'
#' # Animalia
#' visualize_pathways_year_level1(data, category = "Animalia")
#'
#' # Chordata
#' visualize_pathways_year_level1(data, category = "Chordata")
#'
#' # Group by 20 years
#' visualize_pathways_year_level1(data, bin = 20)
#'
#' # Group taxa introudced before 1970 alltogether
#' visualize_pathways_year_level1(data, from = 1970)
#'
#' # facet locality
#' visualize_pathways_year_level1(data, category = "Not Chordata", facet_column = "locality")
#'
#' # facet habitat
#' visualize_pathways_year_level1(data, facet_column = "habitat")
#'
#' # Only taxa with pathways "corridor" and "escape"
#' visualize_pathways_year_level1(data, pathways = c("corridor", "escape"))
#'
#' # Add a title
#' visualize_pathways_year_level1(data, category = "Plantae", from = 1950, title = "Pathway level 1: Plantae")
#'
#' # Personalize axis labels
#' visualize_pathways_year_level1(data, x_lab = "Jaar", y_lab = "Aantal geïntroduceerde taxa")
#' }
visualize_pathways_year_level1 <- function(df,
                                           bin = 10,
                                           from = 1950,
                                           category = NULL,
                                           facet_column = NULL,
                                           pathways = NULL,
                                           pathway_level1_names = "pathway_level1",
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
  assert_that(is.data.frame(df), msg = "`df` must be a data frame.")
  # Check bin
  assert_that(is.numeric(bin), msg = "`bin` must be a number.")
  assert_that(bin == as.integer(bin), msg = "`bin` must be an integer.")
  # Check pathway_level1_names
  assert_that(is.character(pathway_level1_names),
              msg = "`pathway_level1_names` must be a character."
  )
  assert_colnames(df, pathway_level1_names, only_colnames = FALSE)
  
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
  assert_that(is.null(facet_column) | is.character(facet_column),
              msg = "Argument facet_column has to be NULL or a character.")
  if (is.character(facet_column)) {
    assert_colnames(df, facet_column, only_colnames = FALSE)
  }
  # Check for valid facet options
  valid_facet_options <- c(
    "family", "order", "class", "phylum",
    "locality", "native_range", "habitat"
  )
  if (is.character(facet_column)) {
    facet_column <- match.arg(facet_column, valid_facet_options)
    assert_that(is.null(category) || !(category == "Chordata" & facet_column == "phylum"),
                msg = "You cannot use phylum as facet with category Chordata.")
  }
  # Check pathways
  if (!is.null(pathways)) {
    assert_that(is.character(pathways),
                msg = "`pathways` must be a vector of characters."
    )
    invalid_pathways <- pathways[!pathways %in% 
                                   df[[pathway_level1_names]]]
    assert_that(length(invalid_pathways) == 0,
                msg = paste0("Pathways in `pathways` not present in ",
                             "data.frame: ",
                             paste(invalid_pathways, collapse = ","),
                             "."))
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
    rename_at(vars(all_of(pathway_level1_names)), ~ "pathway_level1")
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
  
  # Handle NAs and ""
  nas_or_empty_pathway_level1 <-
    df %>%
    filter(is.na(.data$pathway_level1) |
             .data$pathway_level1 == "") %>%
    distinct(taxonKey)
  if (nrow(nas_or_empty_pathway_level1) > 0) {
    message_warning <- paste(nrow(nas_or_empty_pathway_level1),
                             "taxa have no information about pathway level 1.",
                             "Set to 'unknown'.")
    warning(message_warning)
  }
  df <-
    df %>%
    # Handle NAs and "unknown"
    mutate(pathway_level1 = ifelse(is.na(.data$pathway_level1) |
                                     .data$pathway_level1 == "",
                                   "unknown",
                                   .data$pathway_level1
    ))
  # Import all CBD pathways level 1
  pathways_level1_all <- 
    pathways_cbd() %>%
    distinct(pathway_level1)
  # Select pathways
  if (!is.null(pathways)) {
    pathways <- replace(pathways, is.na(pathways) | pathways == "", "unknown")
    pathways <- unique(pathways)
    df <-
      df %>% 
      filter(.data$pathway_level1  %in% pathways)
  } else {
    if (cbd_standard == TRUE) {
      pathways <- pathways_level1_all %>% pull()
    } else {
      pathways <- unique(df$pathway_level1)
    }
  }
  # Check values in column with pathways level 1
  invalid_pathways <- 
    df %>%
    anti_join(pathways_level1_all, 
              by = "pathway_level1") %>%
    distinct(pathway_level1) %>%
    pull()
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
    assert_that(length(invalid_pathways) == 0,
                msg = message_invalid_pathways)  
  } else {
    warning(message_invalid_pathways)
  }
  # Throw warning if there are taxa without first_observed
  n_first_observed_na <-
    df %>%
    filter(is.na(first_observed)) %>%
    nrow()
  if (n_first_observed_na > 0) {
    warning(
      paste0(n_first_observed_na,
             " rows without year of introduction in column `",
             first_observed,
             "` removed."))
    df <- 
      df %>%
      filter(!is.na(first_observed))
  }
  # Distinct taxa
  if (is.null(facet_column)) {
    df <-
      df %>%
      distinct(.data$taxonKey,
               .data$first_observed,
               .data$pathway_level1)
  } else {
    df <-
      df %>%
      distinct(.data$taxonKey,
               .data$first_observed,
               .data$pathway_level1,
               !!sym(facet_column))
  }
  
  df <-
    df %>%
    mutate(bins_first_observed = 
             floor((.data$first_observed - from) / bin) * bin + from) %>% 
    mutate(bins_first_observed = if_else(
      .data$bins_first_observed < from,
      paste("before", from),
      paste(as.character(.data$bins_first_observed),
            "-",
            as.character(.data$bins_first_observed + bin - 1))))
  
  # Set order of year first_observed based on bin and from
  levels_first_observed <- 
    levels(ordered(unique(df$bins_first_observed)))
  levels_first_observed <-
    c(levels_first_observed[length(levels_first_observed)],
      levels_first_observed[1:length(levels_first_observed) - 1])
  df <- 
    df %>%
    mutate(bins_first_observed = factor(.data$bins_first_observed,
                                   levels = levels_first_observed))
  # Transform pathway level 1 column to factor to make ordering in graph easily
  df <-
    df %>%
    mutate(pathway_level1 = factor(.data$pathway_level1, levels = pathways))
  
  # Plot number of taxa per pathway_level1 over time
  df_top_graph <-
    df %>%
    group_by(.data$bins_first_observed,
           .data$pathway_level1) %>%
    count() %>%
    ungroup()
  top_graph <- 
    ggplot(df_top_graph) +
    geom_line(aes(x = .data$bins_first_observed,
                  y = .data$n,
                  group = .data$pathway_level1,
                  color = .data$pathway_level1)) +
    geom_point(aes(x = .data$bins_first_observed,
                   y = .data$n,
                   group = .data$pathway_level1,
                   color = .data$pathway_level1)) +
    xlab(x_lab) +
    ylab(y_lab) +
    ggtitle(title)
  if (is.null(facet_column)) {
    return(top_graph)
  } else{
    df_facet_graph <-
      df %>%
      group_by(.data$bins_first_observed,
               .data$pathway_level1,
               !!sym(facet_column)) %>%
      count() %>%
      ungroup()
    facet_graph <- 
      ggplot(df_facet_graph) +
      geom_line(aes(x = .data$bins_first_observed,
                    y = .data$n,
                    group = .data$pathway_level1,
                    color = .data$pathway_level1)) +
      geom_point(aes(x = .data$bins_first_observed,
                     y = .data$n,
                     group = .data$pathway_level1,
                     color = .data$pathway_level1)) +
      xlab(x_lab) +
      ylab(y_lab) +
      ggtitle(title) +
      facet_wrap(facet_column)
    ggarrange(top_graph, facet_graph)
  }
}
