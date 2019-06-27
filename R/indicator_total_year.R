#' Create cumulative number of alien species indicator plot.
#'
#' This function calculates the cumulative number of species introduced per
#' year. To do this, a column of input dataframe containing temporal information
#' about year of introduction is required.
#' @param df df. Contains the data as produced by the Trias pipeline,
#'   with minimal columns.
#' @param start_year_plot integer. Limit to use as start year of the plot. For
#'   scientific usage, the entire period could be relevant, but for policy
#'   purpose, focusing on a more recent period could be required.
#' @param x_major_scale_stepsize integer. On which year interval labels are
#'   placed on the x axis.
#' @param x_minor_scale_stepsize integer. On which year interval minor breaks
#'   are placed on the x axis.
#' @param facet_column NULL or character. Name of the column to use to create
#'   additional facet wrap plots underneath the main graph. When NULL, no facet
#'   graph is included. It is typically one of the highest taxonomic ranks, e.g.
#'   \code{"kingdom"}, \code{"phylum"}, \code{"class"}, \code{"order"},
#'   \code{"family"}. Other typical breakwdowns could be geographically related,
#'   e.g. \code{"country"}, \code{"locality"}, \code{"pathway"} of
#'   introduction or \code{"habitat"}.
#' @param first_observed character. Name of the column of \code{df} containing
#'   information about year of introduction. Default: \code{"first_observed"}.
#'
#' @return ggplot2 object
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertable assert_colnames
#' @importFrom dplyr distinct_ %>% filter rowwise do bind_cols group_by_ count
#'   ungroup rename_at
#' @importFrom tidyr unnest
#' @importFrom ggplot2 ggplot geom_line aes xlab ylab scale_x_continuous
#'   facet_wrap
#' @importFrom egg ggarrange
#'
#' @examples
#' \dontrun{
#' library(readr)
#' datafile <- paste0(
#'   "https://raw.githubusercontent.com/trias-project/pipeline/master/data/",
#'   "interim/test_data_output_checklist_indicators.tsv"
#' )
#' data <- read_tsv(datafile)
#' start_year_plot <- 1900
#' x_major_scale_stepsize <- 25
#' x_minor_scale_stepsize <- 5
#' # without facets
#' indicator_total_year(data, start_year_plot, x_major_scale_stepsize)
#' # with facets
#' indicator_total_year(data, start_year_plot, facet_column = "phylum")
#' # specify name of column containing year of introduction (first_observed)
#' indicator_total_year(data, first_observed = "first_observed")
#' }
indicator_total_year <- function(df, start_year_plot = 1940,
                                 x_major_scale_stepsize = 10,
                                 x_minor_scale_stepsize = 5,
                                 facet_column = NULL,
                                 first_observed = "first_observed") {

  # initial input checks
  assert_that(is.data.frame(df))
  assert_that(x_major_scale_stepsize >= x_minor_scale_stepsize)
  assert_colnames(df, first_observed, only_colnames = FALSE)
  if (is.character(facet_column)) {
    assert_colnames(df, facet_column, only_colnames = FALSE)
  }

  # rename to default column name
  df <-
    df %>%
    rename_at(vars(first_observed), ~"first_observed")

  if (is.null(facet_column)) {
    df_cleaned <-
      df %>%
      distinct_("speciesKey", "first_observed")
  } else {
    df_cleaned <-
      df %>%
      distinct_("speciesKey", "first_observed", facet_column)
  }

  # Provide warning messages for first_observed NA values
  if (nrow(filter(df, is.na(first_observed)) > 0)) {
    warning(paste0(
      "Some records have no information about year of introduction (empty values in column ",
      first_observed,
      ") and are not taken into account."
    ))
  }

  df_cleaned <-
    df_cleaned %>%
    filter(!is.na(first_observed)) # ignore information without first_observed

  # Make individual records for each year up to now
  df_extended <- df_cleaned %>%
    rowwise() %>%
    do(year = .data$first_observed:as.integer(format(Sys.Date(), "%Y"))) %>%
    bind_cols(df_cleaned) %>%
    unnest(year)

  maxDate <- max(df_extended$year)
  top_graph <- ggplot(df_extended, aes(x = year)) +
    geom_line(stat = "count") +
    xlab("Year") +
    ylab("Cumulative number of alien species") +
    scale_x_continuous(
      breaks = seq(
        start_year_plot, maxDate,
        x_major_scale_stepsize
      ),
      limits = c(start_year_plot, maxDate)
    )

  if (is.null(facet_column)) {
    return(top_graph)
  } else {
    # check for valid facet options
    valid_facet_options <- c(
      "family", "order", "class", "phylum",
      "kingdom", "pathway_level1", "locality",
      "native_range", "habitat"
    )
    facet_column <- match.arg(facet_column, valid_facet_options)
    # calculate numbers
    counts_ias_grouped <-
      df_extended %>%
      group_by_("year", facet_column) %>%
      count() %>%
      ungroup()

    facet_graph <- ggplot(
      counts_ias_grouped,
      aes(x = year, y = n)
    ) +
      geom_line(stat = "identity") +
      xlab("Year") +
      ylab("Cumulative number of alien species") +
      facet_wrap(facet_column) +
      scale_x_continuous(
        breaks = seq(start_year_plot, maxDate, x_major_scale_stepsize),
        minor_breaks = seq(start_year_plot, maxDate, x_minor_scale_stepsize),
        limits = c(start_year_plot, maxDate)
      )

    ggarrange(top_graph, facet_graph)
  }
}
