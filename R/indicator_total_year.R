#' Create cumulative number of alien species indicator plot.
#'
#' This function calculates the cumulative number of taxa introduced per year.
#' To do this, a column of input dataframe containing temporal information about
#' year of introduction is required.
#' @param df df. Contains the data as produced by the Trias pipeline, with
#'   minimal columns.
#' @param start_year_plot numeric. Limit to use as start year of the plot. For
#'   scientific usage, the entire period could be relevant, but for policy
#'   purpose, focusing on a more recent period could be required. Default: 1940.
#' @param x_major_scale_stepsize integer. On which year interval labels are
#'   placed on the x axis. Default: 10.
#' @param x_minor_scale_stepsize integer. On which year interval minor breaks
#'   are placed on the x axis. Default: 5.
#' @param facet_column NULL or character. Name of the column to use to create
#'   additional facet wrap plots underneath the main graph. When NULL, no facet
#'   graph is included. It is typically one of the highest taxonomic ranks, e.g.
#'   \code{"kingdom"}, \code{"phylum"}, \code{"class"}, \code{"order"},
#'   \code{"family"}. Other typical breakwdowns could be geographically related,
#'   e.g. \code{"country"}, \code{"locality"}, \code{"pathway"} of introduction
#'   or \code{"habitat"}. Default: `NULL`.
#' @param taxon_key_col character. Name of the column of \code{df} containing
#'   unique taxon IDs. Default: \code{key}.
#' @param first_observed character. Name of the column of \code{df} containing
#'   information about year of introduction. Default: \code{"first_observed"}.
#' @param x_lab NULL or character. To personalize or remove the x-axis label.
#'   Default: "Year.
#' @param y_lab NULL or character. To personalize or remove the y-axis label.
#'   Default: "Cumulative number of alien species".
#'
#' @return A list with three slots:
#' - `plot`: ggplot2 object (or egg object if facets are used).
#' - `data_top_graph`: data.frame (tibble) with data used for the main plot (top graph) in `plot`.
#' - `data_facet_graph`: data.frame (tibble) with data used for the faceting
#' plot in `plot`. If `facet_column` is NULL, NULL is returned.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertable assert_colnames
#' @importFrom dplyr %>% filter rowwise do bind_cols group_by count ungroup
#'   rename_at distinct .data syms
#' @importFrom tidyr unnest
#' @importFrom rlang !!!
#' @importFrom ggplot2 coord_cartesian ggplot geom_line aes xlab ylab scale_x_continuous
#'   facet_wrap
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
#' start_year_plot <- 1900
#' x_major_scale_stepsize <- 25
#' x_minor_scale_stepsize <- 5
#' # without facets
#' indicator_total_year(data, start_year_plot, x_major_scale_stepsize)
#' # with facets
#' indicator_total_year(data, start_year_plot, facet_column = "kingdom")
#' # specify name of column containing year of introduction (first_observed)
#' indicator_total_year(data, first_observed = "first_observed")
#' # specify axis labels
#' indicator_total_year(data, x_lab = "YEAR", y_lab = NULL)
#' }
indicator_total_year <- function(df, start_year_plot = 1940,
                                 x_major_scale_stepsize = 10,
                                 x_minor_scale_stepsize = 5,
                                 facet_column = NULL,
                                 taxon_key_col = "key",
                                 first_observed = "first_observed",
                                 x_lab = "Year",
                                 y_lab = "Cumulative number of alien species") {

  # initial input checks
  assert_that(is.data.frame(df))
  assert_that(is.numeric(start_year_plot),
    msg = "Argument start_year_plot has to be a number."
  )
  assert_that(start_year_plot < as.integer(format(Sys.Date(), "%Y")),
    msg = paste(
      "Argument start_year_plot has to be less than",
      format(Sys.Date(), "%Y")
    )
  )
  assert_that(is.numeric(x_major_scale_stepsize),
    msg = "Argument x_major_scale_stepsize has to be a number."
  )
  assert_that(is.numeric(x_minor_scale_stepsize),
    msg = "Argument x_minor_scale_stepsize has to be a number."
  )
  assert_that(x_major_scale_stepsize >= x_minor_scale_stepsize,
    msg = paste0(
      "x_major_scale_stepsize should be greater ",
      "than x_minor_scale_stepsize."
    )
  )
  assert_that(is.null(facet_column) | is.character(facet_column),
    msg = "Argument facet_column has to be NULL or a character."
  )
  if (is.character(facet_column)) {
    assert_colnames(df, facet_column, only_colnames = FALSE)
  }

  # check for valid facet options
  valid_facet_options <- c(
    "family", "order", "class", "phylum",
    "kingdom", "pathway_level1", "locality",
    "native_range", "habitat"
  )
  if (!is.null(facet_column)) {
    facet_column <- match.arg(facet_column, valid_facet_options)
  }

  assert_that(is.character(taxon_key_col),
    msg = "Argument taxon_key_col has to be a character."
  )
  assert_colnames(df, taxon_key_col, only_colnames = FALSE)
  assert_that(is.character(first_observed),
    msg = "Argument first_observed has to be a character."
  )
  assert_colnames(df, first_observed, only_colnames = FALSE)

  assert_that(is.null(x_lab) | is.character(x_lab),
    msg = "Argument x_lab has to be a character or NULL."
  )
  assert_that(is.null(y_lab) | is.character(y_lab),
    msg = "Argument y_lab has to be a character or NULL."
  )
  # rename to default column name
  df <-
    df %>%
    rename_at(vars(first_observed), ~"first_observed") %>%
    rename_at(vars(taxon_key_col), ~"key")

  # Provide warning messages for first_observed NA values
  n_first_observed_not_present <-
    df %>%
    filter(is.na(.data$first_observed)) %>%
    nrow()
  if (n_first_observed_not_present) {
    warning(paste0(
      n_first_observed_not_present,
      " records have no information about year of introduction ",
      "(empty values in column ",
      first_observed,
      ") and are not taken into account.\n"
    ))
  }

  # Filter the incoming data
  df <-
    df %>%
    filter(!is.na(.data$first_observed))

  # Distinct values in columns of interest
  if (is.null(facet_column)) {
    df <-
      df %>%
      distinct(.data$key, .data$first_observed)
  } else {
    df <-
      df %>%
      distinct(.data$key, .data$first_observed, .data[[facet_column]])
  }

  # Make individual records for each year up to now
  maxDate <- as.integer(format(Sys.Date(), "%Y"))
  df_extended <- df %>%
    rowwise() %>%
    do(year = .data$first_observed:maxDate) %>%
    bind_cols(df) %>%
    unnest(.data$year)

  top_graph <- ggplot(df_extended, aes(x = .data$year)) +
    geom_line(stat = "count") +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_x_continuous(
      breaks = seq(
        start_year_plot,
        maxDate,
        x_major_scale_stepsize
      ),
      minor_breaks = seq(
        start_year_plot,
        maxDate,
        x_minor_scale_stepsize
      )
    ) +
    coord_cartesian(xlim = c(start_year_plot, maxDate))

  if (is.null(facet_column)) {
    return(list(plot = top_graph,
                data_top_graph = df_extended,
                data_facet_graph = NULL))
  } else {

    # calculate numbers
    counts_ias_grouped <-
      df_extended %>%
      group_by(!!!syms(c("year", facet_column))) %>%
      count() %>%
      ungroup()

    facet_graph <-
      ggplot(
        counts_ias_grouped,
        aes(x = .data$year, y = .data$n)
      ) +
      geom_line(stat = "identity") +
      xlab(x_lab) +
      ylab(y_lab) +
      facet_wrap(facet_column) +
      scale_x_continuous(
        breaks = seq(start_year_plot, maxDate, x_major_scale_stepsize),
        minor_breaks = seq(start_year_plot, maxDate, x_minor_scale_stepsize)
      ) +
      coord_cartesian(xlim = c(start_year_plot, maxDate))

    return(list(plot = ggarrange(top_graph, facet_graph),
                data_top_graph = df_extended,
                data_facet_graph = counts_ias_grouped)
    )
  }
}
