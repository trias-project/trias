#' Plot number of new introductions per year.
#'
#' @description Calculate how many new species has been introduced in a year.
#'
#' @param df A data frame.
#' @param start_year_plot Year where the plot starts from. Default: 1920.
#' @param smooth_span (numeric) Parameter for the applied
#'   \code{\link[stats]{loess}} smoother. For more information on the
#'   appropriate value, see \code{\link[ggplot2]{geom_smooth}}. Default: 0.85.
#' @param x_major_scale_stepsize (integer) Parameter that indicates the breaks
#'   of the x axis. Default: 10.
#' @param x_minor_scale_stepsize (integer) Parameter that indicates the minor
#'   breaks of the x axis. Default: 5.
#' @param facet_column NULL or character. The column to use to create additional
#'   facet wrap plots underneath the main graph. When NULL, no facet graph are
#'   created. Valid facet options: \code{"family"}, \code{"order"},
#'   \code{"class"}, \code{"phylum"}, \code{"kingdom"}, \code{"pathway_level1"},
#'   \code{"locality"}, \code{"native_range"} or  \code{"habitat"}. Default:
#'   NULL.
#' @param first_observed character. Name of the column of \code{df} containing
#'   information about year of introduction. Default: \code{first_observed}.
#' @param x_lab NULL or character. to set or remove the x-axis label.
#' @param y_lab NULL or character. to set or remove the y-axis label.
#'
#' @return A ggplot2 object (or egg object if facets are used).
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertable assert_colnames
#' @importFrom dplyr %>% filter group_by group_by_ count ungroup rename_at
#' @importFrom ggplot2 geom_point aes xlab ylab scale_x_continuous facet_wrap
#'   geom_smooth
#' @importFrom egg ggarrange
#'
#' @examples
#' \dontrun{
#' library(readr)
#' datafile <- paste0(
#'   "https://raw.githubusercontent.com/trias-project/pipeline/master/data/",
#'   "interim/test_data_output_checklist_indicators.tsv"
#' )
#' data <- read_tsv(datafile, 
#'                  na = "NA", 
#'                  col_types = cols(
#'                    .default = col_character(),
#'                    key = col_double(),
#'                    nubKey = col_double(),
#'                    speciesKey = col_double(),
#'                    acceptedKey = col_double(),
#'                    first_observed = col_double(),
#'                    last_observed = col_double()
#'                  ))
#' # without facets
#' indicator_introduction_year(data)
#' # specify start year and smoother parameter
#' indicator_introduction_year(data, start_year_plot = 1940,
#'                             smooth_span = 0.6)
#' # with facets
#' indicator_introduction_year(data, facet_column = "kingdom")
#' # specifiy columns with year of first observed
#' indicator_introduction_year(data,
#'                             first_observed = "first_observed")
#' # specify axis labels
#' indicator_introduction_year(data, x_lab = "YEAR", y_lab = NULL)
#' }
indicator_introduction_year <- function(df, start_year_plot = 1920,
                                        smooth_span = .85,
                                        x_major_scale_stepsize = 10,
                                        x_minor_scale_stepsize = 5,
                                        facet_column = NULL,
                                        first_observed = "first_observed",
                                        x_lab = "Year",
                                        y_lab = "Number of introduced alien species") {
  # initial input checks
  assert_that(is.data.frame(df))
  assert_colnames(df, c(first_observed), only_colnames = FALSE)

  # rename to default column name
  df <-
    df %>%
    rename_at(vars(first_observed), ~"first_observed")

  # first filtering of the incoming data
  data <- df %>%
    filter(!is.na(.data$first_observed)) %>%
    filter(.data$first_observed > start_year_plot)

  data_top_graph <- data %>%
    group_by(.data$first_observed) %>%
    count() %>%
    ungroup()

  maxDate <- max(data_top_graph$first_observed)
  # top graph with all counts
  top_graph <- ggplot(data_top_graph, aes(x = first_observed, y = n)) +
    geom_point(stat = "identity") +
    geom_smooth(span = smooth_span) +
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
      ),
      limits = c(
        start_year_plot,
        maxDate
      )
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

    data_facet_graph <- data %>%
      group_by_("first_observed", facet_column) %>%
      count() %>%
      ungroup()

    maxDate <- max(data_facet_graph$first_observed)
    facet_graph <- ggplot(data_facet_graph, aes(x = first_observed, y = n)) +
      geom_point(stat = "identity") +
      geom_smooth(span = smooth_span) +
      facet_wrap(facet_column) +
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
        ),
        limits = c(
          start_year_plot,
          maxDate
        )
      )

    ggarrange(top_graph, facet_graph)
  }
}
