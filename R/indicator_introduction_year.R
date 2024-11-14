#' Generate nice sequence with slightly different cut values compared to \code{\link[base]{seq}}
#' 
#' The inner values are forced to be a multiple of the stepsize
#' @param start_year (integer) The min cut value 
#' @param end_year (integer) The max cut value
#' @param step_size (integer) The max distance between two cut values 
#' @return integer vector, all cut values
#' 
nice_seq <- function(start_year, end_year, step_size) {
  
  # Calculate the first "nice" cut point (round up to the nearest multiple of step_size)
  first_nice_cut <- ceiling(start_year / step_size) * step_size
  nice_cuts <- seq(from = first_nice_cut, to = end_year, by = step_size)
  
  cuts <- c(
    start_year, 
    nice_cuts,
    if (end_year > utils::tail(nice_cuts, n = 1)) end_year 
  )
  
  return(cuts)
  
}



#' Plot number of new introductions per year.
#'
#' @description Calculate how many new species has been introduced in a year.
#'
#' @param df A data frame.
#' @param start_year_plot Year where the plot starts from. Default: 1920.
#' @param smooth_span (numeric) Parameter for the applied
#'   \code{\link[stats]{loess}} smoother. For more information on the
#'   appropriate value, see [ggplot2::geom_smooth()]. Default: 0.85.
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
#' @param taxon_key_col character. Name of the column of \code{df} containing
#'   unique taxon IDs. Default: \code{key}.
#' @param first_observed character. Name of the column of \code{df} containing
#'   information about year of introduction. Default: \code{first_observed}.
#' @param x_lab NULL or character. to set or remove the x-axis label.
#' @param y_lab NULL or character. to set or remove the y-axis label.
#'
#' @return A list with three slots:
#' - `plot`: ggplot2 object (or egg object if facets are used).
#' - `data_top_graph`: data.frame (tibble) with data used for the main plot (top graph) in `plot`.
#' - `data_facet_graph`: data.frame (tibble) with data used for the faceting
#' plot in `plot`. If `facet_column` is NULL, NULL is returned.
#'
#' @export
#' @importFrom dplyr %>% .data
#' @importFrom rlang !! !!!
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
#' # without facets
#' indicator_introduction_year(data)
#' # specify start year and smoother parameter
#' indicator_introduction_year(data,
#'   start_year_plot = 1940,
#'   smooth_span = 0.6
#' )
#' # with facets
#' indicator_introduction_year(data, facet_column = "kingdom")
#' # specify columns with year of first observed
#' indicator_introduction_year(data,
#'   first_observed = "first_observed"
#' )
#' # specify axis labels
#' indicator_introduction_year(data, x_lab = "YEAR", y_lab = NULL)
#' }
indicator_introduction_year <- function(df,
                                        start_year_plot = 1920,
                                        smooth_span = .85,
                                        x_major_scale_stepsize = 10,
                                        x_minor_scale_stepsize = 5,
                                        facet_column = NULL,
                                        taxon_key_col = "key",
                                        first_observed = "first_observed",
                                        x_lab = "Year",
                                        y_lab = "Number of introduced alien species") {
  # initial input checks
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.numeric(start_year_plot),
    msg = "Argument start_year_plot has to be a number."
  )
  assertthat::assert_that(start_year_plot < as.integer(format(Sys.Date(), "%Y")),
    msg = paste(
      "Argument start_year_plot has to be less than",
      format(Sys.Date(), "%Y")
    )
  )
  assertthat::assert_that(is.numeric(smooth_span),
    msg = "Argument smooth_span has to be a number between 0 and 1."
  )
  assertthat::assert_that(is.numeric(x_major_scale_stepsize),
    msg = "Argument x_major_scale_stepsize has to be a number."
  )
  assertthat::assert_that(is.numeric(x_minor_scale_stepsize),
    msg = "Argument x_minor_scale_stepsize has to be a number."
  )
  assertthat::assert_that(x_major_scale_stepsize >= x_minor_scale_stepsize,
    msg = paste0(
      "x_major_scale_stepsize has to be greater ",
      "than x_minor_scale_stepsize./n"
    )
  )
  assertthat::assert_that(is.null(facet_column) | is.character(facet_column),
    msg = "Argument facet_column has to be NULL or a character."
  )
  if (is.character(facet_column)) {
    assertable::assert_colnames(df, facet_column, only_colnames = FALSE)
  }
  # check for valid facet options
  valid_facet_options <- c(
    "family", "order", "class", "phylum",
    "kingdom", "pathway_level1", "locality",
    "native_range", "habitat"
  )
  if (is.character(facet_column)) {
    facet_column <- match.arg(facet_column, valid_facet_options)
  }

  assertthat::assert_that(is.character(taxon_key_col),
    msg = "Argument taxon_key_col has to be a character."
  )
  assertable::assert_colnames(df, taxon_key_col, only_colnames = FALSE)
  assertthat::assert_that(is.character(first_observed),
    msg = "Argument first_observed has to be a character."
  )
  assertable::assert_colnames(df, first_observed, only_colnames = FALSE)
  assertthat::assert_that(is.character(x_lab) | is.null(x_lab),
    msg = "Argument x_lab has to be a character or NULL."
  )
  assertthat::assert_that(is.character(y_lab) | is.null(y_lab),
    msg = "Argument y_lab has to be a character or NULL."
  )

  # Rename to default column name
  df <-
    df %>%
    dplyr::rename(first_observed = !!first_observed) %>%
    dplyr::rename(key =  !!taxon_key_col)

  # Provide warning messages for first_observed NA values
  n_first_observed_not_present <-
    df %>%
    dplyr::filter(is.na(.data$first_observed)) %>%
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

  # filter the incoming data
  data <-
    df %>%
    dplyr::filter(.data$first_observed > start_year_plot)

  # dplyr::distinct values in columns of interest
  if (is.null(facet_column)) {
    data <-
      data %>%
      dplyr::distinct(.data$key, .data$first_observed)
  } else {
    data <-
      data %>%
      dplyr::distinct(.data$key, .data$first_observed, .data[[facet_column]])
  }
  
  data_top_graph <-
    data %>%
    dplyr::group_by(.data$first_observed) %>%
    dplyr::count() %>%
    dplyr::ungroup()

  maxDate <- max(data_top_graph$first_observed)
  # top graph with all dplyr::counts
  top_graph <- ggplot2::ggplot(
    data_top_graph,
    ggplot2::aes(x = .data$first_observed, y = .data$n)
  ) +
    ggplot2::geom_point(stat = "identity") +
    ggplot2::geom_smooth(span = smooth_span) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::scale_x_continuous(
      breaks = nice_seq(
        start_year = start_year_plot,
        end_year = maxDate, 
        step_size = x_major_scale_stepsize
      ),
      minor_breaks = nice_seq(
        start_year = start_year_plot,
        end_year = maxDate, 
        step_size = x_minor_scale_stepsize
      )
    ) +
    ggplot2::coord_cartesian(xlim = c(start_year_plot, maxDate))

  if (is.null(facet_column)) {
    return(list(plot = top_graph,
                data_top_graph = data_top_graph,
                data_facet_graph = NULL))
  } else {
    data_facet_graph <- data %>%
      dplyr::group_by(!!!dplyr::syms(c("first_observed", facet_column))) %>%
      dplyr::count() %>%
      dplyr::ungroup()

    maxDate <- max(data_facet_graph$first_observed)
    facet_graph <- ggplot2::ggplot(
      data_facet_graph,
      ggplot2::aes(x = .data$first_observed, y = .data$n)
    ) +
      ggplot2::geom_point(stat = "identity") +
      ggplot2::geom_smooth(span = smooth_span) +
      ggplot2::facet_wrap(facet_column) +
      ggplot2::xlab(x_lab) +
      ggplot2::ylab(y_lab) +
      ggplot2::scale_x_continuous(
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
      ggplot2::coord_cartesian(xlim = c(start_year_plot, maxDate))

    return(list(plot = egg::ggarrange(top_graph, facet_graph, draw = FALSE),
                data_top_graph = data_top_graph,
                data_facet_graph = data_facet_graph))
  }
}
