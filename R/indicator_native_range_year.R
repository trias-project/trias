#' Create an interactive plot for the number of alien species per native region
#' and year of introduction
#'
#' Based on
#' [countYearProvince](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/exoten/reporting-grofwild/R/countYearProvince.R)
#' plot from reporting - rshiny - grofwildjacht
#' @param df input data.frame.
#' @param years (numeric) vector years we are interested to. If `NULL`
#'   (default) all years from minimum and maximum of years of first observation
#'   are taken into account.
#' @param type character, native_range level of interest should be one of
#'   `c("native_range", "native_continent")`. Default: `"native_range"`. A
#'   column called as the selected `type` must be present in `df`.
#' @param x_major_scale_stepsize (integer) Parameter that indicates the breaks
#'   of the x axis. Default: 10.
#' @param x_lab character string, label of the x-axis. Default: "year".
#' @param y_lab character string, label of the y-axis. Default: "number of alien
#'   species".
#' @param response_type (character) summary type of response to be displayed;
#'   should be one of `c("absolute", "relative", "cumulative")`. 
#'   Default: `"absolute"`. If "absolute" the number per year and location
#' is displayed; if "relative" each bar is standardised per year before stacking;
#' if "cumulative" the cumulative number over years per location.
#' @param relative (logical) if TRUE each bar is standardised before
#'   stacking. Deprecated, use `response_type = "relative"` instead.
#' @param taxon_key_col character. Name of the column of `df` containing
#'   taxon IDs. Default: `"key"`.
#' @param first_observed (character) Name of the column in `data`
#'   containing temporal information about introduction of the alien species.
#'   Expressed as years.
#' @return list with:
#' - `static_plot`: ggplot object, for a
#'   given species the observed number per year and per native range is plotted
#'   in a stacked bar chart.
#' - `interactive_plot`: plotly object, for a
#'   given species the observed number per year and per native range is plotted
#'   in a stacked bar chart.
#' - `data`: data displayed in the plot, as a data.frame with: 
#'   - `year`: year at which the species were introduced.
#'   - `native_range`: native range of the introduced species.
#'   - `n`: number of species introduced from the native range for a given year.
#'   - `total`: total number of species, from all around the world, introduced.
#'   during a given year.
#'   - `perc`: percentage of species introduced from the native range for a 
#'   given year, `n`/`total`*100.
#' @export
#' @importFrom dplyr %>% .data
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
#' indicator_native_range_year(data, "native_continent", years = c(2010,2013))
#' }
indicator_native_range_year <- function(
    df,
    years = NULL,
    type = c("native_range", "native_continent"),
    x_major_scale_stepsize = 10,
    x_lab = "year",
    y_lab = "alien species",
    response_type = c("absolute", "relative", "cumulative"),
    relative = lifecycle::deprecated(),
    taxon_key_col = "key",
    first_observed = "first_observed") {
    
  
  # initial input checks
  assertthat::assert_that(is.data.frame(df))
  if (!is.null(years)) {
    assertthat::assert_that(all(is.numeric(years)),
                            msg = "Argument years has to be a number."
    )
    assertthat::assert_that(
      all(years < as.integer(format(Sys.Date(), "%Y"))),
      msg = sprintf(
        "All values in years has to be less than %s.", format(Sys.Date(), "%Y")
      )
    )
  }
  type <- match.arg(type)
  assertthat::assert_that(type %in% names(df),
                          msg = sprintf("Column %s not present in df.", type)
  )
  assertthat::assert_that(is.numeric(x_major_scale_stepsize),
    msg = "Argument x_major_scale_stepsize has to be a number."
  )
  if (!is.null(x_lab)) {
    assertthat::assert_that(is.character(x_lab),
                            msg = "Argument x_lab has to be a character or NULL."
    )
  }
  if (!is.null(y_lab)) {
    assertthat::assert_that(is.character(y_lab),
                            msg = "Argument y_lab has to be a character or NULL."
    )
    
  }
  
  response_type <- match.arg(response_type)
  # Check relative (deprecated)
  warning_detail <- paste(
    "Use argument `response_type = 'relative'` for displaying relative numbers",
    "in the plot. The use of boolean parameter `relative` is deprecated since",
    "version 3.0.0."
  )
  if (lifecycle::is_present(relative)) {
    lifecycle::deprecate_warn(
      when = "3.0.0",
      what = "trias::indicator_native_range_year(relative)",
      details = warning_detail
    )
  }
  # Define the right response_type
  if (lifecycle::is_present(relative)) {
    response_type <- "relative"
  }
  
  assertthat::assert_that(is.character(taxon_key_col),
    msg = "Argument taxon_key_col has to be a character."
  )
  assertable::assert_colnames(df, taxon_key_col, only_colnames = FALSE)
  assertthat::assert_that(is.character(first_observed),
                          msg = "Argument first_observed has to be a character."
  )
  assertable::assert_colnames(df, first_observed, only_colnames = FALSE)
  
  
  # Rename to default column name
  df <-
    df %>%
    dplyr::rename_at(dplyr::vars(dplyr::all_of(first_observed)), ~"first_observed") %>%
    dplyr::rename_at(dplyr::vars(dplyr::all_of(taxon_key_col)), ~"key")
  
  if (is.null(years)) {
    years <- sort(unique(df$first_observed))
  }

  plotData <- df

  plotData$location <- switch(type,
    native_range = plotData$native_range,
    native_continent = plotData$native_continent
  )

  # Select data
  plotData <- plotData[!duplicated(plotData[, c("key", "first_observed", "location")]), ]
  plotData <- plotData[plotData$first_observed %in% years, c("first_observed", "location")]
  plotData <- plotData[!is.na(plotData$first_observed) & !is.na(plotData$location), ]
  
  # Set location and first_observed to factors
  plotData$first_observed <- as.factor(plotData$first_observed)
  plotData$location <- as.factor(plotData$location)
  plotData$location <- droplevels(plotData$location)
  
  
  # Summarize data per native_range and year
  summaryData <- reshape2::melt(table(plotData), id.vars = "first_observed")
  summaryData <- summaryData %>%
    dplyr::group_by(.data$first_observed) %>%
    dplyr::mutate(
      total = sum(.data$value),
      perc = round((.data$value / .data$total) * 100, 2)
    )
  if (response_type == "cumulative")
    summaryData <- summaryData %>%
      dplyr::group_by(.data$location) %>%
      dplyr::mutate(
        value = cumsum(.data$value)
      )

  # For optimal displaying in the plot
  summaryData$location <- as.factor(summaryData$location)
  summaryData$location <- factor(summaryData$location, levels = rev(levels(summaryData$location)))
  summaryData$first_observed <- as.factor(summaryData$first_observed)



  # Create plot

  if (response_type == "relative") {
    position <- "fill"
    text <- paste0(summaryData$location, 
      "<br>", y_lab, ": ", summaryData$perc, "%", 
      "<br>", x_lab, ": ", summaryData$first_observed)
  } else {
    position <- "stack"
    text <- paste0(summaryData$location, 
      "<br>", y_lab, ": ", summaryData$value, 
      "<br>", x_lab, ": ", summaryData$first_observed)
  }

  pl <- ggplot2::ggplot(data = summaryData, ggplot2::aes(
    x = .data$first_observed,
    y = .data$value,
    fill = .data$location,
    text = text
  )) +
    ggplot2::geom_bar(position = position, stat = "identity") +
    ggplot2::scale_x_discrete(
      breaks = nice_seq(
        start_year = min(years, na.rm = TRUE),
        end_year = max(years, na.rm = TRUE),
        step_size = x_major_scale_stepsize
      )
    ) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))

  if (response_type == "relative") {
    pl <- pl + ggplot2::scale_y_continuous(labels = scales::percent_format())
  }

  pl_2 <- plotly::ggplotly(data = summaryData, pl, tooltip = "text") %>%
    plotly::layout(
      xaxis = list(title = x_lab, tickangle = "auto"),
      yaxis = list(title = y_lab, tickformat = ",d"),
      margin = list(b = 80, t = 100),
      barmode = ifelse(nlevels(summaryData$first_observed) == 1, "group", "stack")
    )

  # To prevent warnings in UI
  pl_2$elementId <- NULL

  # Change variable name
  names(summaryData)[names(summaryData) == "value"] <- "n"
  names(summaryData)[names(summaryData) == "first_observed"] <- "year"
  names(summaryData)[names(summaryData) == "location"] <- "native_range"

  return(list(static_plot = pl, interactive_plot = pl_2, data = summaryData))
}
