#' Create an interactive plot for the number of alien species per native region
#' and year of introduction
#'
#' Based on
#' [countYearProvince](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/exoten/reporting-grofwild/R/countYearProvince.R)
#' plot from reporting - rshiny - grofwildjacht
#' @param df input data.frame.
#' @param years (numeric) vector years we are interested to. If \code{NULL}
#'   (default) all years from minimum and maximum of years of first observation
#'   are taken into account.
#' @param type character, native_range level of interest should be one of
#'   `c("native_range", "native_continent")`. Default: `"native_range"`. A
#'   column called as the selected `type` must be present in `df`.
#' @param x_lab character string, label of the x-axis. Default: "year".
#' @param y_lab character string, label of the y-axis. Default: "number of alien
#'   species".
#' @param relative (logical) if TRUE (default), each bar is standardised before
#'   stacking.
#' @param first_observed (character) Name of the column in `data`
#'   containing temporal information about introduction of the alien species.
#'   Expressed as years.
#' @return list with: \itemize{ \item{'static_plot': }{ggplot object, for a
#'   given species the observed number per year and per native range is plotted
#'   in a stacked bar chart} \item{'interactive_plot': }{plotly object, for a
#'   given species the observed number per year and per native range is plotted
#'   in a stacked bar chart} \item{'data': }{data displayed in the plot, as
#'   data.frame with: \itemize{ \item{'year': }{year at which the species were
#'   introduced} \item{'native_range': }{native range of the introduced species}
#'   \item{'n': }{number of species introduced from the native range for a given
#'   year} \item{'total': }{total number of species, from all around the world,
#'   introduced during a given year} \item{'perc': }{percentage of species
#'   introduced from the native range for a given year. (n/total)*100} } } }
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
    x_lab = "year",
    y_lab = "alien species",
    relative = FALSE,
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
  assertthat::assert_that(is.logical(relative),
                          msg = "Argument relative has to be a logical."
  )
  assertthat::assert_that(is.character(first_observed),
                          msg = "Argument first_observed has to be a character."
  )
  assertable::assert_colnames(df, first_observed, only_colnames = FALSE)
  
  
  # Rename to default column name
  df <-
    df %>%
    dplyr::rename_at(dplyr::vars(first_observed), ~"first_observed")

  if (is.null(years)) {
    years <- sort(unique(df$first_observed))
  }

  plotData <- df

  plotData$location <- switch(type,
    native_range = plotData$native_range,
    native_continent = plotData$native_continent
  )

  # Select data
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

  # Summarize data per year
  totalCount <- table(plotData$first_observed)


  # For optimal displaying in the plot
  summaryData$location <- as.factor(summaryData$location)
  summaryData$location <- factor(summaryData$location, levels = rev(levels(summaryData$location)))
  summaryData$first_observed <- as.factor(summaryData$first_observed)



  # Create plot

  if (relative == TRUE) {
    position <- "fill"
    text <- paste0(summaryData$location, "<br>", summaryData$perc, "%")
  } else {
    position <- "stack"
    text <- paste0(summaryData$location, "<br>", summaryData$value)
  }

  pl <- ggplot2::ggplot(data = summaryData, ggplot2::aes(
    x = .data$first_observed,
    y = .data$value,
    fill = .data$location,
    text = text
  )) +
    ggplot2::geom_bar(position = position, stat = "identity") +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))

  if (relative == TRUE) {
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
  pl$elementId <- NULL

  # Change variable name
  names(summaryData)[names(summaryData) == "value"] <- "n"
  names(summaryData)[names(summaryData) == "first_observed"] <- "year"
  names(summaryData)[names(summaryData) == "location"] <- "native_range"

  return(list(static_plot = pl, interactive_plot = pl_2, data = summaryData))
}
