#' Create an interactive plot for the number of alien species per native region
#' and year of introduction
#'
#' Based on
#' [countYearProvince][https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/exoten/reporting-grofwild/R/countYearProvince.R]
#' plot from reporting - rshiny - grofwildjacht
#' @param data input data.frame
#' @param years (numeric) vector years we are interested to. If \code{NULL}
#'   (default) all years from minimum and maximum of years of first observation
#'   are taken into account.
#' @param type character, native_range level of interest should be one of
#'   \code{c("native_continent", "native_range")}
#' @param xlab character string, label of the x-axis. Default: "year".
#' @param ylab character string, label of the y-axis. Default: "number of alien
#'   species".
#' @param relative (logical) if TRUE, each bar is standardised before stacking
#'
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
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_bar scale_y_continuous
#' @importFrom plotly ggplotly layout
#' @importFrom scales percent_format
#' @importFrom dplyr %>% mutate group_by case_when rename_at

indicator_native_range_year <- function(data, years = NULL,
                                        type = c("native_continent", "native_range"),
                                        width = NULL, height = NULL,
                                        x_lab = "year",
                                        y_lab = "alien species",
                                        relative = FALSE,
                                        first_observed = "first_observed") {
  type <- match.arg(type)

  # Rename to default column name
  data <-
    data %>%
    rename_at(vars(first_observed), ~"first_observed")
  
  if (is.null(years)) {
    years <- sort(unique(data$first_observed))
  }

  plotData <- data

  plotData$location <- switch(type,
    native_range = plotData$native_range,
    native_continent = plotData$native_continent
  )

  # Select data
  plotData <- plotData[plotData$first_observed %in% years, c("first_observed", "location")]
  plotData <- plotData[!is.na(plotData$first_observed) & !is.na(plotData$location), ]

  # Exclude unused provinces
  plotData$location <- as.factor(plotData$location)
  plotData$location <- droplevels(plotData$location)

  # Summarize data per native_range and year
  plotData$first_observed <- with(plotData, factor(first_observed,
    levels =
      min(years):max(years)
  ))

  summaryData <- melt(table(plotData), id.vars = "first_observed")
  summaryData <- summaryData %>%
    group_by(.data$first_observed) %>%
    mutate(
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

  pl <- ggplot(data = summaryData, aes(x = .data$first_observed,
                                       y = .data$value,
                                       fill = .data$location,
                                       text = text)) +
    geom_bar(position = position, stat = "identity")

  if (relative == TRUE) {
    pl <- pl + scale_y_continuous(labels = percent_format())
  }

  pl_2 <- ggplotly(data = summaryData, pl, tooltip = "text") %>%
    layout(
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
