#' Create interactive plot for counts per native region and year of introduction
#' 
#' Based on \code{\link{countYearProvince}} plot from grofwild
#' @param type character, native_range level of interest should be one of 
#' \code{c("native_continent", "native_range")} 
#' @param xlab character string, label of the x-axis. Default: "year".
#' @param ylab character string, label of the y-axis. Default: "number of 
#' alien species".  
#' 
#' @return list with:
#' \itemize{
#' \item{'static_plot': }{ggplot object, for a given species the observed number 
#' per year and per province is plotted in a stacked bar chart}
#' \item{'interactive_plot': }{plotly object, for a given species the observed number 
#' per year and per province is plotted in a stacked bar chart}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'year': }{year at which the animal was introduced}
#' \item{'native_range': }{native range of the introduced animal}
#' \item{'n': }{counts of animals}
#' }
#' }
#' }
#' @importForm plotly ggplotly, layout
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot, geom_bar, scale_y_continuous
#' @importFrom scales percent_format
#' @importFrom dplyr mutate, group_by, case_when
#' @export
indicator_native_range_year <- function(data, years = NULL, 
                                 type = c("native_continent", "native_range"),
                                 width = NULL, height = NULL, 
                                 x_lab = "year",
                                 y_lab = "alien species",
                                 relative = FALSE) {
  
  require(plotly)
  require(data.table)
  
  type <- match.arg(type)
  
  if (is.null(years))
    years <- sort(unique(data$first_observed))
  
  plotData <- data %>% 
    mutate(native_continent = case_when(grepl(pattern = "Africa", native_range, ignore.case = TRUE) ~ "Africa",
                                        grepl(pattern = "America", native_range, ignore.case = TRUE) ~ "America",
                                        grepl(pattern = "Asia", native_range, ignore.case = TRUE) ~ "Asia",
                                        grepl(pattern = "Australia", native_range, ignore.case = TRUE) ~ "Oceania",
                                        grepl(pattern = "nesia", native_range, ignore.case = TRUE) ~ "Oceania",
                                        grepl(pattern = "Europe", native_range, ignore.case = TRUE) ~ "Europe",
                                        TRUE ~ as.character(NA)))
    
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
  plotData$first_observed <- with(plotData, factor(first_observed, levels = 
                                                     min(years):max(years)))
  
  summaryData <- melt(table(plotData), id.vars = "first_observed")
  summaryData <- summaryData %>% 
    group_by(first_observed) %>% 
    mutate(total = sum(value),
           perc = round((value/total)*100,2))
  
  # Summarize data per year
  totalCount <- table(plotData$first_observed)
  
  
  # For optimal displaying in the plot
  summaryData$location <- as.factor(summaryData$location)
  summaryData$location <- factor(summaryData$location, levels = rev(levels(summaryData$location)))
  summaryData$first_observed <- as.factor(summaryData$first_observed)
  
  
  
  # Create plot
  
  if(relative == TRUE){
    position <- "fill"
    text <- paste0(summaryData$location, "<br>", summaryData$perc, "%")
  }else{
    position <- "stack"
    text <- paste0(summaryData$location, "<br>", summaryData$value)
  }
  
  pl <- ggplot(data = summaryData, aes(x = first_observed, y = value, fill = location, text = text)) +
    geom_bar(position = position, stat = "identity") 
  
  if(relative == TRUE){
    pl <- pl + scale_y_continuous(labels = percent_format())
  }
  
  pl_2 <- ggplotly(data = summaryData, pl,  width = width, height = height, tooltip ="text") %>% 
    layout(xaxis = list(title = x_lab, tickangle = "auto"), 
           yaxis = list(title = y_lab, tickformat = ",d"),
           margin = list(b = 80, t = 100), 
           barmode = ifelse(nlevels(summaryData$first_observed) == 1, "group", "stack"))
   
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  # Change variable name
  names(summaryData)[names(summaryData) == "value"] <- "number"
  names(summaryData)[names(summaryData) == "first_observed"] <- "year"
  names(summaryData)[names(summaryData) == "location"] <- "region of origin"
  
  return(list(static_plot = pl, interactive_plot = pl_2, data = summaryData))
  
}
