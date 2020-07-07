library(readr)
data_input_checklist_indicators <- read_delim("https://raw.githubusercontent.com/trias-project/indicators/master/data/interim/data_input_checklist_indicators.tsv", 
                                              "\t", escape_double = FALSE, trim_ws = TRUE)

source("./R/indicator_native_range_year.r")

countYearNativerange(data_input_checklist_indicators, jaartallen = c(1950:2019), type = "native_range")