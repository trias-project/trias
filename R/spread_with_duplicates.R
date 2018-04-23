#' Spread a key-value pair across multiple columns in presence of duplicates
#' 
#' @param .data A dataframe
#' @param key,value Column names or positions
#' @param fill If set, missing values will be replaced with this value. 
#' @return A data.frame
#' @examples 
#' test1 <- data.frame(taxonKey = c(1, 1, 1, 1),	
#'                 key = c("A", "B", "C", "C"),
#'                 value = c("R", "S", "T", "X"),
#'                 stringsAsFactors = FALSE)
#' spread_with_duplicates(test1, key, value, by = "taxonKey")
#' # with NAs
#' test2 <- data.frame(taxonKey = c(1, 1, 1, 2),	
#'                 key = c("A", "C", "C", "A"),
#'                 value = c("R", "T", "X", "R"),
#'                 stringsAsFactors = FALSE)
#' spread_with_duplicates(test2, key, value, by = "taxonKey")
#' spread_with_duplicates(test2, key, value, by = "taxonKey", fill = 0)
#' @export
#' @importFrom purrr map map2 reduce
#' @importFrom dplyr mutate_all filter full_join pull
#' @importFrom magrittr %>%
#' @importFrom tidyselect vars_pull enquo
spread_with_duplicates <- function(.data, key, value, by, fill = NA){
  key_var <- vars_pull(names(.data), !! enquo(key))
  col <- .data %>% 
    pull(key_var) %>% 
    unique()
  .data <- map(col, function(x) .data %>% filter(key == x)) %>%
    map2(col, ~ change_colname(.x, .y, value)) %>%
    reduce(full_join, by = by)
  if (!is.na(fill)){
    .data <- .data %>% mutate_all(funs(replace(., is.na(.), fill)))
  }
  return(.data)
}

change_colname <- function(.data, new_colname, value){
  .data %>% 
    rename(!!new_colname := value) %>%
    select(-key)
}
