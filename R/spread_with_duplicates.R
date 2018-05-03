#' Spread a key-value pair across multiple columns in presence of duplicates
#' 
#' @param data A dataframe.
#' @param key,value Column names or positions.
#' @param fill If set, missing values will be replaced with this value.
#' @param aggfunc Aggregation function. 
#' Default: NA (keep dulicates)
#' @return A data.frame
#' @examples 
#' test1 <- data.frame(col1 = c(1, 1, 1, 1),	
#'                 col2 = c("H", "H", "H", "H"),
#'                 key = c("A", "B", "C", "C"),
#'                 value = c("R", "S", "T", "X"),
#'                 stringsAsFactors = FALSE)
#' spread_with_duplicates(test1, key, value)
#' spread_with_duplicates(test1, 3, 4)
#' spread_with_duplicates(test1, -2, -1)
#' 
#' # with NAs
#' test2 <- data.frame(col1 = c(1, 1, 1, 2),	
#'                 key = c("A", "C", "C", "A"),
#'                 value = c("R", "T", "X", "R"),
#'                 stringsAsFactors = FALSE)
#' spread_with_duplicates(test2, key, value)
#' spread_with_duplicates(test2, key, value, fill = "No idea")
#' 
#' # apply aggregate function
#' test3 <- data.frame(col1 = c(1,1,1,1),
#'   col2  = c("H", "H", "H", "H"),
#'   key = c("A", "B", "C", "C"),
#'   value = c(2, 3, 1, 8),
#'   stringsAsFactors = FALSE)
#' spread_with_duplicates(test1, key, value, aggfunc = str_c, collapse = "-")
#' spread_with_duplicates(test2, key, value, aggfunc = min)
#' spread_with_duplicates(test2, key, value, aggfunc = mean)
#' @export
#' @importFrom purrr map map2 reduce
#' @importFrom rlang sym
#' @importFrom dplyr mutate_all filter full_join pull %>% rename one_of
#' @importFrom tidyselect vars_pull enquo
spread_with_duplicates <- function(data, key, value, fill = NA, 
                                   aggfunc = NA, ...){
  args = list(...)
  key_var <- vars_pull(names(data), !! enquo(key))
  value_var <- vars_pull(names(data), !! enquo(value))
  by = colnames(data)[which(!colnames(data) %in% c(key_var,value_var))]
  col <- data %>% 
    pull(key_var) %>% 
    unique()
  data <- map(
    col, 
    function(x) data %>% 
      filter(!! sym(key_var) == x)) %>%
    map2(col, ~ change_colname(.x, .y, value_var, key_var))  %>%
    map2(col, ~ apply_aggfunc(.x, .y,  aggfunc = aggfunc, args)) %>%
    reduce(full_join, by = by)
  data
  if (!is.na(fill)){
    data <- data %>% mutate_all(funs(replace(., is.na(.), fill)))
  }
  return(data)
}

change_colname <- function(data, new_col, value, old_col){
  data %>% 
    rename(!!new_col := !!value) %>%
    select(-one_of(old_col))
}

apply_aggfunc <- function(data, col_name,  aggfunc, args){
  if (is.function(aggfunc)) {
    args[["x"]] <- data %>% pull(!! quo(col_name))
    aggregated_value <- do.call(aggfunc, args = args)
    data %>% mutate(!!col_name := aggregated_value) %>%
      distinct()
  } else {
    data
  }
}
