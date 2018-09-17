#' Spread key-value pairs across multiple columns.
#'
#' @description This function spread one or more key-value pairs across multiple
#'   columns. If only one value exists per key, then this functions returns the
#'   same output as \code{\link[tidyr]{spread}}. Otherwise, aggregation function
#'   \code{aggfunc} is applied. If no function is passed, then the key-value(s)
#'   are spread on multiple rows.

#' @param data A dataframe.
#' @param key,value Column names or positions. This is passed to
#'   \code{\link[tidyselect]{vars_pull}}. These arguments are passed by
#'   expression and support \code{\link[rlang]{quasiquotation}} (you can unquote
#'   column names or column positions).
#' @param fill If set, missing values will be replaced with this value.
#' @param convert If \code{TRUE}, \code{\link[utils]{type.convert}} with
#'   \code{asis = TRUE} will be run on each of the new columns. This is useful
#'   if the value column was a mix of variables that was coerced to a string. If
#'   the class of the value column was factor or date, note that will not be
#'   true of the new columns that are produced, which are coerced to character
#'   before type conversion.
#' @param drop If \code{FALSE}, will keep factor levels that don't appear in the
#'   data, filling in missing combinations with \code{fill}.
#' @param sep If \code{NULL}, the column names will be taken from the values of
#'   \code{key} variable. If non-\code{NULL}, the column names will be given by
#'   "<key_name><sep><key_value>".
#' @param aggfunc Aggregation function. Default: NULL (keep all values on
#'   different rows).
#' @param ... Any parameter needed to apply \code{aggfunc}.
#'
#' @return A data.frame.
#'
#' @export
#'
#' @importFrom purrr map map2 reduce compact
#' @importFrom rlang sym syms is_character !!! !! :=
#' @importFrom dplyr summarize funs rename mutate_all mutate_at filter full_join
#'   pull %>% one_of group_by vars
#' @importFrom tidyselect vars_pull enquo
#' @importFrom tidyr complete_
#' @importFrom utils type.convert
#'
#' @examples
#' \dontrun{
#' test1 <- data.frame(
#' col1 = c(1, 1, 1, 1),
#' col2 = c("H", "H", "H", "H"),
#' key = c("A", "B", "C", "C"),
#' value = c("R", "S", "T", "X"),
#' stringsAsFactors = FALSE
#' )
#' spread_with_multiple_values(test1, key, value)
#' spread_with_multiple_values(test1, 3, 4)
#' spread_with_multiple_values(test1, -2, -1)
#' spread_with_multiple_values(test1, "key", "value")
#'
#' # with NAs
#' test2 <- data.frame(
#' col1 = c(1, 1, 1, 2),
#' key = c("A", "C", "C", "A"),
#' value = c("R", "T", "X", "R"),
#' stringsAsFactors = FALSE
#' )
#' spread_with_multiple_values(test2, key, value)
#' spread_with_multiple_values(test2, key, value, fill = "No idea")
#'
#' # apply aggregate function
#' test3 <- data.frame(
#' col1 = c(1,1,1,1),
#'   col2  = c("H", "H", "H", "H"),
#'   key = c("A", "B", "C", "C"),
#'   value = c(2, 3, 1, 8),
#'   stringsAsFactors = FALSE
#' )
#' spread_with_multiple_values(test1, key, value, aggfunc = str_c, collapse = "-")
#' spread_with_multiple_values(test3, key, value, aggfunc = min)
#' spread_with_multiple_values(test3, key, value, aggfunc = mean)
#'
#' # same output of tidyr::spread() if one value per key and no aggfunc
#' library(dplyr)
#' stocks <- data.frame(
#'  time = as.Date('2009-01-01') + 0:9,
#'  X = rnorm(10, 0, 1),
#'  Y = rnorm(10, 0, 2),
#'  Z = rnorm(10, 0, 4)
#' )
#' stocksm <- stocks %>% gather(stock, price, -time)
#' stocksm %>% spread_with_multiple_values(stock, price)
#' stocksm %>% tidyr::spread(stock, price)
#' stocksm %>% spread_with_multiple_values(time, price)
#' stocksm %>% tidyr::spread(time, price)
#'
#' # Use 'convert = TRUE' to produce variables of mixed type
#' df <- data.frame(row = rep(c(1, 51), each = 3),
#'                  var = c("Sepal.Length", "Species", "Species_num"),
#'                  value = c(5.1, "setosa", 1, 7.0, "versicolor", 2))
#' df %>% spread_with_multiple_values(var, value) %>% str
#' df %>% tidyr::spread(var, value, convert = TRUE) %>% str
#'
#' # Use sep non-NULL
#' spread_with_multiple_values(test2, key, value, sep = "_var_")
#' spread_with_multiple_values(df, var, value, sep = "_")
#' }
spread_with_multiple_values <- function(data, key, value, fill = NA, 
                                   convert = FALSE, drop = TRUE,
                                   sep = NULL, aggfunc = NULL, ...) {
  args = list(...)
  key_var <- vars_pull(names(data), !! enquo(key))
  value_var <- vars_pull(names(data), !! enquo(value))
  by = colnames(data)[which(!colnames(data) %in% c(key_var,value_var))]
  col <- data %>% 
    pull(key_var) %>% 
    unique() %>%
    as.character()
  
  data <- map(
    col, 
    function(x) data %>% 
      filter(!! sym(key_var) == x)
  ) %>%
    map2(col, ~ change_colname(.x, .y, value_var, key_var)) %>%
    map2(col, ~ apply_aggfunc(.x, .y,
                              group_by_col = by,
                              aggfunc = aggfunc,
                              args)) %>%
    map2(col, ~ apply_convert(.x, .y, convert)) %>%
    map2(col, ~ apply_sep(.x, .y, key_var, sep)) %>%
    reduce(full_join, by = by)
  
  if (!drop) {
    data <- data %>% complete_(by)
  }
  
  if (!is.na(fill)){
    key_value_cols <- colnames(data)[which(!colnames(data) %in% by)]
    data <- data %>% mutate_at(vars(one_of(key_value_cols)),
                               funs(replace(., is.na(.), fill)))
  }
  
  return(data)
}

change_colname <- function(data, new_col, value, old_col) {
  data %>% 
    rename(!! as.character(new_col) := !!value) %>%
    select(-one_of(old_col))
}


apply_aggfunc <- function(data, col_name, group_by_col,  aggfunc, args) {
  if (is.function(aggfunc)) {
    data <- data %>%
      group_by(!!! syms(group_by_col)) %>% 
      summarize(
        !! col_name := do.call(
          aggfunc, args = c(list(!! sym(col_name)), args) %>% compact)
      ) %>% ungroup()
  } else {
    data
  }
}

apply_convert <- function(data, col_name, convert){
  values <- data[[col_name]]
  if (convert & !is_character(values)) {
    values <- as.character(values)
    values <- type.convert(values, as.is = TRUE)
  }
  data <- data %>% mutate(!! col_name := values)
}

apply_sep <- function(data, new_col, old_col, sep) {
  if (!is.null(sep)) {
    data %>% 
      rename(!!str_c(as.character(old_col), as.character(new_col), 
                     sep = sep) := !! as.character(new_col))
  } else {
    data
  }
}
