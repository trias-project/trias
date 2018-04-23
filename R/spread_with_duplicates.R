#' Spread a key-value pair across multiple columns in presence of duplicates
#' 
#' @param .data A dataframe
#' @param key,value Column names or positions
#' @param fill If set, missing values will be replaced with this value. 
#' @examples 
#' test1 <- data.frame(taxonKey = c(1, 1, 1, 1),	
#'                 key = c("A", "B", "C", "C"),
#'                 value = c("R", "S", "T", "X"),
#'                 stringsAsFactors = FALSE)
#' test1 %>% spread_with_duplicates(key, value, by = "taxonKey")
#' # with NAs
#' test2 <- data.frame(taxonKey = c(1, 1, 1, 2),	
#'                 key = c("A", C", "C", "A"),
#'                 value = c("R", "T", "X", "R"),
#'                 stringsAsFactors = FALSE)
#' test2 %>% spread_with_duplicates(key, value, by = "taxonKey")
#' test2 %>% spread_with_duplicates(key, value, by = "taxonKey", fill = 0)
spread_with_duplicates <- function(.df, key, value, by, fill = NA){
  key_var <- vars_pull(names(.df), !! enquo(key))
  col <- .df[key_var] %>% pull() %>% unique()
  .df <- map(col, function(x) .df %>% filter(key == x)) %>%
    map2(col, ~ change_colname(.x, .y, value)) %>%
    reduce(full_join, by = by)
  if (!is.na(fill)){
    .df <- .df %>% mutate_all(funs(replace(., is.na(.), fill)))
  }
  return(.df)
}

change_colname <- function(.df, new_colname, value){
  .df %>% 
    rename(!!new_colname := value) %>%
    select(-key)
}
