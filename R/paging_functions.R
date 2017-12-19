#' Function containing binary coercions rules
#'
#' This function contains coercion rules and will be constantly updated.
#'
#'
#' @param v1 (vector) First vector
#' @param v2 (vector) Second vector
#' @return A vector containing the modified vectors
#' @export
coerce_table <- function(v1, v2) {
  if ((is.logical(typeof(v1))) ||  (is.logical(typeof(v2)))) {
    if (is.logical(typeof(v1))) {
      v1 <- as(v1, typeof(v2)) 
    } else{
      v2 <- as(v2, typeof(v1)) 
    }
  }
  return(c(v1, v2))
}


#' Function containing binary coercions rules
#'
#' This function contains coercion rules and will be constantly updated.
#'
#'
#' @param df1 (dataframe) First dataframe
#' @param df2 (dataframe) Second dataframe
#' @param diff_type (vector) TRUE for columns where type coercion is needed
#' @return A list with two dataframes
#' @export
coerce_datatypes <- function(df1, df2, diff_type) {
  for (name in names(diff_type)) {
    if (diff_type[[name]] == TRUE) {
      newvalues <- coerce_table(df1[[name]], df2[[name]])
      df1[[name]] <- newvalues[1]
      df2[[name]] <- newvalues[2]
    }
  }
  return(list(df1 = df1, df2 = df2))
}


#' Function checking columns types of two dataframes
#'
#' This function checks types of corresponding columns from two dataframes.
#' TRUE if they are different, FALSE otherwise
#'
#' @param v1 (dataframe) First dataframe
#' @param v2 (dataframe) Second dataframe
#' @return A logical vector
#' @export
check_type <- function(df1, df2) {
  diff_type.names <- names(df1)
  diff_type <- sapply(diff_type.names,function(x) FALSE)
  for (name in names(df1)) {
    if (!isTRUE(name %in% names(df2))) {
      diff_type[[name]] <- TRUE
    }
  }
  return(diff_type)
}


#' Function comparing columns of two dataframes and adding missing columns where needed.
#'
#' This function adds columns to two dataframes based on columns of both
#'
#' @param df1 (dataframe) First dataframe 
#' @param df2 (dataframe) Second dataframe
#' @return list with two dataframes
#' @export
add_type <- function(df1, df2, verbose = FALSE) {
  for (name in names(df2)) {
    if (!name %in% names(df1)) {
      if (isTRUE(verbose)) print(paste0("Add ", name))
      df1[[name]] <- unlist(map(.x = logical(length = nrow(df1)), ~ NA))
      v <- coerce_table(df1[[name]], df2[[name]])
      df1[[name]] <- v[1]
      df2[[name]] <- v[2]
    }
  }
  for (name in names(df1)) {
    if (!name %in% names(df2)) {
      if (isTRUE(verbose)) print(paste0("Add ", name))
      df2[[name]] <- unlist(map(.x = logical(length = nrow(df2)), ~ NA))
      v <- coerce_table(df1[[name]], df2[[name]])
      df1[[name]] <- v[1]
      df2[[name]] <- v[2]
    }
  }
  return(list(df1 = df1, df2 = df2))
}
