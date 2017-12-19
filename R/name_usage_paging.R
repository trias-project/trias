#' Retrieve all records contained in a GBIF checklist
#'
#' This function retrieves all records contained in a GBIF checklist.
#' It implements paging by using iteratively rgbif fucntion name_usage.
#'
#'
#' @param datasetKey (character) Filters by the GBIF dataset’s key (a uuid)
#' @param data (character) Specify an option to select what data is returned. 
#' See name_usage documentation.
#' @param offset Record number to start at. Default: 0 (as in name_usage) 
#' Use in combination with limit to page through results
#' @param tot_limit Number of records to return. 
#' Default NULL (return all records)
#' @return A list of length two. 
#' The first element is metadata. The second is a data.frame (as in name_usage)
#' @export
#' @importFrom rgbif name_usage
#' @importFrom purrr modify
#' @importFrom tibble as_tibble
name_usage_paging <- function(
  datasetKey, data = "all", 
  offset = 0, tot_limit = NULL, verbose = FALSE) {
  page_limit <- 500
  cat("\n")
  if ((!isTRUE(is.null(tot_limit))) && (tot_limit <=  page_limit)) {
    page_limit <- tot_limit
    cat(paste0("page_limit= ", page_limit))
  }
  start <- offset
  if (isTRUE(verbose)) {
    cat("\r", paste0("retrieving records: (", 
                    start, ", ", start + page_limit, ")"))
    flush.console()
  }
  species_to_add <- name_usage(
    datasetKey = datasetKey, data = data, start = start, limit = page_limit)
  species <- species_to_add
  species$data$issues <- NULL # issues not needed
  species$data <- modify(species$data, unlist)
  species$data <- data.frame(species$data)
  while ((nrow(species_to_add$data) == page_limit)  && 
         ((isTRUE(is.null(tot_limit))) || (isTRUE(nrow(species$data) < tot_limit)))) {
    start <- start + page_limit
    if ((!isTRUE(is.null(tot_limit))) && (start + page_limit > offset + tot_limit)) {
      last_limit <- offset + tot_limit - start
      if (isTRUE(verbose)) {
        cat("\r", paste0("retrieving records: (",
                        start, ", ", start + last_limit, ")"))
        flush.console()
      }
      species_to_add <- name_usage(
        datasetKey = datasetKey, data = data, start = start, limit = last_limit)
    } else {
      if (isTRUE(verbose)) {
        cat("\r", paste0("retrieving records: (",
                        start, ", ", start + page_limit, ")"))
        flush.console()
      }
      species_to_add <- name_usage(
        datasetKey = datasetKey, data = data, start = start, limit = page_limit)
    }
    species_to_add$data$issues <- NULL # issues not needed
    species_to_add$data <- modify(species_to_add$data, unlist)
    species_to_add$data <- data.frame(species_to_add$data)
    dfs <- add_type(df1 = species$data, df2 = species_to_add$data, verbose = FALSE)
    species$data <- dfs$df1
    species_to_add$data <- dfs$df2
    diff_type <- check_type(species$data,species_to_add$data)   
    new_df <- coerce_datatypes(df1 = species$data, df2 = species_to_add$data, diff_type = diff_type)
    species$data <- rbind(
      new_df$df1[ , order(names(new_df$df1))], new_df$df2[ , order(names(new_df$df2))])
  }
  as_tibble(species$data)
  species$meta$offset <- offset
  species$meta$limit <- nrow(species$data)
  species$meta$endOfRecords <- species_to_add$meta$endOfRecords
  as_tibble(species$meta)
  return(species)
}



# dk <- "9ff7d317-609b-4c08-bd86-3bc404b77c42"
# dk <- "70ad86f4-6be8-4e5b-aee8-61896310fb18"
# test <- name_usage_paging(datasetKey = dk, offset = 0, tot_limit = NULL, verbose = TRUE)
# test2 <- name_usage_paging(datasetKey = dk, offset = 200, tot_limit = 200, verbose = TRUE)
# test_no_paging <- name_usage(datasetKey = dk, data = "all", start = 0, limit = 5000)
# test_no_paging$data <- modify(test_no_paging$data, unlist)
# test_no_paging$data <- data.frame(test_no_paging$data)
# 
