#' Import GBIF or ad hoc checklist
#'
#' This function opens a checklist from GBIF and/or from a tsv file as R dataframes.
#'
#' @param checklist GBIF checklist datasetKey or tsv file path
#' @param tot_limit Maximum number of records to return for each dataset. 
#' Default NULL, that means all records of every dataset
#' @return A list with two elements: 
#' species, list with data from checklists
#' @export
checklist_import <- function(checklist, tot_limit = NULL, verbose = FALSE) {
  if (file.exists(checklist)) {
    cat("\n", paste0("Importing checklist from (tsv) file ", checklist))
    species <- read.table(file = checklist, header = TRUE, sep = "\t")
  } else {
    cat("\n", paste0("Importing GBIF checklist with datasetKey = ", checklist))
    species <- name_usage_paging(datasetKey = checklist, tot_limit = tot_limit, 
                                 verbose = verbose)
  }
  return(species)
}


#' Import GBIF or ad hoc checklists
#'
#' This function calls checklist_import for opening all checklists
#' 
#' @param checklists list containing all GBIF checklist datasetKey or tsv file paths
#' @param tot_limit number of records to import from each dataset. 
#' This function supports only default valuue NULL, that means importing all records.
#' @return dataframe
#' @export
#' @importFrom purrr map
checklists_import <- function(checklists, tot_limit = NULL, verbose = FALSE) {
  if (is.null(tot_limit)) {
    dataframes_taxonKeys <- purrr::map(checklists,checklist_import, verbose = verbose)
  }
  return(dataframes_taxonKeys)
}