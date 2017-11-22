source("R/trias_utilities.R")
#' Trigger a download from GBIF
#'
#' This function uses GBIF to trigger a download of a set of  features for a list of species.
#' @param taxa Path to a csv file with informations about species
#' @param country Country (default: BE)
#' @param output Path to output csv file
#'
#' @export
gbif_download <- function(taxa="https://raw.githubusercontent.com/trias-project/alien-plants-belgium/afbd2805de77afd79fb74669c403d40f1416661b/data/processed/taxon.csv",
                          country="BE",
                          output="./data/output/gbif_downloads.csv") {
  # Check the input file
  if (identical(extension(taxa)[1],"csv") == FALSE) {
    stop("The input has to be a csv file.")
  }
  if (grepl("https",taxa) == TRUE) {
    if (http_error(taxa) == TRUE)  {
      stop("The file doesn't exists.")
    }
  }

  # Check country code following ISO_3166_1 Alpha_2
  if (is.element(country,ISOcodes::ISO_3166_1$Alpha_2) == FALSE) {
    stop("The country code is not a valid ISO 3166-1 alpha-2 code.")
  }

  # Check the output csv file.
  if (file.exists(output)) {
    warning("Output file already exists.")
  }
  if (file.exists(output)){
    proceed <- readline(prompt=cat("Do you want to overwrite it? (Y/N) Enter to escape without saving."))
    if (toupper(proceed[1]) == "N") {
      output <- readline(prompt="Insert the output filename (e.g. \"./data/input/output.csv\"): ")
    }
    else {
      if (toupper(proceed[1]) == "") {
        print("Process terminated.")
      }
    }
  }

  # download the file with the list of species
  destfile <- paste("./data/input",split_path(taxa)[[1]],sep="/")
  download.file(taxa,destfile,method="auto")
  species_table <- read.table(destfile, header = TRUE, sep = ",")

  # check whether the column gbif_nubKey is present in the input file
  if ("gbif_nubKey" %in% colnames(species_table) == FALSE) {
    stop("gbif_nubKey column not present in the csv file")
  }
}
