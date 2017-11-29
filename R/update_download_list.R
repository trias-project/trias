#' Update the list with all downloads
#'
#' This function opens a csv file containing all occurrence downloads from GBIF.
#' It updates the stauts of all downloads with status RUNNING.
#' In case download key is passed which is not present in the file it will be added as a new line. 
#'
#' @param file file containing all occurrence downloads from GBIF
#' @param download_to_add GBIF download key to be added in database
#' @param countries vector with countries
#' @return message with the performed updates
#'
#' @export
#' @importFrom rgbif occ_download_meta
update_download_list <- function(file, download_to_add, input_checklist, countries) {
  
  downloads <- read.table(file = file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  # downloadKey not present
  if (is.element(download_to_add, downloads$gbif_download_key) == FALSE) {
    metadata <- occ_download_meta(key = download_to_add)
    gbif_download_status <- metadata$status
    gbif_download_doi <- metadata$doi
    gbif_download_created <- metadata$created
    write.table(x = list(toString(download_to_add), input_checklist, countries, 
                         gbif_download_created, gbif_download_status, gbif_download_doi), file = file,
                append = TRUE, sep = ",", quote = FALSE, row.names = FALSE, col.names =! file.exists(file))
    print(paste0("gbif_download_Key ", download_to_add, 
                 " added to ", file, "; download status = ", gbif_download_status))
    #reload file
    downloads <- read.table(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  } else {
    print(paste0("gbif_download_Key ", download_to_add, " already present in ", file))
    }
  
  # check all downloads with status "PREPARING" or "RUNNING".
  changes <- FALSE
  for (i in 1:length(downloads)) {
    if (is.element(downloads$gbif_download_status[i], c("RUNNING","PREPARING"))) {
      metadata <- occ_download_meta(key = downloads$gbif_download_key[i])
      gbif_download_status <- metadata$status
      # In case status SUCCEEDED or FAILED, update it
      if (is.element(gbif_download_status, c("SUCCEEDED","FAILED"))) {
        if (is.element(gbif_download_status, c("SUCCEEDED"))) {
          downloads$gbif_download_status[i] <- "SUCCEEDED"
        } else {
          downloads$gbif_download_status[i] <- "FAILED"
        }
        print(paste0("gbif_download_Key ", 
                     downloads$gbif_download_key[i], ", status: ", gbif_download_status))
        changes <- TRUE
      }
    }
  }
  
  # if download status changes are detected, then csv is rewritten with the needed changes  
  if (!changes) {
    print("No changes in download status detected")
  } else {
    write.table(x = downloads, file = file, sep = ",", quote = FALSE, row.names = FALSE)
  }
  
}