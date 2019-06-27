#' Update the list with all downloads
#'
#' This function opens a (tab-separated) text file containing all occurrence
#' downloads from GBIF and updates the status of all downloads with status
#' \code{RUNNING} or \code{PREPARING}. If the specifid download is not present it will be add.
#'
#' If a download key is passed which is not present in the file it will be added
#' as a new line.
#'
#' @param file text file containing all occurrence downloads from GBIF.
#' @param download_to_add character. A GBIF download key to be added to file.
#' @param input_checklist text file with taxon keys whose occurrences you want
#'   to download.
#' @param url_doi_base character. doi base URL; \code{url_doi_base} + doi form a
#'   link to a page with download information. Default: "https://doi.org/".
#' @return message with the performed updates
#'
#' @export
#' @importFrom rgbif occ_download_meta
#' @importFrom readr read_delim
#' @importFrom stringr str_c
#' @importFrom utils read.table write.table
update_download_list <- function(file, download_to_add, input_checklist,
                                 url_doi_base = "https://doi.org/") {
  downloads <- readr::read_delim(file, "\t",
    escape_double = FALSE, trim_ws = TRUE
  )
  # downloadKey not present
  if (is.element(download_to_add, downloads$gbif_download_key) == FALSE) {
    metadata <- rgbif::occ_download_meta(key = download_to_add)
    gbif_download_status <- metadata$status
    gbif_download_doi <- stringr::str_c(url_doi_base, metadata$doi)
    print(gbif_download_doi)
    gbif_download_created <- metadata$created
    write.table(
      x = list(
        toString(download_to_add), input_checklist,
        gbif_download_created, gbif_download_status,
        gbif_download_doi
      ),
      file = file,
      append = TRUE, sep = "\t", quote = FALSE,
      row.names = FALSE, col.names = !file.exists(file)
    )
    print(paste(
      "gbif_download_Key", download_to_add,
      "added to", file, "; download status =", gbif_download_status
    ))
    # reload file
    downloads <- read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  } else {
    print(paste("gbif_download_Key", download_to_add, "already present in", file))
  }
  # check all downloads with status "PREPARING" or "RUNNING".
  changes <- FALSE
  for (i in 1:nrow(downloads)) {
    if (downloads$gbif_download_status[i] %in% c("RUNNING", "PREPARING")) {
      metadata <- rgbif::occ_download_meta(key = downloads$gbif_download_key[i])
      gbif_download_status <- metadata$status
      # status is SUCCEEDED or FAILED
      if (gbif_download_status %in% c("SUCCEEDED", "FAILED")) {
        if (gbif_download_status == "SUCCEEDED") {
          downloads$gbif_download_status[i] <- "SUCCEEDED"
        } else {
          downloads$gbif_download_status[i] <- "FAILED"
        }
        print(paste(
          "gbif_download_Key", downloads$gbif_download_key[i],
          ", status:", gbif_download_status
        ))
        changes <- TRUE
      }
    }
  }
  # if download status changes detected, rewrite csv with changes
  if (!changes) {
    print("No changes in download status detected")
  } else {
    write.table(
      x = downloads, file = file, sep = "\t",
      quote = FALSE, row.names = FALSE
    )
  }
}
