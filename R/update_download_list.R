#' Update the list with all downloads
#'
#' This function opens a (tab-separated) text file containing all occurrence
#' downloads from GBIF and updates the status of all downloads with status
#' `RUNNING` or `PREPARING`. If the specified download is not present it will be add.
#'
#' If a download key is passed which is not present in the file it will be added
#' as a new line.
#'
#' @param file text file (tab separated) containing all occurrence downloads
#'   from GBIF. File should contain the following columns:
#' - `gbif_download_key`: GBIF download keys, e.g. 0012078-181003121212138
#' - `input_checklist`: filename or link to readable text file containing the
#' list of queried taxa
#' - `gbif_download_created`: datetime of the GBIF download creation as
#' returned by `rgbif` function  `occ_download_meta`
#' - `gbif_download_status`: status of the GBIF download as returned by as
#' returned by `rgbif` function  `occ_download_meta`, e.g. `RUNNING`,
#' `PREPARING`, `SUCCEDEED`
#' - `gbif_download_doi`: DOI link as returned by as returned by `rgbif`
#' function  `occ_download_meta` (e.g. `10.15468/dl.kg0uxf`) + `url_doi_base`
#' value (see below)
#' @param download_to_add character. A GBIF download key to be added to file.
#' @param input_checklist text file with taxon keys whose occurrences you want
#'   to download
#' @param url_doi_base character. doi base URL; `url_doi_base` + doi form a
#'   link to a page with download information. Default: "https://doi.org/".
#' @return message with the performed updates
#'
#' @family download functions
#' @export
update_download_list <- function(file, download_to_add, input_checklist,
                                 url_doi_base = "https://doi.org/") {
  downloads <- readr::read_tsv(file,
    trim_ws = TRUE,
    na = "",
    lazy = FALSE
  )
  # downloadKey not present
  if (is.element(download_to_add, downloads$gbif_download_key) == FALSE) {
    metadata <- rgbif::occ_download_meta(key = download_to_add)
    gbif_download_status <- metadata$status
    gbif_download_doi <- stringr::str_c(url_doi_base, metadata$doi)
    print(gbif_download_doi)
    gbif_download_created <- metadata$created
    readr::write_tsv(
      x = as.data.frame(list(
        gbif_download_key = toString(download_to_add),
        input_checklist = input_checklist,
        gbif_download_created = gbif_download_created,
        gbif_download_status = gbif_download_status,
        gbif_download_doi = gbif_download_doi
      )),
      file = file,
      na = "",
      append = TRUE,
      col_names = !file.exists(file),
      escape = "none"
    )
    print(paste(
      "gbif_download_Key", download_to_add,
      "added to", file, "; download status =", gbif_download_status
    ))
    # reload file
    downloads <- readr::read_tsv(file, na = "", lazy = FALSE)
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
    readr::write_tsv(
      x = downloads,
      file = file,
      na = "",
      escape = "none"
    )
  }
}
