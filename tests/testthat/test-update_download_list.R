testthat::test_that("Error returned if file does not contain the right columns", {
  directory_data <- "./data_test_upload_list/"
  gbif_downloads_filename <- "gbif_downloads_wrong_cols.tsv"
  gbif_downloads_file <- paste0(directory_data, gbif_downloads_filename)
  gbif_download_key_to_add <- "0003300-181003121212138"
  checklist <- "https://raw.githubusercontent.com/trias-project/pipeline/9f20d33a14696bc42d059a2e0f50fb11c4f97d35/data/input/occ_indicator_emerging_species.tsv"
  testthat::expect_error(
    update_download_list(
      file = gbif_downloads_file,
      download_to_add = gbif_download_key_to_add,
      input_checklist = checklist
    ),
    regexp = "The file should contain the following columns: gbif_download_key, input_checklist, gbif_download_created, gbif_download_status, gbif_download_doi"
  )
})

testthat::test_that("gbif download added correctly to list GBIF downloads and other status updated", {
  # Define file containing gbif downloads
  directory_data <- "./data_test_upload_list/"
  gbif_downloads_filename <- "gbif_downloads.tsv"
  gbif_downloads_file <- paste0(directory_data, gbif_downloads_filename)

  # Make a coy of it to replace original file at the end of the test
  gbif_downloads_filename_copy <- "gbif_downloads_copy.tsv"
  copy_file <- paste0(directory_data, gbif_downloads_filename_copy)
  file.copy(
    from = gbif_downloads_file,
    to = copy_file,
    overwrite = TRUE
  )

  input_df <- readr::read_tsv(gbif_downloads_file, trim_ws = TRUE,
                              na = "",
                              lazy = FALSE)

  # Define arguments related to new GBIF download
  gbif_download_key_to_add <- "0003300-181003121212138"
  checklist <- "https://raw.githubusercontent.com/trias-project/pipeline/9f20d33a14696bc42d059a2e0f50fb11c4f97d35/data/input/occ_indicator_emerging_species.tsv"


  # Apply update_download_list
  update_download_list(
    file = gbif_downloads_file,
    download_to_add = gbif_download_key_to_add,
    input_checklist = checklist
  )

  output_df <- readr::read_tsv(gbif_downloads_file, trim_ws = TRUE, na = "",
                               lazy = FALSE)

  # new download is added
  testthat::expect_equal(nrow(output_df), nrow(input_df) + 1)
  testthat::expect_equal(output_df$gbif_download_key[nrow(input_df) + 1], gbif_download_key_to_add)
  testthat::expect_equal(output_df$input_checklist[nrow(input_df) + 1], checklist)
  testthat::expect_equal(output_df$gbif_download_status[nrow(input_df) + 1], "SUCCEEDED")
  testthat::expect_equal(output_df$gbif_download_doi[nrow(input_df) + 1], "https://doi.org/10.15468/dl.5xygvn")

  # status previous download updated to SUCCEEDED
  testthat::expect_equal(output_df$gbif_download_status[nrow(input_df)], "SUCCEEDED")

  #' replace modified file with copy
  #' make a coy of it to replace original file at the end of the test
  file.copy(
    from = copy_file,
    to = gbif_downloads_file,
    overwrite = TRUE
  )

  #' remove copy
  file.remove(copy_file)
})
