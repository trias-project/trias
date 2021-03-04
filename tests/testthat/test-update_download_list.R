testthat::test_that("gbif download added correctly to list GBIF downloads and other status updated", {

  #' define file containing gbif downloads
  directory_data <- "./data_test_upload_list/"
  gbif_downloads_filename <- "gbif_downloads.tsv"
  gbif_downloads_file <- paste0(directory_data, gbif_downloads_filename)

  #' make a coy of it to replace original file at the end of the test
  gbif_downloads_filename_copy <- "gbif_downloads_copy.tsv"
  copy_file <- paste0(directory_data, gbif_downloads_filename_copy)
  file.copy(
    from = gbif_downloads_file,
    to = copy_file,
    overwrite = TRUE
  )

  input_df <- readr::read_tsv(gbif_downloads_file, trim_ws = TRUE, na = "")

  #' define arguments related to new GBIF download
  gbif_download_key_to_add <- "0003300-181003121212138"
  checklist <- "https://raw.githubusercontent.com/trias-project/pipeline/9f20d33a14696bc42d059a2e0f50fb11c4f97d35/data/input/occ_indicator_emerging_species.tsv"


  #' apply update_download_list
  update_download_list(
    file = gbif_downloads_file,
    download_to_add = gbif_download_key_to_add,
    input_checklist = checklist
  )

  output_df <- readr::read_tsv(gbif_downloads_file, trim_ws = TRUE, na = "")

  # new download is added
  expect_equal(nrow(output_df), nrow(input_df) + 1)
  expect_equal(output_df$gbif_download_key[nrow(input_df) + 1], gbif_download_key_to_add)
  expect_equal(output_df$input_checklist[nrow(input_df) + 1], checklist)
  expect_equal(output_df$gbif_download_status[nrow(input_df) + 1], "SUCCEEDED")
  expect_equal(output_df$gbif_download_doi[nrow(input_df) + 1], "https://doi.org/10.15468/dl.5xygvn")

  # status previous download updated to SUCCEEDED
  expect_equal(output_df$gbif_download_status[nrow(input_df)], "SUCCEEDED")

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
