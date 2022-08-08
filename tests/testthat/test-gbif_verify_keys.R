#' @importFrom purrr map_chr
#' 
context("test_gbif_verify_keys")

testthat::test_that("test several input types", {
  # input contains nothing
  keys_null1 <- list()
  testthat::expect_null(gbif_verify_keys(keys_null1))
  
  # input contains invalid taxon keys (they contain letters)
  keys_err1 <- c("1", "128", "120391203", "AE", "12k")
  testthat::expect_error(
    gbif_verify_keys(keys_err1),
    "Invalid keys: AE,12k ."
  )
  
  # input contains empty strings
  keys_err2 <- c("", "", "1")
  testthat::expect_error(
    gbif_verify_keys(keys_err2),
    "Invalid keys: \"\"\"\",\"\"\"\" ."
  )
  
  # input contains dates
  testthat::expect_error(
    gbif_verify_keys(as.Date("2018-01-01")),
    "keys should be a vector, a named list or a data.frame."
  )
  
  # input df doesn't contain column with name keys
  testthat::expect_error(gbif_verify_keys(data.frame(bad_col_name = 12)),
    paste(
      "Column with keys not found.",
      "Did you forget maybe to pass",
      "the right column name to col_keys?"
    ),
    fixed = TRUE
  )
})

testthat::test_that("output type and content", {
  
  # basic vector with input keys for tests
  keys1 <- c(
    "12323785387253", # is not a taxonkey
    "172331902", # not a taxonKey from Backbone, but kingdom Animalia from CoL
    "1000693", # is a synonym: Pterodina calcaris Langer, 1909,
    # Synonym of Testudinella parva (Ternetz, 1892)
    "1000310", # accepted taxon: Pyrococcus woesei Zillig, 1988
    NA, NA
  )
  
  # input is a df
  keys2 <- tibble(
    key = keys1,
    other = sample.int(40, size = length(keys1))
  )
  
  # input is a named list
  keys3 <- keys1
  names(keys3) <- purrr::map_chr(
    c(1:length(keys3)),
    ~ paste(sample(c(0:9, letters, LETTERS), 3),
            collapse = ""
    )
  )
  # input keys are numeric
  keys4 <- as.numeric(keys1)
  
  # generate outputs
  output1 <- gbif_verify_keys(keys1)
  output2 <- gbif_verify_keys(keys2)
  output3 <- gbif_verify_keys(keys3)
  output4 <- gbif_verify_keys(keys4)

  # output expected
  output_keys <- tibble(
    key = c(
      12323785387253, 172331902,
      1000693, 1000310
    ),
    is_taxonKey = c(FALSE, TRUE, TRUE, TRUE),
    is_from_gbif_backbone = c(NA, FALSE, TRUE, TRUE),
    is_synonym = c(NA, NA, TRUE, FALSE)
  )  
  
  # output type is "list"
  testthat::expect_type(output1, "list")
  testthat::expect_type(output2, "list")
  testthat::expect_type(output3, "list")
  testthat::expect_type(output4, "list")
  testthat::expect_null(gbif_verify_keys(keys = c(NA, NA)))
  testthat::expect_null(gbif_verify_keys(keys = c(NA_character_, NA_character_)))
  
  # output class is tibble data.frame
  testthat::expect_equal(class(output1), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(class(output2), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(class(output3), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(class(output2), c("tbl_df", "tbl", "data.frame"))


  # output content is the same for all kinds of inputs
  testthat::expect_true(nrow(output1) == nrow(output_keys))
  testthat::expect_true(nrow(output2) == nrow(output_keys))
  testthat::expect_true(nrow(output3) == nrow(output_keys))
  testthat::expect_true(nrow(output4) == nrow(output_keys))
  testthat::expect_true(nrow(gbif_verify_keys(keys = "1")) == 1)
  testthat::expect_equal(output1, output_keys)
  testthat::expect_equal(output2, output_keys)
  testthat::expect_equal(output3, output_keys)
  testthat::expect_equal(output3, output_keys)
  testthat::expect_equal(output4, output_keys)
})

testthat::test_that("function works even with duplicated taxon keys", {
  
  # basic vector with input keys for tests
  keys_with_duplicates <- c(
    "12323785387253", # is not a taxonkey
    "172331902", # not a taxonKey from Backbone, but kingdom Animalia from CoL
    "1000693", # is a synonym: Pterodina calcaris Langer, 1909,
    # Synonym of Testudinella parva (Ternetz, 1892)
    "1000310", # accepted taxon: Pyrococcus woesei Zillig, 1988
    "12323785387253", # duplicate
    "172331902", # duplicate
    "1000693", # duplicate
    "1000310" # duplicate
  )
  
  # generate output
  output_with_duplicates <- gbif_verify_keys(keys_with_duplicates)
  
  # expected output
  expected_output_with_duplicates <- 
    tibble(
      key = as.numeric(keys_with_duplicates),
      is_taxonKey = rep(c(FALSE, TRUE, TRUE, TRUE), 2),
      is_from_gbif_backbone = rep(c(NA, FALSE, TRUE, TRUE), 2),
      is_synonym = rep(c(NA, NA, TRUE, FALSE), 2)
    )
  
  testthat::expect_equal(output_with_duplicates, expected_output_with_duplicates)
})
