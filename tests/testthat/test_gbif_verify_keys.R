#' @importFrom purrr map_chr
context("test_gbif_verify_keys")

# input is a vector
keys1 = c("12323785387253", # is not a taxonkey
          "128545334", # not a taxonKey from Backbone, but Euglenes nitidifrons 
          # (Thomson, 1886) in "Checklist of Danish Beetles (Coleoptera)"
          "1000693", # is a synonym: Pterodina calcaris Langer, 1909,
          # Synonym of Testudinella parva (Ternetz, 1892)
          "1000310", # accepted taxon: Pyrococcus woesei Zillig, 1988
          NA, NA)
# input is a df
keys2 <- data.frame(key = keys1, 
                    other = sample.int(40, size = length(keys1)),
                    stringsAsFactors = FALSE)
# input is a named list 
keys3 <- keys1
names(keys3) <- purrr::map_chr(c(1:length(keys3)), 
                               ~paste(sample(c(0:9, letters, LETTERS),3), 
                                      collapse=""))
# input keys are numeric
keys4 <- as.numeric(keys1)

# same output expected
output_keys <- data.frame(key = c(12323785387253, 128545334, 
                                   1000693),
                           is_taxonKey = c(FALSE, TRUE, TRUE), 
                           is_from_gbif_backbone = c(NA, FALSE, TRUE),
                           is_synonym = c(NA, NA, TRUE),
                           stringsAsFactors = FALSE)

output1 <- gbif_verify_keys(keys1)
output2 <- gbif_verify_keys(keys2)
output3 <- gbif_verify_keys(keys3)
output4 <- gbif_verify_keys(keys4)

testthat::test_that("test several input types", {
  expect_error(gbif_verify_keys(as.Date("2018-01-01")),
               "keys should be a vector, a named list or a data.frame.")
  expect_error(gbif_verify_keys(data.frame(bad_col_name = 12)),
               "Columns 'key' not found.")
})

testthat::test_that("output type", {
  expect_type(output1, "list")
  expect_type(output2, "list")
  expect_type(output3, "list")
  expect_null(gbif_verify_keys(keys = c(NA,NA)))
  expect_null(gbif_verify_keys(keys = c(NA_character_,NA_character_)))
})

testthat::test_that("output content", {
  expect_true(nrow(output1) == 3)
  expect_true(nrow(output2) == 3)
  expect_true(nrow(output3) == 3)
  expect_true(nrow(gbif_verify_keys(keys = "1")) == 0)
  expect_equal(output1, output_keys)
  expect_equal(output2, output1)
  expect_equal(output3, output2)
  expect_equal(output3, output2)
  expect_equal(output4, output3)
})
