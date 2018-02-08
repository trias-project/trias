context("output_get_taxa_limit_warnings")

testthat::test_that("checklist is character, limit higher than n. of records", {
  expect_warning(get_taxa(checklist_keys = "e2a2c3f4-60e5-4724-8bb8-4b0618fbd9df",
                          limit = 500),
                 "Dataset contains less records than limit.")
})

testthat::test_that("taxon_key is vector, limit higher than n. of records", {
  expect_warning(get_taxa(taxon_keys =  c(1,2,3,4,5),
                          limit = 500),
                 "Limit is higher than number of taxon keys.")
})

