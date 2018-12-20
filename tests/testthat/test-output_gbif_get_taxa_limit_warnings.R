context("output_get_taxa_limit_warnings")

testthat::test_that("checklist is character, limit higher than n. of records", {
  expect_warning(
    gbif_get_taxa(
      checklist_keys = "e2a2c3f4-60e5-4724-8bb8-4b0618fbd9df",
      limit = 500
    ),
    "Dataset contains less records than limit."
  )
})

testthat::test_that("checklist is vector, limit higher than n. of records in all datasets", {
  expect_warning(
    gbif_get_taxa(
      checklist_keys = c(
        "46261ec5-38e8-44c9-b8e9-edaddf99fa29",
        "e4746398-f7c4-47a1-a474-ae80a4f18e92"
      ),
      limit = 5000
    ),
    "One of the datasets contains less records than limit."
  )
})

testthat::test_that(paste0(
  "checklist is vector, limit higher than n. of records",
  "in one of the datasets"
), {
  expect_warning(
    gbif_get_taxa(
      checklist_keys = c(
        "68f76539-8e5c-4967-a480-14d2d8e46637",
        "b3fa7329-a002-4243-a7a7-cd066092c9a6"
      ),
      limit = 70
    ),
    "One of the datasets contains less records than limit."
  )
})


testthat::test_that("taxon_key is vector, limit higher than n. of records", {
  expect_warning(
    gbif_get_taxa(
      taxon_keys = c(1, 2, 3, 4, 5),
      limit = 500
    ),
    "Limit is higher than number of taxon keys."
  )
})
