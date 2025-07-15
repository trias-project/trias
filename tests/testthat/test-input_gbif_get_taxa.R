context("input_gbif_get_taxa")


testthat::test_that("taxon_keys and checklist_keys cannot be both not NULL", {
  testthat::expect_error(
    gbif_get_taxa(
      taxon_keys = 3,
      checklist_keys = "../data/input/file.tsv"
    ),
    paste("Both taxon_keys and checklist_keys not NULL.",
      "You should choose one of the two!",
      sep = " "
    )
  )
  testthat::expect_error(
    gbif_get_taxa(
      taxon_keys = c(21212, 12423),
      checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42"
    ),
    paste("Both taxon_keys and checklist_keys not NULL.",
      "You should choose one of the two!",
      sep = " "
    )
  )
  testthat::expect_error(
    gbif_get_taxa(
      taxon_keys = list(2, 4),
      checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42"
    ),
    paste("Both taxon_keys and checklist_keys not NULL.",
      "You should choose one of the two!",
      sep = " "
    )
  )
})

testthat::test_that("taxon_keys class: character, integer or vector", {
  testthat::expect_error(
    gbif_get_taxa(taxon_keys = data.frame()),
    "taxon_keys should be a numeric, character or a vector."
  )
})

testthat::test_that("checklist_keys class: character, (character) vector", {
  testthat::expect_error(
    gbif_get_taxa(checklist_keys = data.frame()),
    "checklist_keys should be a character or a vector."
  )
  testthat::expect_error(
    gbif_get_taxa(
      checklist_keys =
        list(x = "e4746398-f7c4-47a1-a474-ae80a4f18e92")
    ),
    "checklist_keys should be a character or a vector."
  )
})

testthat::test_that("Limit is a number", {
  testthat::expect_error(
    gbif_get_taxa(
      checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
      limit = "10"
    ),
    "Limit has to be numeric."
  )
  testthat::expect_error(
    gbif_get_taxa(taxon_keys = 1, limit = -4),
    "Limit has to be a positive number."
  )
  testthat::expect_error(
    gbif_get_taxa(taxon_keys = 1, limit = 0),
    "Limit has to be a positive number."
  )
})

testthat::test_that("taxon_keys not found in GBIF, error from rgbif", {
  skip_if_offline()
  testthat::expect_error(
    gbif_get_taxa(taxon_keys = 103451),
    "Entity not found for uri: /",
    fixed = TRUE
  )
  testthat::expect_error(
    gbif_get_taxa(taxon_keys = "103451"),
    "Entity not found for uri: /",
    fixed = TRUE
  )
  testthat::expect_error(
    gbif_get_taxa(taxon_keys = c(2, 103451)),
    "Entity not found for uri: /",
    fixed = TRUE
  )
})

testthat::test_that("checklist_keys not found in GBIF, error from rgbif", {
  testthat::expect_error(
    gbif_get_taxa(checklist_keys = "falcon heavy"),
    "Invalid UUID string: falcon heavy"
  )
  testthat::expect_error(
    gbif_get_taxa(checklist_keys = c(
      "e4746398-f7c4-47a1-a474-ae80a4f18e92",
      "batman"
    )),
    "Invalid UUID string: batman"
  )
})
