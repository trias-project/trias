context("input_get_taxa")


testthat::test_that("taxon_keys and checklist_keys cannot be both not NULL", {
  expect_error(get_taxa(taxon_keys = 3, 
                        checklist_keys = "../data/input/file.tsv"), 
               paste("Both taxon_keys and checklist_keys not NULL.",
                     "You should choose one of the two!", sep = " "))
  expect_error(get_taxa(taxon_keys = c(21212,12423), 
                        checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42"),
               paste("Both taxon_keys and checklist_keys not NULL.",
                     "You should choose one of the two!", sep = " "))
  expect_error(get_taxa(taxon_keys = list(2,4), 
                        checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42"), 
               paste("Both taxon_keys and checklist_keys not NULL.",
                     "You should choose one of the two!", sep = " "))
})

testthat::test_that("taxon_keys is a list: output of name_usage(key = ...)", {
  expect_error(get_taxa(list(2)),
               "List is corrupted: meta and data are both expected and nothing else.")
  expect_error(get_taxa(list(meta = data.frame())),
               "List is corrupted: meta and data are both expected and nothing else.")
  expect_error(get_taxa(list(data = data.frame())),
               "List is corrupted: meta and data are both expected and nothing else.")
  expect_error(get_taxa(list(meta = data.frame(),
                             data = tibble::tibble(key = 4, batman = "batman"))),
               "One or more attributes don't match API terms.")
  })

testthat::test_that(paste("taxon_keys is a tibble data.frame: output of",
                          "name_usage(key = ...)$data", sep = " "), {
  expect_error(get_taxa(taxon_keys = tibble::tibble(key = 1, batman = "batman")),
    "One or more attributes don't match API terms.")
  })


testthat::test_that("checklist_keys class: character, (character) vector or list", {
  expect_error(get_taxa(checklist_keys = data.frame()),
               paste("checklist_keys should be a character, vector,", 
                     "tibble data.frame or a list."))
  })

testthat::test_that(
  paste("checklist_keys is a list:",
        "output of name_usage(datasetkey = datasetKey)", 
        sep = " "), {
  expect_error(get_taxa(checklist_keys = list(2)),
               "List is corrupted: meta and data are both expected and nothing else.")
  expect_error(get_taxa(checklist_keys =  list(meta = data.frame())),
               "List is corrupted: meta and data are both expected and nothing else.")
  expect_error(get_taxa(checklist_keys =  list(data = data.frame())),
               "List is corrupted: meta and data are both expected and nothing else.")
  expect_error(get_taxa(checklist_keys =  list(meta = data.frame(),
                                               data = data.frame())),
               paste("List is corrupted: class(checklist_keys$data) !=", 
                     "class(name_usage(datasetKey = ...)), tibble.", 
                     sep = " "))
  expect_error(get_taxa(
    checklist_keys =  list(meta = data.frame(),
                           data = tibble::tibble(key = 4, batman = "batman"))),
               "One or more attributes don't match API terms.")
})

testthat::test_that(paste("Checklist_keys is a tibble dataframe",
                          "output of name_usage(datasetkey = datasetKey,",
                          "return = data)",
                          sep = " "), {
  expect_error(get_taxa(
    checklist_keys =  tibble::tibble(key = 1, batman = "batman")),
    "One or more attributes don't match API terms.")
})

testthat::test_that("Limit is a number", {
  expect_error(get_taxa(checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                        limit = "10"),
               "Limit has to be numeric.")
  })
