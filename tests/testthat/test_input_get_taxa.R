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

testthat::test_that("taxon_keys class: numeric, character, vector or list.", {
  expect_error(get_taxa(data.frame()), 
               "taxon_keys should be a numeric, character, vector or a list.")
})

testthat::test_that("taxon_keys is a list: output of name_usage(key = key)", {
  expect_error(get_taxa(list(2)),
               "List is corrupted: meta and data are both expected and nothing else.")
  expect_error(get_taxa(list(meta = data.frame())),
               "List is corrupted: meta and data are both expected and nothing else.")
  expect_error(get_taxa(list(data = data.frame())),
               "List is corrupted: meta and data are both expected and nothing else.")
  expect_error(get_taxa(list(meta = data.frame(),
                             data = data.frame())),
               paste("List is corrupted: class(data) doesn't match class", 
                     "of data from name_usage(), tibble.", sep = " "))
  expect_error(get_taxa(list(meta = data.frame(),
                             data = tibble::tibble(key = 4, batman = "batman"))),
               "One or more ttributes don't match API terms.")
  })

testthat::test_that("checklist_keys class: character, (character) vector or list", {
  expect_error(get_taxa(checklist_keys = 1),
               "checklist_keys should be a numeric, character, vector or a list.")
  # expect_error(get_taxa(checklist_keys = data.frame()),
  #              s)
  })

# if (inherits(taxon_keys, "list")) {
#   assert_that(isTRUE(all.equal(names(taxon_keys), c("meta", "data"))),
#               msg = "List is corrupted: meta and data are both expected and nothing else.")
  
# 
# test_that("str_length of missing is missing", {
#   expect_equal(str_length(NA), NA_integer_)
#   expect_equal(str_length(c(NA, 1)), c(NA, 1))
#   expect_equal(str_length("NA"), 2)
# })