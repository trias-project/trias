context("test_output_decision_rules")

df_dr <- tibble(
  taxonID = c(rep(1008955, 10), rep(2493598, 3), 1000000),
  y = c(seq(2009, 2018), seq(2016, 2018), 1990),
  obs = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 3, 0, 5)
)

df_dr_expanded <- tibble(
  taxonID = c(rep(1008955, 10), rep(2493598, 3), 1000000),
  y = c(seq(2009, 2018), seq(2016, 2018), 1990),
  obs = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 3, 0, 5),
  cobs = c(1, 23, 10, 1000, 234, 592, 495, 12, 2324, 43, 31434, 32, 34, 50)
)

df_dr_one_val <- tibble(
  taxonID = 1,
  y = 2018,
  obs = 45
)

eyear <- 2018

result_dr <- apply_decision_rules(df = df_dr,
                     y_var = "obs",
                     eval_year = eyear,
                     year = "y",
                     taxonKey = "taxonID")

result_dr_expanded <- apply_decision_rules(df = df_dr_expanded,
                                  y_var = "obs",
                                  eval_year = eyear,
                                  year = "y",
                                  taxonKey = "taxonID")

result_dr_one_val <- apply_decision_rules(df = df_dr_one_val,
                                          y_var = "obs",
                                          eval_year = eyear,
                                          year = "y",
                                          taxonKey = "taxonID")

cols_output <- c("taxonID", "y", "em_status", "dr_1", "dr_3", "dr_4", "dr_8")

testthat::test_that("Test type of output", {
  # em_summary is a data.frame
  expect_true(is.data.frame(result_dr))
  expect_true(is.data.frame(result_dr_expanded))
  expect_true(is.data.frame(result_dr_one_val))
  }
)

testthat::test_that("Test number of rows and columns", {
  # output doesn't contain taxa whith data before eval_year (taxon 1000000)
  expect_true(nrows(result_dr) == n_distinct(df_dr$taxonID) - 1)
  expect_true(nrows(result_dr_expanded) == nrows(result_dr))
  expect_true(nrows(result_dr_one_val) == nrows(df_dr_one_val))
  # output contains only some columns and in a specific order 
  expect_true(ncols(result_dr) == length(cols_output))
  expect_true(ncols(result_dr_expanded) == length(cols_output))
  expect_true(ncols(result_dr_one_val) == length(cols_output))
  expect_identical(names(result_dr), cols_output)
  expect_identical(names(result_dr_expanded), cols_output)
  expect_identical(names(result_dr_one_val), cols_output)
  }
)

testthat::test_that("Test type of columns", {
  expect_true(all(is.numeric(result_dr$taxonID),
                  is.numeric(result_dr$y),
                  is.numeric(result_dr$em_status)))
  expect_true(all(is.logical(result_dr$dr_1),
                  is.logical(result_dr$dr_3),
                  is.logical(result_dr$dr_4),
                  is.logical(result_dr$dr_8)))
  }
)

testthat::test_that("Test values of column with taxon keys", {
  expect_true(all(result_dr$taxonID %in% df_dr$taxonID))
  expect_true(all(result_dr_expanded$taxonID %in% df_dr_expanded$taxonID))
  }
)

testthat::test_that("Test values of column with year values", {
  expect_true(all(result_dr$y == eyear))
  expect_true(all(result_dr_expanded$y == eyear))
  }
)

testthat::test_that("Test values of emerging status", {
  expect_true(all(result_dr$em_status %in% c(0, 1, 2, 3)))
  expect_true(all(result_dr_expanded$em_status %in% c(0, 1, 2, 3)))
  expect_true(result_dr %>% filter(em_status == 0) %>% pull(dr_4))
  expect_true()
  }
)
