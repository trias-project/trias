context("test_output_decision_rules")
library(dplyr)
library(tibble)
library(purrr)

df_dr <- tibble(
  taxonID = c(rep(1008955, 10), rep(2493598, 3), rep(1143956, 3)),
  y = c(seq(2011, 2020), seq(2016, 2018), seq(2016, 2018)),
  obs = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 3, 0, 4, 3, 40)
)

df_dr_chr <-
  df_dr %>%
  mutate(taxonID = as.character(taxonID))

df_dr_int <-
  df_dr %>%
  mutate(taxonID = as.integer(taxonID))

df_dr_y_int <-
  df_dr %>%
  mutate(y = as.integer(y))

df_dr_expanded <- tibble(
  taxonID = c(rep(1008955, 10), rep(2493598, 3), rep(1143956, 3)),
  y = c(seq(2011, 2020), seq(2016, 2018), seq(2016, 2018)),
  obs = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 3, 0, 4, 3, 40),
  cobs = c(1, 23, 10, 100, 23, 59, 495, 12, 234, 43, 3434, 32, 3, 4, 9, 100)
)

df_dr_one_val <- tibble(
  taxonID = 1,
  y = 2018,
  obs = 45
)

eyear <- 2018
early_eyear <- 2014
late_eyear <- 2020

result_dr <- apply_decision_rules(
  df = df_dr,
  y_var = "obs",
  eval_year = eyear,
  year = "y",
  taxonKey = "taxonID"
)

result_dr_chr <- apply_decision_rules(
  df = df_dr_chr,
  y_var = "obs",
  eval_year = eyear,
  year = "y",
  taxonKey = "taxonID"
)

result_dr_int <- apply_decision_rules(
  df = df_dr_int,
  y_var = "obs",
  eval_year = eyear,
  year = "y",
  taxonKey = "taxonID"
)

result_dr_expanded <- apply_decision_rules(
  df = df_dr_expanded,
  y_var = "obs",
  eval_year = eyear,
  year = "y",
  taxonKey = "taxonID"
)

result_dr_y_int <- apply_decision_rules(
  df = df_dr_y_int,
  y_var = "obs",
  eval_year = eyear,
  year = "y",
  taxonKey = "taxonID"
)

expect_warning(
  result_dr_expanded_early <- apply_decision_rules(
    df = df_dr,
    y_var = "obs",
    eval_year = early_eyear,
    year = "y",
    taxonKey = "taxonID"
  )
)

expect_warning(
  result_dr_expanded_late <- apply_decision_rules(
    df = df_dr,
    y_var = "obs",
    eval_year = late_eyear,
    year = "y",
    taxonKey = "taxonID"
  )
)

result_dr_one_val <- apply_decision_rules(
  df = df_dr_one_val,
  y_var = "obs",
  eval_year = eyear,
  year = "y",
  taxonKey = "taxonID"
)

df_inputs <- list(
  df_dr,
  df_dr_expanded,
  df_dr_chr,
  df_dr_int,
  df_dr_y_int,
  df_dr_one_val,
  df_dr,
  df_dr
)
df_outputs <- list(
  result_dr,
  result_dr_expanded,
  result_dr_chr,
  result_dr_int,
  result_dr_y_int,
  result_dr_one_val,
  result_dr_expanded_early,
  result_dr_expanded_late
)
cols_output <- c("taxonID", "y", "em_status", "dr_1", "dr_2", "dr_3", "dr_4")

test_that("Test type of output", {
  # output is a data.frame
  expect_true(all(map_lgl(
    df_outputs, ~ is.data.frame(.)
  )))
  # output is a tibble
  expect_true(all(map_lgl(
    df_outputs, ~ is_tibble(.)
  )))
})

test_that("Test rows and columns", {
  # output contain always all taxa
  expect_true(all(map2_lgl(df_outputs, df_inputs, function(x, y) {
    nrow(x) == n_distinct(y$taxonID)
  })))
  # output contains only some columns and in a specific order
  expect_true(all(map_lgl(df_outputs, ~ all(names(.) == cols_output))))
})

test_that("Test type of columns", {
  expect_true(all(map2_lgl(
    df_outputs, df_inputs,
    function(x, y) {
      class(x$taxonID) == class(y$taxonID)
    }
  )))
  expect_true(all(map_lgl(df_outputs, ~ is.numeric(.$y))))
  expect_true(all(map_lgl(df_outputs, ~ is.numeric(.$em_status))))
  expect_true(all(map_lgl(df_outputs, ~ is.logical(.$dr_1))))
  expect_true(all(map_lgl(df_outputs, ~ is.logical(.$dr_2))))
  expect_true(all(map_lgl(df_outputs, ~ is.logical(.$dr_3))))
  expect_true(all(map_lgl(df_outputs, ~ is.logical(.$dr_4))))
})

test_that("Test values of column with taxon keys", {
  expect_true(all(map2_lgl(
    df_outputs,
    df_inputs,
    function(x, y) {
      all(x$taxonID %in% y$taxonID) & all(y$taxonID %in% x$taxonID)
    }
  )))
})

test_that("Test values of column with year values", {
  expect_true(all(map_lgl(
    list(
      result_dr,
      result_dr_int,
      result_dr_chr,
      result_dr_y_int,
      result_dr_one_val,
      result_dr_expanded
    ),
    function(x) all(x$y == eyear)
  )))
  expect_true(all(result_dr_expanded_early$y == early_eyear))
  expect_true(all(result_dr_expanded_late$y == late_eyear))
})

test_that("Test values of emerging status", {
  expect_true(all(map_lgl(
    list(
      result_dr_one_val,
      result_dr_expanded,
      result_dr,
      result_dr_y_int,
      result_dr_chr,
      result_dr_int
    ),
    function(x) all(x$em_status %in% c(0, 1, 2, 3))
  )))

  expect_true(all(
    (result_dr_expanded_early %>%
      filter(is.na(em_status)) %>%
      pull(taxonID)) %in%
      (df_dr %>%
        group_by(taxonID) %>%
        summarize(min_year = min(y)) %>%
        filter(min_year > early_eyear) %>%
        pull(taxonID))
  ))
  expect_true(all(
    (result_dr_expanded_late %>%
      filter(is.na(em_status)) %>%
      pull(taxonID)) %in%
      (df_dr %>%
        group_by(taxonID) %>%
        summarize(max_year = max(y)) %>%
        filter(max_year < late_eyear) %>%
        pull(taxonID))
  ))

  expect_true(result_dr_expanded_late %>%
    filter(em_status == 0) %>%
    pull(dr_3) == TRUE)
  # Apeparing species (one value) have emerging status unclear (em_status: 1)
  expect_true(result_dr_one_val$em_status == 1)
})
