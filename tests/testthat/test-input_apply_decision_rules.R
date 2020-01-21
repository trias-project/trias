context("test_input_decision_rules")

df_dr <- tibble(
  taxonID = c(rep(1008955, 10), rep(2493598, 3)),
  y = c(seq(2009, 2018), seq(2016, 2018)),
  obs = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 3, 0)
)

testthat::test_that("Test inputs' types.", {
  # df
  expect_error(apply_decision_rules(df = 3,
                                    y_var = "n_observations",
                                    eval_year = 2018),
               paste("3 is not a data frame.",
                     "Check value of argument df."),
               fixed = TRUE)
  # y_var
  expect_error(apply_decision_rules(df = df_dr,
                                    y_var = 3,
                                    eval_year = 2018),
               paste("3 is not a character vector.",
                     "Check value of argument y_var."),
               fixed = TRUE)
  # eval_year
  expect_error(apply_decision_rules(df = df_dr,
                         y_var = "n_observations",
                         eval_year = "yyyy"),
               paste("yyyy is not a numeric or integer vector.",
                     "Check value of argument eval_year."),
               fixed = TRUE)
  expect_error(apply_decision_rules(df = df_dr,
                         y_var = "n_observations",
                         eval_year = list(a = 2018)),
               paste("2018 is not a numeric or integer vector.",
                     "Check value of argument eval_year."),
               fixed = TRUE)
  # year
  expect_error(apply_decision_rules(df = df_dr,
                         y_var = "n_observations",
                         year = 3,
                         eval_year = 2018),
               paste("3 is not a character vector.",
                     "Check value of argument year."),
               fixed = TRUE)
  # taxonKey
  expect_error(apply_decision_rules(df = df_dr,
                         y_var = "n_observations",
                         eval_year = 2018, 
                         taxonKey = 3),
               paste("3 is not a character vector.",
                     "Check value of argument taxonKey."),
               fixed = TRUE)
}
)

testthat::test_that("Test input length.", {
  expect_error(apply_decision_rules(df = df_dr,
                         y_var = c("n_observations", "why a second col?"),
                         eval_year = 2018),
               paste("Multiple values for argument y_var provided."))
  expect_error(apply_decision_rules(df = df_dr,
                         y_var = c("n_observations"),
                         year = c("year", "add a second to screw my code"),
                         eval_year = 2018),
               paste("Multiple values for argument year provided."))
  expect_error(apply_decision_rules(df = df_dr,
                         y_var = c("n_observations"),
                         eval_year = 2018,
                         taxonKey = c("taxonKey", "taxon_col")),
               paste("Multiple values for argument taxonKey provided."))
  }
)

testthat::test_that("Test df contains all needed columns and information.", {
  expect_error(apply_decision_rules(df = df_dr,
                         y_var = "y_var_wrong",
                         eval_year = 2018,
                         year = "y",
                         taxonKey = "taxonID"),
               paste("The column y_var_wrong is not present in df.",
                     "Check value of argument y_var."),
               fixed = TRUE)
  expect_error(apply_decision_rules(df = df_dr,
                                    taxonKey = "taxonID",
                                    y_var = "obs",
                                    year = "bad_year_colname",
                                    eval_year = 2018),
               paste("The column bad_year_colname is not present in df.",
                     "Check value of argument year."))
  expect_error(apply_decision_rules(df = df_dr,
                                    y_var = "obs",
                                    year = "y",
                                    eval_year = 2018,
                                    taxonKey = "bad_key_col"),
               paste("The column bad_key_col is not present in df.",
                     "Check value of argument taxonKey."),
               fixed = TRUE)
  expect_error(apply_decision_rules(df = df_dr,
                                    y_var = "obs",
                                    year = "y",
                                    eval_year = 1980,
                                    taxonKey = "taxonID"),
               paste("Evaluation year not present in df.",
                     "Check value of argument eval_year."))
  }
)
