context("test_input_apply_gam")

df_gam <- data.frame(
  taxonKey = rep(2224970, 19),
  canonicalName = rep("Palaemon macrodactylus", 19),
  year = seq(2001, 2019),
  n_observations = c(1, 5, 3, 12, 10, 3, 5, 0, 1, 3, 2, 4, 1,3, 3, 4,3, 0, 5),
  baseline_observations = c(40, 50, 30, 120, 100, 30, 5000, 40, 100, 30, 20, 40,
                            100,304, 343, 423, 343, 20, 50),
  stringsAsFactors = FALSE
)

testthat::test_that("Test inputs' types.", {
  expect_error(apply_gam(df = 3,
                         y_var = "n_observations",
                         eval_years = 2018),
               paste("3 is not a data frame.",
                     "Check value of argument df."),
               fixed = TRUE)
  # eval_years
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = "yyyy"),
               paste("yyyy is not a numeric or integer vector.",
                     "Check value of argument eval_years."),
               fixed = TRUE)
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = list(a = 2018)),
               paste("2018 is not a numeric or integer vector.",
                     "Check value of argument eval_years."),
               fixed = TRUE)
  # y_var
  expect_error(apply_gam(df = df_gam,
                         y_var = 3,
                         eval_years = 2018),
               paste("3 is not a character vector.",
                     "Check value of argument y_var."),
               fixed = TRUE)
  # year
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         year = 3,
                         eval_years = 2018),
               paste("3 is not a character vector.",
                     "Check value of argument year."),
               fixed = TRUE)
  # taxonKey
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018, 
                         taxonKey = 3),
               paste("3 is not a character vector.",
                     "Check value of argument taxonKey."),
               fixed = TRUE)
  # type_indicator
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         taxonKey = "taxonKey",
                         type_indicator = 34),
               paste("34 is not a character vector.",
                     "Check value of argument type_indicator."),
               fixed = TRUE)
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         taxonKey = "taxonKey",
                         baseline_var = 4),
               paste("4 is not a character vector.",
                     "Check value of argument baseline_var."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         saveplot = c("TRUE")),
               paste("TRUE is not a logical vector.",
                     "Check value of argument saveplot.",
                     "Did you maybe use quotation marks?"))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         saveplot = c(1,1)),
               paste("1,1 is not a logical vector.",
                     "Check value of argument saveplot.",
                     "Did you maybe use quotation marks?"))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         verbose = c(1,1)),
               paste("1,1 is not a logical vector.",
                     "Check value of argument saveplot.",
                     "Did you maybe use quotation marks?"))
  }
)

testthat::test_that("Test input length.", {
  expect_error(apply_gam(df = df_gam,
                         y_var = c("n_observations", "why a second col?"),
                         eval_years = 2018),
               paste("Multiple values for argument y_var provided."))
  expect_error(apply_gam(df = df_gam,
                         y_var = c("n_observations"),
                         year = c("year", "add a second to screw my code"),
                         eval_years = 2018),
               paste("Multiple values for argument year provided."))
  expect_error(apply_gam(df = df_gam,
                         y_var = c("n_observations"),
                         eval_years = 2018,
                         taxonKey = c("taxonKey", "taxon_col")),
               paste("Multiple values for argument taxonKey provided."))
  expect_error(apply_gam(df = df_gam,
                         y_var = c("n_observations"),
                         eval_years = 2018,
                         type_indicator = c("occurrences", "occupancy")),
               paste("Multiple values for argument type_indicator provided."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         name = c("1st_name", "2nd_name")),
               paste("Multiple values for argument name provided."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         df_title = c("aaa", "bbb")),
               paste("Multiple values for argument df_title provided."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         saveplot = c(FALSE, FALSE)),
               paste("Multiple values for argument saveplot provided."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         saveplot = TRUE,
                         dir_name = c("One_dir", "Second_dir")),
               paste("Multiple values for argument dir_name provided."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         verbose = c(FALSE, FALSE)),
               paste("Multiple values for argument verbose provided."))
  }
)

testthat::test_that("Test df contains all needed columns and information.", {
  expect_error(apply_gam(df = df_gam,
                         y_var = "y_var_wrong",
                         eval_years = 2018),
               paste("The column y_var_wrong is not present in df.",
                      "Check value of argument y_var."),
               fixed = TRUE)
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         year = "bad_year_colname",
                         eval_years = 2018),
               paste("The column bad_year_colname is not present in df.",
                     "Check value of argument year."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018, 
                         taxonKey = "bad_key_col"),
               paste("The column bad_key_col is not present in df.",
                     "Check value of argument taxonKey."),
               fixed = TRUE)
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 1999),
               paste("One or more evaluation years not present in df.",
                     "Check value of argument eval_years."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         baseline_var = "baseline"),
               paste("The column baseline is not present in df.",
                     "Check value of argument baseline_var"),
               fixed = TRUE)
  }
)

testthat::test_that("Test other inputs.", {
  expect_error(apply_gam(df = df_gam,
                          y_var = "n_observations",
                          eval_years = 2018,
                          type_indicator = "bad name"),
                paste("Invalid type_indicator.",
                      "type_indicator has to be one of:", 
                      "observations, occupancy."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         p_max = "not a number"),
               paste("p_max is a p-value: it has to be a",
                     "number between 0 and 1."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         p_max = -4),
               paste("p_max is a p-value: it has to be a",
                     "number between 0 and 1."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         p_max = 2),
               paste("p_max is a p-value: it has to be a",
                     "number between 0 and 1."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         p_max = 0),
               paste("p_max is a p-value: it has to be a",
                     "number between 0 and 1."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         p_max = 1),
               paste("p_max is a p-value: it has to be a",
                     "number between 0 and 1."))
  expect_error(apply_gam(df = df_gam,
                         y_var = "n_observations",
                         eval_years = 2018,
                         name = c(5,2)),
               paste("5,2 is not a character vector.",
                     "Check value of argument name."))
  
  expect_warning(apply_gam(df = df_gam,
                           y_var = "n_observations",
                           eval_years = 2018, 
                           saveplot = FALSE, 
                           dir_name = "./data/"),
                 paste("saveplot is FALSE: plots are not saved.", 
                       "Argument dir_name ignored."))
  }
)