df_gam <- data.frame(
  taxonKey = rep(2224970, 19),
  canonicalName = rep("Palaemon macrodactylus", 19),
  year = seq(2001, 2019),
  n_observations = c(
    1, 5, 8, 12, 18, 23, 30, 40, 60, 40, 20, 10, 1,
    3, 10, 20, 35, 50, 80
  ),
  baseline_observations = c(
    40, 50, 30, 120, 100, 30, 5000, 40, 100, 30, 20, 40,
    100, 304, 343, 423, 343, 20, 50
  ),
  stringsAsFactors = FALSE
)

testthat::test_that("Test inputs' types.", {
  # df
  testthat::expect_error(apply_gam(
    df = 3,
    y_var = "n_observations",
    eval_years = 2018
  ),
  paste(
    "3 is not a data frame.",
    "Check value of argument df."
  ),
  fixed = TRUE
  )
  # y_var
  testthat::expect_error(apply_gam(
    df = df_gam,
    y_var = 3,
    eval_years = 2018
  ),
  paste(
    "3 is not a character vector.",
    "Check value of argument y_var."
  ),
  fixed = TRUE
  )
  # eval_years
  testthat::expect_error(apply_gam(
    df = df_gam,
    y_var = "n_observations",
    eval_years = "yyyy"
  ),
  paste(
    "yyyy is not a numeric or integer vector.",
    "Check value of argument eval_years."
  ),
  fixed = TRUE
  )
  testthat::expect_error(apply_gam(
    df = df_gam,
    y_var = "n_observations",
    eval_years = list(a = 2018)
  ),
  paste(
    "2018 is not a numeric or integer vector.",
    "Check value of argument eval_years."
  ),
  fixed = TRUE
  )
  # year
  testthat::expect_error(apply_gam(
    df = df_gam,
    y_var = "n_observations",
    year = 3,
    eval_years = 2018
  ),
  paste(
    "3 is not a character vector.",
    "Check value of argument year."
  ),
  fixed = TRUE
  )
  # taxonKey
  testthat::expect_error(apply_gam(
    df = df_gam,
    y_var = "n_observations",
    eval_years = 2018,
    taxonKey = 3
  ),
  paste(
    "3 is not a character vector.",
    "Check value of argument taxonKey."
  ),
  fixed = TRUE
  )
  # type_indicator
  testthat::expect_error(apply_gam(
    df = df_gam,
    y_var = "n_observations",
    eval_years = 2018,
    taxonKey = "taxonKey",
    type_indicator = 34
  ),
  paste(
    "34 is not a character vector.",
    "Check value of argument type_indicator."
  ),
  fixed = TRUE
  )
  # baseline_var
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      taxonKey = "taxonKey",
      baseline_var = 4
    ),
    paste(
      "4 is not a character vector.",
      "Check value of argument baseline_var."
    )
  )
  # taxon_key
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      taxon_key = FALSE
    ),
    paste(
      "FALSE is not a character vector.",
      "Check value of argument taxon_key."
    )
  )
  # name
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      name = c(5, 2)
    ),
    paste(
      "5,2 is not a character vector.",
      "Check value of argument name."
    )
  )
  # df_title
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      df_title = c(5, 2)
    ),
    paste(
      "5,2 is not a character vector.",
      "Check value of argument df_title."
    )
  )
  # x_label
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      x_label = c(5, 2)
    ),
    paste(
      "5,2 is not a character vector.",
      "Check value of argument x_label."
    )
  )
  # y_label
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      y_label = c(5, 2)
    ),
    paste(
      "5,2 is not a character vector.",
      "Check value of argument y_label."
    )
  )
  # saveplot
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      saveplot = c("TRUE")
    ),
    paste(
      "TRUE is not a logical vector.",
      "Check value of argument saveplot.",
      "Did you maybe use quotation marks?"
    )
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      saveplot = c(1, 1)
    ),
    paste(
      "1,1 is not a logical vector.",
      "Check value of argument saveplot.",
      "Did you maybe use quotation marks?"
    )
  )
  # verbose
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      verbose = c(1, 1)
    ),
    paste(
      "1,1 is not a logical vector.",
      "Check value of argument saveplot.",
      "Did you maybe use quotation marks?"
    )
  )
})

testthat::test_that("Test input length.", {
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = c("n_observations", "why a second col?"),
      eval_years = 2018
    ),
    paste("Multiple values for argument y_var provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = c("n_observations"),
      year = c("year", "add a second to screw my code"),
      eval_years = 2018
    ),
    paste("Multiple values for argument year provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = c("n_observations"),
      eval_years = 2018,
      taxonKey = c("taxonKey", "taxon_col")
    ),
    paste("Multiple values for argument taxonKey provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = c("n_observations"),
      eval_years = 2018,
      type_indicator = c("occurrences", "occupancy")
    ),
    paste("Multiple values for argument type_indicator provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = c("n_observations"),
      eval_years = 2018,
      baseline_var = c("q", "f")
    ),
    paste("Multiple values for argument baseline_var provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      name = c("1st_name", "2nd_name")
    ),
    paste("Multiple values for argument name provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      df_title = c("aaa", "bbb")
    ),
    paste("Multiple values for argument df_title provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      x_label = c("aaa", "bbb")
    ),
    paste("Multiple values for argument x_label provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      y_label = c("aaa", "bbb")
    ),
    paste("Multiple values for argument y_label provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      saveplot = c(FALSE, FALSE)
    ),
    paste("Multiple values for argument saveplot provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      saveplot = TRUE,
      dir_name = c("One_dir", "Second_dir")
    ),
    paste("Multiple values for argument dir_name provided.")
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      verbose = c(FALSE, FALSE)
    ),
    paste("Multiple values for argument verbose provided.")
  )
})

testthat::test_that("Test df contains all needed columns and information.", {
  testthat::expect_error(apply_gam(
    df = df_gam,
    y_var = "y_var_wrong",
    eval_years = 2018
  ),
  paste(
    "The column y_var_wrong is not present in df.",
    "Check value of argument y_var."
  ),
  fixed = TRUE
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      year = "bad_year_colname",
      eval_years = 2018
    ),
    paste(
      "The column bad_year_colname is not present in df.",
      "Check value of argument year."
    )
  )
  testthat::expect_error(apply_gam(
    df = df_gam,
    y_var = "n_observations",
    eval_years = 2018,
    taxonKey = "bad_key_col"
  ),
  paste(
    "The column bad_key_col is not present in df.",
    "Check value of argument taxonKey."
  ),
  fixed = TRUE
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 1999
    ),
    paste(
      "One or more evaluation years not present in df.",
      "Check value of argument eval_years."
    )
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2017,
      baseline_var = "you_will_not_find_it"
    ),
    paste(
      "The column you_will_not_find_it is not present in df.",
      "Check value of argument baseline_var."
    )
  )
  testthat::expect_error(apply_gam(
    df = df_gam,
    y_var = "n_observations",
    eval_years = 2018,
    baseline_var = "baseline"
  ),
  paste(
    "The column baseline is not present in df.",
    "Check value of argument baseline_var"
  ),
  fixed = TRUE
  )
})

testthat::test_that("Test other inputs.", {
  # type_indicator
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      type_indicator = "bad name"
    ),
    paste(
      "Invalid type_indicator.",
      "type_indicator has to be one of:",
      "observations, occupancy."
    )
  )
  # p-max
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      p_max = "not a number"
    ),
    paste(
      "p_max is a p-value: it has to be a",
      "number between 0 and 1."
    )
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      p_max = -4
    ),
    paste(
      "p_max is a p-value: it has to be a",
      "number between 0 and 1."
    )
  )
  testthat::expect_error(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      p_max = 2
    ),
    paste(
      "p_max is a p-value: it has to be a",
      "number between 0 and 1."
    )
  )
  # message dir_name when saveplot is FALSE
  testthat::expect_message(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      saveplot = FALSE,
      dir_name = "./data/",
      width = NULL,
      height = NULL
    ),
    paste(
      "saveplot is FALSE: plots are not saved.",
      "Argument `dir_name` ignored."
    )
  )
  # message width when saveplot is FALSE
  testthat::expect_message(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      saveplot = FALSE,
      dir_name = NULL,
      width = 1000,
      height = NULL
    ),
    paste(
      "saveplot is FALSE: plots are not saved.",
      "Argument `width` ignored."
    )
  )
  # message height when saveplot is FALSE
  testthat::expect_message(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      saveplot = FALSE,
      dir_name = NULL,
      width = NULL,
      height = 1000
    ),
    paste(
      "saveplot is FALSE: plots are not saved.",
      "Argument `height` ignored."
    )
  )
  # message width = NULL and saveplot is TRUE
  testthat::expect_message(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      saveplot = TRUE,
      width = NULL,
      height = 1000
    ),
    "width not provided. Set to 1680 pixels."
  )
  # message height = NULL and saveplot is TRUE
  testthat::expect_message(
    apply_gam(
      df = df_gam,
      y_var = "n_observations",
      eval_years = 2018,
      saveplot = TRUE,
      width = 2000,
      height = NULL
    ),
    "height not provided. Set to 1200 pixels."
  )
  # Remove the png created while using `saveplot = TRUE`
  unlink("GAM_observations_basic.png")
})
