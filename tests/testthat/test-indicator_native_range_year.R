context("test_indicator_native_range_year")

# test df
input_test_df <- read.delim(
  test_path("data_test_input_graphs_indicators/test_visualization_df.tsv"),
  sep = "\t"
)

nrow_no_first_obs <-
  nrow(input_test_df[is.na(input_test_df$first_observed), ])

cleaned_input_test_df <- input_test_df[!is.na(input_test_df$first_observed), ]

test_that("Arg: df", {
  expect_error(
    indicator_native_range_year(3),
    "df is not a data frame"
  )
})


test_that("Arg: type", {
  expect_error(
    indicator_native_range_year(cleaned_input_test_df,
                                type = 2,
                                years = 2001),
    "'arg' must be NULL or a character vector"
  )
  a <- cleaned_input_test_df
  colnames(a)[colnames(a) == "native_range"] <- "l"
  expect_error(
    indicator_native_range_year(a,
                                type = "native_range", 
                                years = 2001),
    "Column native_range not present in df."
  )
})


test_that("Arg: years", {
  expect_error(
    indicator_native_range_year(cleaned_input_test_df,
      years = "2000"
    ),
    "Argument years has to be a number."
  )
  expect_error(
    indicator_native_range_year(cleaned_input_test_df,
                                years = c(2000, 3000)
    ),
    sprintf("All values in years has to be less than %s.", format(Sys.Date(), "%Y"))
  )
})

test_that("Arg: first_observed", {
  expect_error(
    indicator_native_range_year(input_test_df, 
                                years = 2010, 
                                first_observed = 3),
    "Argument first_observed has to be a character."
  )
  expect_error(
    indicator_native_range_year(input_test_df,
                                year = 2010,
                                first_observed = "bad_colname"
    )
  )
})

test_that("Param: labels", {
  expect_error(
    indicator_native_range_year(input_test_df, x_lab = input_test_df),
    "Argument x_lab has to be a character or NULL."
  )
  expect_error(
    indicator_native_range_year(input_test_df, y_lab = 4),
    "Argument y_lab has to be a character or NULL."
  )
})

test_that("Test output type, class, slots and columns", {
  plot_output <-
    indicator_native_range_year(cleaned_input_test_df,
                                years = c(2000, 2005)
  )
  plot_output_rel <-
    indicator_native_range_year(cleaned_input_test_df,
                                years = c(2000, 2005),
                                relative = TRUE
  )
  # output is a list
  expect_type(plot_output, type = "list")
  expect_type(plot_output_rel, type = "list")
  
  # output has the right three slots
  slots <- c("static_plot", "interactive_plot", "data")
  expect_equal(names(plot_output), slots)
  expect_equal(names(plot_output_rel), slots)
  
  # static plot slot is a list with gg as class
  expect_type(plot_output$static_plot, type = "list")
  expect_type(plot_output_rel$static_plot, type = "list")
  expect_s3_class(plot_output$static_plot, class = c("gg", "ggplot"))
  expect_s3_class(plot_output_rel$static_plot, class = c("gg", "ggplot"))
  
  # interactive plot slot is a list with plotly and htmlwidget as class
  expect_type(plot_output$interactive_plot, type = "list")
  expect_type(plot_output_rel$interactive_plot, type = "list")
  expect_s3_class(plot_output$interactive_plot, class = c("plotly", "htmlwidget"))
  expect_s3_class(plot_output_rel$interactive_plot, class = c("plotly", "htmlwidget"))
  
  # data is a data.frame (tibble)
  expect_type(plot_output$data, type = "list")
  expect_s3_class(plot_output$data, class = c("data.frame", "tbl_df"))
  expect_type(plot_output_rel$data, type = "list")
  expect_s3_class(plot_output_rel$data, class = c("data.frame", "tbl_df"))
  
  # data contains only columns year, native_range, n, total and perc in this
  # order
  expect_equal(
    names(plot_output$data),
    c("year", "native_range", "n", "total", "perc")
  )
  expect_equal(
    names(plot_output_rel$data),
    c("year", "native_range", "n", "total", "perc")
  )
  
  # columns year and native_range of data slot are factors
  expect_true(is.factor(plot_output$data$year))
  expect_true(is.factor(plot_output$data$native_range))
  # columns n and total of data slot are integers
  expect_true(is.integer(plot_output$data$n))
  expect_true(is.integer(plot_output$data$total))
  # column perc of data slot is double
  expect_true(is.double(plot_output$data$perc))
  
  # data slot is not affected by value of related arg
  expect_identical(plot_output$data, plot_output_rel$data)
  
  # function automatically retains unique records per species & type
  invasive_df <- cleaned_input_test_df[
    cleaned_input_test_df$degree_of_establishment == "invasive", 
  ]
  nRecords <- nrow(unique(invasive_df[, c("last_observed", "native_range")]))
  plot_output_invasive <- indicator_native_range_year(invasive_df)
  expect_equal(nrow(plot_output_invasive$data), nRecords)
  
})
