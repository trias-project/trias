context("test_indicator_native_range_year")

# test df
input_test_df <- read.delim(
  test_path("data_test_input_graphs_indicators/test_visualization_df.tsv"),
  sep = "\t"
)

cleaned_input_test_df <- input_test_df[!is.na(input_test_df$first_observed), ]
cleaned_input_test_df <- cleaned_input_test_df[
  cleaned_input_test_df$locality == "Belgium",
]

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


test_that("Arg: x_major_scale_stepsize", {
  expect_error(
    indicator_native_range_year(cleaned_input_test_df,
                                x_major_scale_stepsize = "10"
    ),
    "Argument x_major_scale_stepsize has to be a number."
  )
})

test_that("Arg: x_include_missing", {
  expect_error(
    indicator_native_range_year(cleaned_input_test_df,
                                x_include_missing = "TRUE"
    ),
    "Argument x_include_missing has to be a logical."
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
  
  # function automatically retains unique records per species & type
  invasive_df <- cleaned_input_test_df[
    cleaned_input_test_df$degree_of_establishment == "invasive", 
  ]
  nRecords <- nrow(unique(invasive_df[, c("last_observed", "native_range")]))
  plot_output_invasive <- indicator_native_range_year(invasive_df)
  expect_equal(nrow(plot_output_invasive$data), nRecords)  
  
  # Otherwise too many entries in legend
  cleaned_input_test_df <- cleaned_input_test_df[grepl("america", cleaned_input_test_df$native_range, ignore.case = TRUE), ]
  
  plot_output_list <- list(
    # absolute
    absolute = indicator_native_range_year(df = cleaned_input_test_df,
                                           years = c(2000, 2005)
    ),
    # relative                                
    relative = indicator_native_range_year(df = cleaned_input_test_df,
                                           years = c(2000, 2005),
                                           response_type = "relative"
    ),
    # cumulative
    cumulative = indicator_native_range_year(df = cleaned_input_test_df,
                                             years = c(2000, 2005),
                                             response_type = "cumulative"
    )
  )
  
  # data slot is not affected by value of related arg
  expect_identical(plot_output_list$absolute$data, plot_output_list$relative$data)
  
  for (response_type in names(plot_output_list)) {
    
    plot_output <- plot_output_list[[response_type]]
    
    # output is a list
    expect_type(plot_output, type = "list")
    
    # output has the right three slots
    slots <- c("static_plot", "interactive_plot", "data")
    expect_equal(names(plot_output), slots, info = response_type)
    
    # static plot slot is a ggplot object
    expect_true(
      all(c("gg", "ggplot") %in% class(plot_output$static_plot))
    )
    
    # interactive plot slot is a list with plotly and htmlwidget as class
    expect_type(plot_output$interactive_plot, type = "list")
    expect_s3_class(plot_output$interactive_plot, class = c("plotly", "htmlwidget"))
    
    # data is a data.frame (tibble)
    expect_type(plot_output$data, type = "list")
    expect_s3_class(plot_output$data, class = c("data.frame", "tbl_df"))
    
    # data contains only columns `year`, `native_range`, `n`, `total` and `perc`
    # in this order
    expect_equal(
      names(plot_output$data),
      c("year", "native_range", "n", "total", "perc"),
      info = response_type
    )
    
    # columns `year` and `native_range` of data slot are factors
    expect_true(is.factor(plot_output$data$year), info = response_type)
    expect_true(is.factor(plot_output$data$native_range), info = response_type)
    # columns `n` and total of data slot are integers
    expect_true(is.integer(plot_output$data$n), info = response_type)
    expect_true(is.integer(plot_output$data$total), info = response_type)
    # column `perc` of data slot is double
    expect_true(is.double(plot_output$data$perc), info = response_type)
    
  }
  
})

test_that("x_include_missing arg for indicator_native_range", {
  # For brevity we perform general checks with `response_type` = "absolute"
  # only
  plot_output_absolute <- indicator_native_range_year(
    df = cleaned_input_test_df,
    response_type = "absolute",
    x_include_missing = TRUE
  )
  
  # output is a list
  expect_type(plot_output_absolute, type = "list")
  
  # static plot slot is a ggplot object
  expect_true(
    all(c("gg", "ggplot") %in% class(plot_output_absolute$static_plot))
  )
  
  # interactive plot slot is a list with plotly and htmlwidget as class
  expect_type(plot_output_absolute$interactive_plot, type = "list")
  expect_s3_class(
    plot_output_absolute$interactive_plot,
    class = c("plotly", "htmlwidget")
  )
  
  # `data` is a data.frame (tibble)
  expect_type(plot_output_absolute$data, type = "list")
  expect_s3_class(
    plot_output_absolute$data,
    class = c("data.frame", "tbl_df")
  )
  
  # `data` contains only columns `year`, `native_range`, `n`, `total` and `perc`
  expect_equal(
    names(plot_output_absolute$data),
    c("year", "native_range", "n", "total", "perc")
  )
  
  # columns `year` and `native_range` of data slot are factors
  expect_true(is.factor(plot_output_absolute$data$year))
  expect_true(is.factor(plot_output_absolute$data$native_range))
  # columns `n` and `total` of data slot are integers
  expect_true(is.integer(plot_output_absolute$data$n))
  expect_true(is.integer(plot_output_absolute$data$total))
  
  # Non general checks start here
  
  # `n` is 0 for missing years
  missing_years <- plot_output_absolute$data$year[
    !(plot_output_absolute$data$year %in% 
        cleaned_input_test_df$first_observed)
  ]
  expect_true(all(plot_output_absolute$data$n[
    plot_output_absolute$data$year %in% missing_years
  ] == 0))
  
  # `n` is 0 for missing years when `response_type` = "relative". Also,
  # same missing years as for absolute output
  plot_output_relative <- indicator_native_range_year(
    df = cleaned_input_test_df,
    response_type = "relative",
    x_include_missing = TRUE
  )
  expect_identical(
    missing_years,
    plot_output_relative$data$year[
      !(plot_output_relative$data$year %in% 
          cleaned_input_test_df$first_observed)
    ]
  )
  expect_true(all(plot_output_relative$data$n[
    plot_output_relative$data$year %in% missing_years
  ] == 0))
  
  # When `response_type` = "relative", columns `year` and `native_range` of data
  # slot are still factors
  expect_true(is.factor(plot_output_relative$data$year))
  expect_true(is.factor(plot_output_relative$data$native_range))
  # Columns `n` and `total` of data slot are still integers
  expect_true(is.integer(plot_output_relative$data$n))
  expect_true(is.integer(plot_output_relative$data$total))
  
  # n is always equal or greater than previous year if `response_type` =
  # "cumulative" and same missing years
  plot_output_cumulative <- indicator_native_range_year(
    df = cleaned_input_test_df,
    response_type = "cumulative",
    x_include_missing = TRUE
  )
  expect_identical(
    missing_years,
    plot_output_cumulative$data$year[
      !(plot_output_cumulative$data$year %in% 
          cleaned_input_test_df$first_observed)
    ]
  )
  testthat::expect_true(
    all(
      unlist(
        lapply(
          split(plot_output_cumulative$data, 
                f = plot_output_cumulative$data$native_range),
          function(x) all(diff(x$n) >= 0)
        )
      )
    )
  )
  
  # When `response_type` = "cumulative", columns `year` and `native_range` of
  # data slot are still factors
  expect_true(is.factor(plot_output_cumulative$data$year))
  expect_true(is.factor(plot_output_cumulative$data$native_range))
  # Columns `n` and `total` of data slot are still integers
  expect_true(is.integer(plot_output_cumulative$data$n))
  expect_true(is.integer(plot_output_cumulative$data$total))
})

test_that("relative arg is deprecated", {
  lifecycle::expect_deprecated(
      indicator_native_range_year(
        df = cleaned_input_test_df,
        years = c(2000, 2005),
        relative = TRUE
  ))
})
