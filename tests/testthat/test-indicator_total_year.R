context("test_indicator_total_year")

# test df
input_test_df_with_na <- read.delim(
  paste0(
    "./data_test_input_graphs_indicators/",
    "test_visualization_df.tsv"
  ),
  sep = "\t"
)

nrow_no_first_obs <-
  nrow(input_test_df_with_na[is.na(input_test_df_with_na$first_observed), ])

input_test_df <- input_test_df_with_na[!is.na(input_test_df_with_na$first_observed), ]

plot_output_without_facets <-
  indicator_total_year(input_test_df,
    facet_column = NULL
  )
plot_output_with_facets <-
  indicator_total_year(input_test_df,
    facet_column = "kingdom"
  )

test_that("Param: df", {
  expect_error(
    indicator_total_year(3),
    "df is not a data frame"
  )
})

test_that("Param: start_year_plot", {
  expect_error(
    indicator_total_year(input_test_df,
      start_year_plot = "1950"
    ),
    "Argument start_year_plot has to be a number."
  )
  expect_error(
    indicator_total_year(input_test_df,
      start_year_plot = 2900
    ),
    paste(
      "Argument start_year_plot has to be less than",
      format(Sys.Date(), "%Y")
    )
  )
})

test_that("Param: x_scale_stepsize", {
  expect_error(
    indicator_total_year(input_test_df,
      x_minor_scale_stepsize = "10"
    ),
    "Argument x_minor_scale_stepsize has to be a number."
  )
  expect_error(
    indicator_total_year(input_test_df,
      x_major_scale_stepsize = "10"
    ),
    "Argument x_major_scale_stepsize has to be a number."
  )
  expect_error(
    indicator_total_year(input_test_df,
      x_major_scale_stepsize = 50,
      x_minor_scale_stepsize = 100
    ),
    paste0(
      "x_major_scale_stepsize should be greater ",
      "than x_minor_scale_stepsize."
    )
  )
})

test_that("Param: facet_column", {
  expect_error(
    indicator_total_year(input_test_df, facet_column = 3),
    "Argument facet_column has to be NULL or a character."
  )
  expect_error(
    indicator_total_year(input_test_df,
      facet_column = "not nice"
    )
  )
})

test_that("Param: taxon_key_col", {
  expect_error(
    indicator_total_year(input_test_df, taxon_key_col = "blablabla")
  )
})

test_that("Param: first_observed", {
  expect_error(
    indicator_total_year(input_test_df, first_observed = 4),
    "Argument first_observed has to be a character."
  )
  expect_error(
    indicator_total_year(input_test_df,
      first_observed = "strange_colname"
    )
  )
})

test_that("Param: labels", {
  expect_error(
    indicator_total_year(input_test_df, x_lab = input_test_df),
    "Argument x_lab has to be a character or NULL."
  )
  expect_error(
    indicator_total_year(input_test_df, y_lab = 4),
    "Argument y_lab has to be a character or NULL."
  )
})

test_that("Test warnings", {
  expect_warning(indicator_total_year(input_test_df_with_na),
    paste0(
      nrow_no_first_obs,
      " records have no information about year of introduction ",
      "(empty values in column first_observed) and are not taken ",
      "into account."
    ),
    fixed = TRUE
  )
})

test_that("Test output class", {
  # output is a list
  expect_type(plot_output_without_facets, type = "list")
  expect_type(plot_output_with_facets, type = "list")
  
  # plot slot is a list with gg as class
  expect_type(plot_output_without_facets$plot, type = "list")
  expect_type(plot_output_with_facets$plot, type = "list")
  expect_s3_class(plot_output_without_facets$plot, class = "gg")
  expect_s3_class(plot_output_with_facets$plot, class = "egg")
  
  # data_top_graph is a data.frame (tibble)
  expect_type(plot_output_with_facets$data_top_graph, type = "list")
  expect_s3_class(plot_output_with_facets$data_top_graph,
                            class = "data.frame")
  expect_s3_class(plot_output_with_facets$data_top_graph,
                            class = "tbl_df")
  expect_type(plot_output_without_facets$data_top_graph,
                        type = "list")
  expect_s3_class(plot_output_without_facets$data_top_graph,
                            class = "data.frame")
  expect_s3_class(plot_output_without_facets$data_top_graph,
                            class = "tbl_df")
  
  # data_top_graph contains only columns year and n in this order
  expect_equal(
    names(plot_output_without_facets$data_top_graph),
    c("year", "n"))
  # data_top_graph contains only columns year and n in this order
  expect_equal(
    names(plot_output_with_facets$data_top_graph),
    c("year", "n"))
  
  # data_facet_graph is NULL if faceting is deactivated
  expect_null(plot_output_without_facets$date_facet_graph)
  
  # data_facet_graph is a data.frame (tibble) if faceting is activated
  expect_type(plot_output_with_facets$data_facet_graph, type = "list")
  expect_s3_class(plot_output_with_facets$data_facet_graph,
                            class = "data.frame")
  expect_s3_class(plot_output_with_facets$data_facet_graph,
                            class = "tbl_df")
})
