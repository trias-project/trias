context("test_indicator_introduction_year")

# test df
input_test_df <- read.delim(
  paste0("./data_test_input_graphs_indicators/",
         "test_visualization_df.tsv"),
  sep = "\t"
)

nrow_no_first_obs <- 
  nrow(input_test_df[is.na(input_test_df$first_observed),])

cleaned_input_test_df <- input_test_df[!is.na(input_test_df$first_observed),]

plot_output_without_facets <- 
  indicator_introduction_year(cleaned_input_test_df,
                       facet_column = NULL)
plot_output_with_facets <-
  indicator_introduction_year(cleaned_input_test_df,
                       facet_column = "kingdom")
testthat::test_that("Param: df", {
  expect_error(indicator_introduction_year(3),
               "df is not a data frame")
})

testthat::test_that("Param: start_year_plot", {
  expect_error(indicator_introduction_year(input_test_df,
                                    start_year_plot = "1950"),
               "Argument start_year_plot has to be a number.")
})

testthat::test_that("Param: smooth_span", {
  expect_error(indicator_introduction_year(input_test_df,
                                           smooth_span = "0.4"),
               "Argument smooth_span has to be a number between 0 and 1.")
})

testthat::test_that("Param: x_scale_stepsize", {
  expect_error(indicator_introduction_year(input_test_df,
                                    x_minor_scale_stepsize = "10"),
               "Argument x_minor_scale_stepsize has to be a number.")
  expect_error(indicator_introduction_year(input_test_df,
                                    x_major_scale_stepsize = "10"),
               "Argument x_major_scale_stepsize has to be a number.")
  expect_error(indicator_introduction_year(input_test_df,
                                    x_major_scale_stepsize = 50,
                                    x_minor_scale_stepsize = 100),
               paste0("x_major_scale_stepsize has to be greater ",
                      "than x_minor_scale_stepsize."))
})

testthat::test_that("Param: facet_column", {
  expect_error(
    indicator_introduction_year(input_test_df, facet_column = 3),
    "Argument facet_column has to be NULL or a character."
  )
  expect_error(
    indicator_introduction_year(input_test_df,
                         facet_column = "not nice"))
})

testthat::test_that("Param: taxon_key_col", {
  expect_error(
    indicator_introduction_year(input_test_df, taxon_key_col = 3),
    "Argument taxon_key_col has to be a character.")
  expect_error(
    indicator_introduction_year(input_test_df, taxon_key_col = "blablabla"))
})

testthat::test_that("Param: first_observed", {
  expect_error(
    indicator_total_year(input_test_df, first_observed = 3),
    "Argument first_observed has to be a character.")
  expect_error(
    indicator_total_year(input_test_df,
                         first_observed = "bad_colname"))
})

testthat::test_that("Param: labels", {
  expect_error(
    indicator_introduction_year(input_test_df, x_lab = input_test_df),
    "Argument x_lab has to be a character or NULL.")
  expect_error(
    indicator_introduction_year(input_test_df, y_lab = 4),
    "Argument y_lab has to be a character or NULL.")
})

testthat::test_that("Test warnings", {
  expect_warning(indicator_introduction_year(input_test_df),
                 paste0(
                   nrow_no_first_obs,
                   " records have no information about year of introduction ",
                   "(empty values in column first_observed) and are not taken ",
                   "into account."),
                 fixed = TRUE
  )
})

testthat::test_that("Test output class", {
  expect_type(plot_output_without_facets, type = "list")
  expect_type(plot_output_with_facets, type = "list")
  expect_s3_class(plot_output_without_facets, class = "gg")
  expect_s3_class(plot_output_with_facets, class = "egg")
})