context("input_has_distribution")

testthat::test_that("distribution properties: character or vectors", {
  expect_error(has_distribution("134086893", i_am_not_a_distribution_prop = "nooo"),
               paste("Strings must match column names. Unknown columns:",
                     "i_am_not_a_distribution_prop"))
  expect_error(has_distribution(134086954, countryCode = "BE"),
               "Strings must match column names. Unknown columns: countryCode")
})
