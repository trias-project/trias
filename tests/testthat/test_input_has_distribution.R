context("input_has_distribution")

testthat::test_that("distribution properties", {
  expect_error(has_distribution("134086893", i_am_not_a_distribution_prop = "nooo"),
               "Strings must match column names. Unknown columns: i_am_not_a_distribution_prop")
  #countryCode instead of country
  expect_error(has_distribution(134086954, countryCode = "BE"),
               "Strings must match column names. Unknown columns: countryCode")
  expect_true(has_distribution(134086954, country = "BE"))
  expect_false(has_distribution(134086954, country = "BE", status = "DOUBTFUL"))
  expect_true(has_distribution(134086954, country = "BE", status = "PRESENT"))
})
