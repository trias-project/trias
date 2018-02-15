context("output_has_distribution")

testthat::test_that("has_distribution with user parameters", {
expect_true(has_distribution(134086954, country = "BE"))
expect_false(has_distribution(134086954, country = "BE", status = "DOUBTFUL"))
expect_false(has_distribution(134086954, country = "BE", status = "DOUBTFUL"))
expect_true(has_distribution(134086954, 
                             country = c("BE"), 
                             status = c("PRESENT")))
expect_true(has_distribution(taxon_key = 140563025, 
                             country = "BE", 
                             status = "PRESENT", 
                             establishmentMeans = "introduced"))
expect_true(has_distribution(134086954, 
                             country = c("NL", "BE", "LU"), 
                             status = c("PRESENT",
                                        "DOUBTFUL")))
has_distribution(taxon_key = 140563025, 
                 establishmentMeans = c("introduced", "original"), 
                 country = "BE", status = c("PRESENT", "ABSENT"))
})

testthat::test_that("has_distribution without user parameters", {
  expect_true(has_distribution(2225776))
  expect_false(has_distribution(121483688))
})

testthat::test_that("has_distribution with multiple distributions", {
  expect_true(has_distribution(139334288, country = "CA"))
  expect_true(has_distribution(139334288, 
                               country = "CA",
                               status = "present"))
  expect_true(has_distribution(139334288, 
                               country = "CA",
                               status = "absent"))
  expect_true(has_distribution(139334288, country = "CA", 
                               status = "present", 
                               establishmentMeans = "native"))
  expect_false(has_distribution(139334288, country = "CA", 
                               status = "absent", 
                               establishmentMeans = "native"))
})
