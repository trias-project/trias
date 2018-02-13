context("output_has_distribution")

testthat::test_that("testing output of has_distribution", {
expect_true(has_distribution(134086954, country = "BE"))
expect_false(has_distribution(134086954, country = "BE", status = "DOUBTFUL"))
expect_false(has_distribution(134086954, country = "BE", status = "DOUBTFUL"))
expect_true(has_distribution(134086954, 
                             country = c("BE"), 
                             status = c("PRESENT")))
expect_true(has_distribution(134086954, 
                             country = c("NL", "BE", "LU"), 
                             status = c("PRESENT",
                                        "DOUBTFUL")))
})
