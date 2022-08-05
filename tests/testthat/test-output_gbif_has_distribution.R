context("output_gbif_has_distribution")

testthat::test_that("gbif_has_distribution with user parameters", {
  expect_true(gbif_has_distribution(140563025, country = "BE"))
  expect_false(gbif_has_distribution(113794849, country = "BE", status = "DOUBTFUL"))
  expect_true(gbif_has_distribution(
    taxon_key = 140563025,
    country = "BE",
    # uncomment after https://github.com/gbif/gbif-api/issues/94 is solved
    # status = "PRESENT", 
    establishmentMeans = "introduced"
  ))
  expect_true(gbif_has_distribution(141265335,
    country = c("NL", "BE", "LU"),
    status = c(
      "PRESENT",
      "DOUBTFUL"
    )
  ))
  expect_true(gbif_has_distribution(
    taxon_key = 100022263,
    establishmentMeans = c("introduced", "original"),
    country = "CA", status = c("PRESENT", "ABSENT")
  ))
})

testthat::test_that("gbif_has_distribution without user parameters", {
  expect_true(gbif_has_distribution(2225776))
  expect_false(gbif_has_distribution(121483688))
})

testthat::test_that("gbif_has_distribution with multiple distributions", {
  expect_true(gbif_has_distribution(139334288, country = "CA"))
  expect_true(gbif_has_distribution(139334288,
    country = "CA",
    status = "present"
  ))
  expect_true(gbif_has_distribution(139334288,
    country = "CA",
    status = "absent"
  ))
  expect_true(gbif_has_distribution(139334288,
    country = "CA",
    status = "present",
    establishmentMeans = "native"
  ))
  expect_false(gbif_has_distribution(139334288,
    country = "CA",
    status = "absent",
    establishmentMeans = "native"
  ))
})

testthat::test_that(
  "gbif_has_distribution is case insensitive", {
  expect_true(gbif_has_distribution(140563025,
                                    country = c("bE"),
                                    establishmentMeans = "inTrODUceD"
  ))
})