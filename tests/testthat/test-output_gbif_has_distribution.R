context("output_gbif_has_distribution")

testthat::test_that("gbif_has_distribution with user parameters", {
  skip_on_os(os = "windows")
  testthat::expect_true(gbif_has_distribution(140563025, country = "BE"))
  testthat::expect_false(gbif_has_distribution(113794849, country = "BE", status = "DOUBTFUL"))
  testthat::expect_true(gbif_has_distribution(
    taxon_key = 140563025,
    country = "BE",
    establishmentMeans = "introduced"
  ))
  testthat::expect_true(gbif_has_distribution(100022263,
    country = c("GL", "CA"),
    status = c(
      "PRESENT",
      "DOUBTFUL"
    )
  ))
  testthat::expect_true(gbif_has_distribution(
    taxon_key = 100022263,
    establishmentMeans = c("introduced", "original"),
    country = "CA",
    status = c("PRESENT", "ABSENT")
  ))
})

testthat::test_that("gbif_has_distribution without user parameters", {
  skip_on_os(os = "windows")
  testthat::expect_true(gbif_has_distribution(2225776))
  testthat::expect_false(gbif_has_distribution(121483688))
})

testthat::test_that("gbif_has_distribution with multiple distributions", {
  skip_on_os(os = "windows")
  testthat::expect_true(gbif_has_distribution(139334288, country = "CA"))
  testthat::expect_true(gbif_has_distribution(139334288,
    country = "CA",
    status = "present"
  ))
  testthat::expect_true(gbif_has_distribution(139334288,
    country = "CA",
    status = "absent"
  ))
  testthat::expect_true(gbif_has_distribution(139334288,
    country = "CA",
    status = "present",
    establishmentMeans = "native"
  ))
  testthat::expect_false(gbif_has_distribution(139334288,
    country = "FR", # instead of "CA"
    status = "present",
    establishmentMeans = "native"
  ))
})

testthat::test_that(
  "gbif_has_distribution is case insensitive", {
    skip_on_os(os = "windows")
    testthat::expect_true(
      gbif_has_distribution(140563025,
                            country = c("bE"),
                            establishmentMeans = "inTrODUceD"
      )
    )
})
