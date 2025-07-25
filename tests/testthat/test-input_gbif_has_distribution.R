context("input_gbif_has_distribution")

testthat::test_that("distribution properties: character or vectors", {
  skip_on_os(os = "windows")
  testthat::expect_error(
    gbif_has_distribution("134086893", country = "BE", country = "NL"),
    paste(
      "Duplicates in property assignments.",
      "Use vectors for multiple assignments."
    )
  )
  testthat::expect_error(
    gbif_has_distribution("134086893", i_am_not_a_distribution_prop = "no!no!"),
    "Invalid distribution properties."
  )

  testthat::expect_error(
    gbif_has_distribution(134086954,
      countryValue = "BE",
      status = "PRESENT"
    ),
    "Invalid distribution properties."
  )

  testthat::expect_error(
    gbif_has_distribution(134086954,
      country = "BE",
      lifeStatus = "PRESENT"
    ),
    "Invalid distribution properties."
  )

  testthat::expect_warning(
    gbif_has_distribution(134086954, occurrenceStatus = "PRESENT"),
    "occurrenceStatus renamed to status"
  )

  testthat::expect_warning(
    gbif_has_distribution(134086954, countryCode = "NL"),
    "countryCode renamed to country"
  )

  testthat::expect_warning(
    gbif_has_distribution(134086954,
      status = "PRESENT",
      occurrenceStatus = "PRESENT"
    ),
    paste(
      "status and occurrenceStatus are alternative names of the",
      "same property. occurrenceStatus removed."
    )
  )

  testthat::expect_warning(
    gbif_has_distribution(134086954,
      countryCode = "NL",
      country = "BE"
    ),
    paste(
      "country and countryCode are alternative names of the",
      "same property. countryCode removed."
    )
  )
})
