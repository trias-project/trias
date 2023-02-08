context("test_output_apply_gam")

df_gam <- data.frame(
  taxonKey = rep(2224970, 19),
  canonicalName = rep("Palaemon macrodactylus", 19),
  year = as.numeric(seq(2001, 2019)),
  n_observations = c(
    1, 5, 8, 12, 18, 23, 30, 40, 60, 40, 20, 10, 1,
    3, 10, 20, 35, 50, 80
  ),
  baseline_observations = c(
    40, 50, 30, 120, 100, 30, 5000, 40, 100, 30, 20, 40,
    100, 304, 343, 423, 343, 20, 50
  ),
  stringsAsFactors = FALSE
)

df_bad <- data.frame(
  taxonKey = rep(2224970, 3),
  canonicalName = rep("Palaemon macrodactylus", 3),
  year = as.numeric(seq(2017, 2019)),
  n_observations = c(0, 0, 0),
  baseline_observations = c(0, 0, 0),
  stringsAsFactors = FALSE
)

df_bad_p_values_occupancy <- read.delim(
  test_path(
    "data_test_output_apply_gam/example_gam_taxonKey2876049_p-values_error.tsv"
  ),
  sep = "\t"
)

df_bad_gam_not_perf <- read.delim(
  test_path(
    "data_test_output_apply_gam/example_gam_taxonKey2927530_no_gam.tsv"
  ),
  sep = "\t"
)

evaluation_year <- 2018
evaluation_years <- c(2017, 2018)

basic_fm <- as.formula("n_observations ~ s(year, k = maxk, m = 3, bs = \"tp\")")
corrected_fm <- as.formula(paste0(
  "n_observations ~ ",
  "s(year, k = maxk, ",
  "m = 3, bs = \"tp\") ",
  "+ s(baseline_observations)"
))

obs_indicator <- "observations"
occ_indicator <- "occupancy"

name_sp <- "Palaemon macrodactylus"

taxon_key <- "2224970"

basic_gam <- apply_gam(
  df = df_gam,
  y_var = "n_observations",
  eval_years = evaluation_year
)

corrected_gam <- apply_gam(
  df = df_gam,
  y_var = "n_observations",
  eval_years = evaluation_year,
  baseline_var = "baseline_observations"
)

basic_gam_bad <- apply_gam(
  df = df_bad,
  y_var = "n_observations",
  eval_years = evaluation_year
)

corrected_gam_bad <- apply_gam(
  df = df_bad,
  y_var = "n_observations",
  eval_years = evaluation_year,
  baseline_var = "baseline_observations"
)

basic_gam_ys <- apply_gam(
  df = df_gam,
  y_var = "n_observations",
  eval_years = evaluation_years
)

corrected_gam_ys <- apply_gam(
  df = df_gam,
  y_var = "n_observations",
  eval_years = evaluation_years,
  baseline_var = "baseline_observations"
)

basic_gam_bad_ys <- apply_gam(
  df = df_bad,
  y_var = "n_observations",
  eval_years = evaluation_years
)

corrected_gam_bad_ys <- apply_gam(
  df = df_bad,
  y_var = "n_observations",
  eval_years = evaluation_years,
  baseline_var = "baseline_observations"
)

basic_gam_name <- apply_gam(
  df = df_gam,
  y_var = "n_observations",
  eval_years = evaluation_year,
  name = name_sp
)

basic_gam_occ <- apply_gam(
  df = df_gam,
  y_var = "n_observations",
  eval_years = evaluation_year,
  type_indicator = occ_indicator
)

basic_gam_taxon_key_as_number <- apply_gam(
  df = df_gam,
  y_var = "n_observations",
  eval_years = evaluation_year,
  taxon_key = as.integer(taxon_key)
)

basic_gam_taxon_key_as_string <- apply_gam(
  df = df_gam,
  y_var = "n_observations",
  eval_years = evaluation_year,
  taxon_key = taxon_key
)

basic_gam_taxon_key_and_name <- apply_gam(
  df = df_gam,
  y_var = "n_observations",
  eval_years = evaluation_year,
  name = name_sp,
  taxon_key = taxon_key
)

testthat::test_that("Test length of output list", {
  # length of list output is not dependent on GAM output
  testthat::expect_equal(length(basic_gam), expected = 6)
  testthat::expect_equal(length(corrected_gam), expected = 6)
  testthat::expect_equal(length(basic_gam_bad), expected = 6)
  testthat::expect_equal(length(corrected_gam_bad), expected = 6)
})

testthat::test_that("Test em_summary", {
  # em_summary is a data.frame
  testthat::expect_true(is.data.frame(basic_gam$em_summary))
  testthat::expect_true(is.data.frame(basic_gam_ys$em_summary))
  testthat::expect_true(is.data.frame(basic_gam_bad$em_summary))
  testthat::expect_true(is.data.frame(basic_gam_bad_ys$em_summary))
  testthat::expect_true(is.data.frame(corrected_gam$em_summary))
  testthat::expect_true(is.data.frame(corrected_gam_ys$em_summary))
  testthat::expect_true(is.data.frame(corrected_gam_bad$em_summary))
  testthat::expect_true(is.data.frame(corrected_gam_bad_ys$em_summary))
  # evaluation on one year
  testthat::expect_equal(nrow(basic_gam$em_summary),
    expected = length(evaluation_year)
  )
  testthat::expect_equal(nrow(corrected_gam$em_summary),
    expected = length(evaluation_year)
  )
  testthat::expect_equal(nrow(basic_gam_bad$em_summary),
    expected = length(evaluation_year)
  )
  testthat::expect_equal(nrow(corrected_gam_bad$em_summary),
    expected = length(evaluation_year)
  )
  # evaluation on two years
  testthat::expect_equal(nrow(basic_gam_ys$em_summary),
    expected = length(evaluation_years)
  )
  testthat::expect_equal(nrow(corrected_gam_ys$em_summary),
    expected = length(evaluation_years)
  )
  testthat::expect_equal(nrow(basic_gam_bad_ys$em_summary),
    expected = length(evaluation_years)
  )
  testthat::expect_equal(nrow(corrected_gam_bad_ys$em_summary),
    expected = length(evaluation_years)
  )
  # if GAM cannot be performed, em_summary and growth are NA
  testthat::expect_true(all(
    is.na(corrected_gam_bad$em_summary$em_status),
    is.na(corrected_gam_bad$em_summary$growth),
    is.na(corrected_gam_bad_ys$em_summary$em_status),
    is.na(corrected_gam_bad_ys$em_summary$growth)
  ))
  # if GAM is performed em_summary and growth are numeric
  testthat::expect_true(all(
    is.numeric(corrected_gam$em_summary$em_status),
    is.numeric(corrected_gam$em_summary$growth),
    is.numeric(corrected_gam_ys$em_summary$em_status),
    is.numeric(corrected_gam_ys$em_summary$growth)
  ))
  # if GAM is performed em_summary is one of 0,1,2,3
  testthat::expect_true(all(
    corrected_gam$em_summary$em_status %in% c(0, 1, 2, 3),
    corrected_gam_ys$em_summary$em_status %in% c(0, 1, 2, 3)
  ))
})

testthat::test_that("Test model", {

  # model output is an instance of class "gam"
  testthat::expect_true(all(class(basic_gam$model) == c("gam", "glm", "lm")))
  testthat::expect_true(all(class(basic_gam_ys$model) == c("gam", "glm", "lm")))
  testthat::expect_true(all(class(corrected_gam$model) == c("gam", "glm", "lm")))
  testthat::expect_true(all(class(corrected_gam_ys$model) == c("gam", "glm", "lm")))

  # model output is NULL if gam is not performed
  expect_null(basic_gam_bad$model)
  expect_null(basic_gam_bad_ys$model)
  expect_null(corrected_gam_bad$model)
  expect_null(corrected_gam_bad_ys$model)

  # model formula is correct
  testthat::expect_true(all(
    basic_gam$model$formula == basic_fm,
    basic_gam_ys$model$formula == basic_fm
  ))
  testthat::expect_true(all(
    corrected_gam$model$formula == corrected_fm,
    corrected_gam_ys$model$formula == corrected_fm
  ))
})

testthat::test_that("Test output", {

  # output is a data.frame
  testthat::expect_true(is.data.frame(basic_gam$output))
  testthat::expect_true(is.data.frame(basic_gam_ys$output))
  testthat::expect_true(is.data.frame(basic_gam_bad$output))
  testthat::expect_true(is.data.frame(basic_gam_bad_ys$output))
  testthat::expect_true(is.data.frame(corrected_gam$output))
  testthat::expect_true(is.data.frame(corrected_gam_ys$output))
  testthat::expect_true(is.data.frame(corrected_gam_bad$output))
  testthat::expect_true(is.data.frame(corrected_gam_bad_ys$output))
  # output has all columns of input df
  testthat::expect_true(all(names(df_gam) %in% names(basic_gam$output)))
  testthat::expect_true(all(names(df_gam) %in% names(basic_gam_ys$output)))
  testthat::expect_true(all(names(df_bad) %in% names(basic_gam_bad$output)))
  testthat::expect_true(all(names(df_bad) %in% names(basic_gam_bad_ys$output)))
  testthat::expect_true(all(names(df_gam) %in% names(corrected_gam$output)))
  testthat::expect_true(all(names(df_gam) %in% names(corrected_gam_ys$output)))
  testthat::expect_true(all(names(df_bad) %in% names(corrected_gam_bad$output)))
  testthat::expect_true(all(names(df_bad) %in% names(corrected_gam_bad_ys$output)))

  # values of these columns are also identical
  testthat::expect_true(all(df_gam$taxonKey == basic_gam$output$taxonKey))
  testthat::expect_true(all(df_gam$taxonKey == basic_gam_ys$output$taxonKey))
  testthat::expect_true(all(df_bad$taxonKey == basic_gam_bad$output$taxonKey))
  testthat::expect_true(all(df_bad$taxonKey == basic_gam_bad_ys$output$taxonKey))
  testthat::expect_true(all(df_gam$taxonKey == corrected_gam$output$taxonKey))
  testthat::expect_true(all(df_gam$taxonKey == corrected_gam_ys$output$taxonKey))
  testthat::expect_true(all(df_bad$taxonKey == corrected_gam_bad$output$taxonKey))
  testthat::expect_true(all(df_bad$taxonKey == corrected_gam_bad_ys$output$taxonKey))
  testthat::expect_true(all(df_gam$canonicalName == basic_gam$output$canonicalName))
  testthat::expect_true(all(df_gam$canonicalName == basic_gam_ys$output$canonicalName))
  testthat::expect_true(all(df_bad$canonicalName == basic_gam_bad$output$canonicalName))
  testthat::expect_true(all(df_bad$canonicalName ==
    basic_gam_bad_ys$output$canonicalName))
  testthat::expect_true(all(df_gam$canonicalName == corrected_gam$output$canonicalName))
  testthat::expect_true(all(df_gam$canonicalName == corrected_gam_ys$output$canonicalName))
  testthat::expect_true(all(df_bad$canonicalName == corrected_gam_bad$output$canonicalName))
  testthat::expect_true(all(df_bad$canonicalName ==
    corrected_gam_bad_ys$output$canonicalName))
  testthat::expect_true(all(df_gam$year == basic_gam$output$year))
  testthat::expect_true(all(df_gam$year == basic_gam_ys$output$year))
  testthat::expect_true(all(df_bad$year == basic_gam_bad$output$year))
  testthat::expect_true(all(df_bad$year == basic_gam_bad_ys$output$year))
  testthat::expect_true(all(df_gam$year == corrected_gam$output$year))
  testthat::expect_true(all(df_gam$year == corrected_gam_ys$output$year))
  testthat::expect_true(all(df_bad$year == corrected_gam_bad$output$year))
  testthat::expect_true(all(df_bad$year == corrected_gam_bad_ys$output$year))
  testthat::expect_true(all(df_gam$n_observations == basic_gam$output$n_observations))
  testthat::expect_true(all(df_gam$n_observations == basic_gam_ys$output$n_observations))
  testthat::expect_true(all(df_bad$n_observations == basic_gam_bad$output$n_observations))
  testthat::expect_true(all(df_bad$n_observations ==
    basic_gam_bad_ys$output$n_observations))
  testthat::expect_true(all(df_gam$n_observations == corrected_gam$output$n_observations))
  testthat::expect_true(all(df_gam$n_observations == corrected_gam_ys$output$n_observations))
  testthat::expect_true(all(df_bad$n_observations == corrected_gam_bad$output$n_observations))
  testthat::expect_true(all(df_bad$n_observations ==
    corrected_gam_bad_ys$output$n_observations))

  testthat::expect_true(all(df_gam$baseline_observations ==
    basic_gam$output$baseline_observations))
  testthat::expect_true(all(df_gam$baseline_observations ==
    basic_gam_ys$output$baseline_observations))
  testthat::expect_true(all(df_bad$baseline_observations ==
    basic_gam_bad$output$baseline_observations))
  testthat::expect_true(all(df_bad$baseline_observations ==
    basic_gam_bad_ys$output$baseline_observations))
  testthat::expect_true(all(df_gam$baseline_observations ==
    corrected_gam$output$baseline_observations))
  testthat::expect_true(all(df_gam$baseline_observations ==
    corrected_gam_ys$output$baseline_observations))
  testthat::expect_true(all(df_bad$baseline_observations ==
    corrected_gam_bad$output$baseline_observations))
  testthat::expect_true(all(df_bad$baseline_observations ==
    corrected_gam_bad_ys$output$baseline_observations))

  # output has columns em1, em2, em, em_status, growth, method
  testthat::expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in%
    names(basic_gam$output)))
  testthat::expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in%
    names(basic_gam_ys$output)))
  testthat::expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in%
    names(basic_gam_bad$output)))
  testthat::expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in%
    names(basic_gam_bad_ys$output)))
  testthat::expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in%
    names(corrected_gam$output)))
  testthat::expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in%
    names(corrected_gam_ys$output)))
  testthat::expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in%
    names(corrected_gam_bad$output)))
  testthat::expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in%
    names(corrected_gam_bad_ys$output)))

  # fit values are zero or above
  testthat::expect_true(all(c(
    basic_gam$output$fit,
    basic_gam$output$ucl,
    basic_gam$output$lcl,
    corrected_gam$output$fit,
    corrected_gam$output$ucl,
    corrected_gam$output$lcl
  ) >= 0))
  testthat::expect_true(all(c(
    basic_gam_ys$output$fit,
    basic_gam_ys$output$ucl,
    basic_gam_ys$output$lcl,
    corrected_gam_ys$output$fit,
    corrected_gam_ys$output$ucl,
    corrected_gam_ys$output$lcl
  ) >= 0))

  # if GAM is not performed: fit columns are NA
  testthat::expect_true(all(is.na(c(
    basic_gam_bad$output$fit,
    basic_gam_bad$output$ucl,
    basic_gam_bad$output$lcl,
    corrected_gam_bad$output$fit,
    corrected_gam_bad$output$ucl,
    corrected_gam_bad$output$lcl
  ))))
  testthat::expect_true(all(is.na(c(
    basic_gam_bad_ys$output$fit,
    basic_gam_bad_ys$output$ucl,
    basic_gam_bad_ys$output$lcl,
    corrected_gam_bad_ys$output$fit,
    corrected_gam_bad_ys$output$ucl,
    corrected_gam_bad_ys$output$lcl
  ))))

  # lcl <= fit <= ucl
  testthat::expect_true(all(basic_gam$output$lcl <= basic_gam$output$fit &
    basic_gam$output$fit <= basic_gam$output$ucl))
  testthat::expect_true(all(basic_gam_ys$output$lcl <= basic_gam_ys$output$fit &
    basic_gam_ys$output$fit <= basic_gam_ys$output$ucl))
  testthat::expect_true(all(corrected_gam$output$lcl <= corrected_gam$output$fit &
    corrected_gam$output$fit <= corrected_gam$output$ucl))
  testthat::expect_true(all(corrected_gam_ys$output$lcl <= corrected_gam_ys$output$fit &
    corrected_gam_ys$output$fit <= corrected_gam_ys$output$ucl))

  # number of years for evaluation doesn't affect fit values
  testthat::expect_true(all(basic_gam$output$lcl == basic_gam_ys$output$lcl &
    basic_gam$output$fit == basic_gam_ys$output$fit &
    basic_gam$output$ucl == basic_gam_ys$output$ucl))
  testthat::expect_true(all(corrected_gam$output$lcl == corrected_gam_ys$output$lcl &
    corrected_gam$output$fit == corrected_gam_ys$output$fit &
    corrected_gam$output$ucl == corrected_gam_ys$output$ucl))

  # em1, em2, em, em_status are numeric
  testthat::expect_true(is.numeric(basic_gam$output$em1))
  testthat::expect_true(is.numeric(basic_gam$output$em2))
  testthat::expect_true(is.numeric(basic_gam$output$em))
  testthat::expect_true(is.numeric(basic_gam$output$em_status))
  testthat::expect_true(is.numeric(corrected_gam$output$em1))
  testthat::expect_true(is.numeric(corrected_gam$output$em2))
  testthat::expect_true(is.numeric(corrected_gam$output$em))
  testthat::expect_true(is.numeric(corrected_gam$output$em_status))
  testthat::expect_true(is.numeric(basic_gam_bad$output$em1))
  testthat::expect_true(is.numeric(basic_gam_bad$output$em2))
  testthat::expect_true(is.numeric(basic_gam_bad$output$em))
  testthat::expect_true(is.numeric(basic_gam_bad$output$em_status))
  testthat::expect_true(is.numeric(corrected_gam_bad$output$em1))
  testthat::expect_true(is.numeric(corrected_gam_bad$output$em2))
  testthat::expect_true(is.numeric(corrected_gam_bad$output$em))
  testthat::expect_true(is.numeric(corrected_gam_bad$output$em_status))

  # if GAM is performed em1 em2 are one of c(-1, 0, 1),
  testthat::expect_true(all(
    basic_gam$output$em1 %in% c(-1, 0, 1),
    basic_gam$output$em2 %in% c(-1, 0, 1)
  ))
  testthat::expect_true(all(
    basic_gam_ys$output$em1 %in% c(-1, 0, 1),
    basic_gam_ys$output$em2 %in% c(-1, 0, 1)
  ))
  testthat::expect_true(all(
    corrected_gam$output$em1 %in% c(-1, 0, 1),
    corrected_gam$output$em2 %in% c(-1, 0, 1)
  ))
  testthat::expect_true(all(
    corrected_gam_ys$output$em1 %in% c(-1, 0, 1),
    corrected_gam_ys$output$em2 %in% c(-1, 0, 1)
  ))

  # if GAM is performed em is a value between -4 and +4
  testthat::expect_true(all(basic_gam$output$em <= 4 & basic_gam$output$em >= -4))
  testthat::expect_true(all(basic_gam_ys$output$em <= 4 & basic_gam$output$em >= -4))
  testthat::expect_true(all(corrected_gam$output$em <= 4 & basic_gam$output$em >= -4))
  testthat::expect_true(all(corrected_gam_ys$output$em <= 4 & basic_gam$output$em >= -4))

  # if GAM is performed em_stauts is one of 0, 1, 2, 3
  testthat::expect_true(all(basic_gam$output$em_status <= 3 &
    basic_gam$output$em_status >= 0))
  testthat::expect_true(all(basic_gam_ys$output$em_status <= 3 &
    basic_gam_ys$output$em_status >= 0))
  testthat::expect_true(all(corrected_gam$output$em_status <= 3 &
    corrected_gam$output$em_status >= 0))
  testthat::expect_true(all(corrected_gam_ys$output$em_status <= 3 &
    corrected_gam_ys$output$em_status >= 0))

  # em_status is equal to em_summary for evaluation_years
  testthat::expect_true(all((basic_gam$output %>%
    dplyr::filter(year == evaluation_year) %>%
    pull(em_status)) ==
    (basic_gam$em_summary %>%
      dplyr::filter(year == evaluation_year) %>%
      pull(em_status))))
  testthat::expect_true(all((basic_gam_ys$output %>%
    dplyr::filter(year %in% evaluation_years) %>%
    pull(em_status)) ==
    (basic_gam_ys$em_summary %>%
      dplyr::filter(year %in% evaluation_years) %>%
      pull(em_status))))
  testthat::expect_true(all((corrected_gam$output %>%
    dplyr::filter(year == evaluation_year) %>%
    pull(em_status)) ==
    (corrected_gam$em_summary %>%
      dplyr::filter(year == evaluation_year) %>%
      pull(em_status))))
  testthat::expect_true(all((corrected_gam_ys$output %>%
    dplyr::filter(year %in% evaluation_years) %>%
    pull(em_status)) ==
    (corrected_gam_ys$em_summary %>%
      dplyr::filter(year %in% evaluation_years) %>%
      pull(em_status))))
  testthat::expect_true(all(is.na(corrected_gam_bad$output %>%
    dplyr::filter(year == evaluation_year) %>%
    pull(em_status))))
  testthat::expect_true(all(is.na(corrected_gam_bad_ys$output %>%
    dplyr::filter(year %in% evaluation_years) %>%
    pull(em_status))))
  testthat::expect_true(all(is.na(corrected_gam_bad$output %>%
    dplyr::filter(year == evaluation_year) %>%
    pull(em_status))))
  testthat::expect_true(all(is.na(corrected_gam_bad_ys$output %>%
    dplyr::filter(year %in% evaluation_years) %>%
    pull(em_status))))

  # number of years for evaluation doesn't affect em1, em2, em, em_status
  testthat::expect_true(all(
    basic_gam$output$em1 == basic_gam_ys$output$em1,
    basic_gam$output$em2 == basic_gam_ys$output$em2,
    basic_gam$output$em == basic_gam_ys$output$em,
    basic_gam$output$em_status == corrected_gam_ys$output$em_status
  ))
  testthat::expect_true(all(
    corrected_gam_ys$output$em1 == corrected_gam_ys$output$em1,
    corrected_gam_ys$output$em2 == corrected_gam_ys$output$em2,
    corrected_gam_ys$output$em == corrected_gam_ys$output$em,
    corrected_gam_ys$output$em_status == corrected_gam_ys$output$em_status
  ))

  # growth
  testthat::expect_true(is.numeric(basic_gam$output$growth))
  testthat::expect_true(is.numeric(basic_gam_ys$output$growth))
  testthat::expect_true(is.numeric(basic_gam_bad$output$growth))
  testthat::expect_true(is.numeric(basic_gam_bad_ys$output$growth))
  testthat::expect_true(is.numeric(corrected_gam$output$growth))
  testthat::expect_true(is.numeric(corrected_gam_ys$output$growth))
  testthat::expect_true(is.numeric(corrected_gam_bad$output$growth))
  testthat::expect_true(is.numeric(corrected_gam_bad_ys$output$growth))

  # add some checks about values of growth?

  # method is correctly returned
  testthat::expect_true(all(c(
    basic_gam$output$method,
    basic_gam_bad$output$method,
    basic_gam_ys$output$method,
    basic_gam_bad_ys$output$method
  ) == "basic"))
  testthat::expect_true(all(c(
    corrected_gam$output$method,
    corrected_gam_bad$output$method,
    corrected_gam_ys$output$method,
    corrected_gam_bad_ys$output$method
  ) == "correct_baseline"))
})

testthat::test_that("Test first derivative", {

  # first derivative is a data.frame or is NULL
  testthat::expect_true(is.data.frame(basic_gam$first_derivative))
  testthat::expect_true(is.data.frame(basic_gam_ys$first_derivative))
  testthat::expect_true(is.data.frame(corrected_gam$first_derivative))
  testthat::expect_true(is.data.frame(corrected_gam_ys$first_derivative))
  expect_null(basic_gam_bad$first_derivative)
  expect_null(basic_gam_bad_ys$first_derivative)
  expect_null(corrected_gam_bad$first_derivative)
  expect_null(corrected_gam_bad_ys$first_derivative)

  # first derivatives has right columns
  testthat::expect_that(
    basic_gam$first_derivative,
    has_names(expected = c(
      "smooth",
      "var",
      "data",
      "derivative",
      "se",
      "crit",
      "lower",
      "upper"
    ))
  )
  testthat::expect_true(all(names(basic_gam$first_derivative) ==
    names(basic_gam_ys$first_derivative)))
  testthat::expect_true(all(names(basic_gam$first_derivative) ==
    names(corrected_gam$first_derivative)))
  testthat::expect_true(all(names(corrected_gam$first_derivative) ==
    names(corrected_gam_ys$first_derivative)))

  # smooth column contains only s(year) for basic gam
  testthat::expect_true(unique(basic_gam$first_derivative$smooth) == "s(year)")
  testthat::expect_true(unique(basic_gam_ys$first_derivative$smooth) == "s(year)")
  testthat::expect_false(all(unique(corrected_gam$first_derivative$smooth) == "s(year)"))
  testthat::expect_false(all(unique(corrected_gam_ys$first_derivative$smooth) == "s(year)"))

  # smooth column contains s(year) and s(baseline_observations) if correction
  # baseline is applied
  testthat::expect_true(all(corrected_gam$first_derivative$smooth %in%
    c("s(year)", "s(baseline_observations)")))
  testthat::expect_true(all(corrected_gam_ys$first_derivative$smooth %in%
    c("s(year)", "s(baseline_observations)")))

  # var contains always value "year" if GAM is performed
  testthat::expect_true("year" %in% basic_gam$first_derivative$var)
  testthat::expect_true("year" %in% basic_gam_ys$first_derivative$var)
  testthat::expect_true("year" %in% corrected_gam$first_derivative$var)
  testthat::expect_true("year" %in% corrected_gam_ys$first_derivative$var)

  # if correction based on column baseline_observations, baseline_observations
  # is present in var
  testthat::expect_true("baseline_observations" %in% corrected_gam$first_derivative$var)
  testthat::expect_true("baseline_observations" %in%
    corrected_gam_ys$first_derivative$var)
  testthat::expect_false("baseline_observations" %in% basic_gam$first_derivative$var)
  testthat::expect_false("baseline_observations" %in% basic_gam_ys$first_derivative$var)

  # data contains numeric values
  testthat::expect_true(is.numeric(basic_gam$first_derivative$data))
  testthat::expect_true(is.numeric(basic_gam_ys$first_derivative$data))
  testthat::expect_true(is.numeric(corrected_gam$first_derivative$data))
  testthat::expect_true(is.numeric(corrected_gam_ys$first_derivative$data))

  # data column is equal to column year of df_gam for basic gam
  expect_equivalent(round(basic_gam$first_derivative$data),
    expected = df_gam$year
  )
  expect_equivalent(round(basic_gam_ys$first_derivative$data),
    expected = df_gam$year
  )

  # columns derivative, se, crit, lower, upper are numeric
  testthat::expect_true(is.numeric(basic_gam$first_derivative$derivative))
  testthat::expect_true(is.numeric(basic_gam$first_derivative$se))
  testthat::expect_true(is.numeric(basic_gam$first_derivative$crit))
  testthat::expect_true(is.numeric(basic_gam$first_derivative$lower))
  testthat::expect_true(is.numeric(basic_gam$first_derivative$upper))
  testthat::expect_true(is.numeric(basic_gam_ys$first_derivative$derivative))
  testthat::expect_true(is.numeric(basic_gam_ys$first_derivative$se))
  testthat::expect_true(is.numeric(basic_gam_ys$first_derivative$crit))
  testthat::expect_true(is.numeric(basic_gam_ys$first_derivative$lower))
  testthat::expect_true(is.numeric(basic_gam_ys$first_derivative$upper))
  testthat::expect_true(is.numeric(corrected_gam$first_derivative$derivative))
  testthat::expect_true(is.numeric(corrected_gam$first_derivative$se))
  testthat::expect_true(is.numeric(corrected_gam$first_derivative$crit))
  testthat::expect_true(is.numeric(corrected_gam$first_derivative$lower))
  testthat::expect_true(is.numeric(corrected_gam$first_derivative$upper))
  testthat::expect_true(is.numeric(corrected_gam_ys$first_derivative$derivative))
  testthat::expect_true(is.numeric(corrected_gam_ys$first_derivative$se))
  testthat::expect_true(is.numeric(corrected_gam_ys$first_derivative$crit))
  testthat::expect_true(is.numeric(corrected_gam_ys$first_derivative$lower))
  testthat::expect_true(is.numeric(corrected_gam_ys$first_derivative$upper))

  # number of evaluation years doesn't affect 1st derivative values
  testthat::expect_that(
    basic_gam_ys$first_derivative$derivative,
    testthat::is_identical_to(basic_gam$first_derivative$derivative)
  )
  testthat::expect_that(
    corrected_gam$first_derivative$derivative,
    testthat::is_identical_to(corrected_gam$first_derivative$derivative)
  )
  testthat::expect_that(
    basic_gam_ys$first_derivative$se,
    testthat::is_identical_to(basic_gam$first_derivative$se)
  )
  testthat::expect_that(
    corrected_gam$first_derivative$se,
    testthat::is_identical_to(corrected_gam$first_derivative$se)
  )
  testthat::expect_that(
    basic_gam_ys$first_derivative$crit,
    testthat::is_identical_to(basic_gam$first_derivative$crit)
  )
  testthat::expect_that(
    corrected_gam$first_derivative$crit,
    testthat::is_identical_to(corrected_gam$first_derivative$crit)
  )
  testthat::expect_that(
    basic_gam_ys$first_derivative$lower,
    testthat::is_identical_to(basic_gam$first_derivative$lower)
  )
  testthat::expect_that(
    corrected_gam$first_derivative$lower,
    testthat::is_identical_to(corrected_gam$first_derivative$lower)
  )
  testthat::expect_that(
    basic_gam_ys$first_derivative$upper,
    testthat::is_identical_to(basic_gam$first_derivative$upper)
  )
  testthat::expect_that(
    corrected_gam$first_derivative$upper,
    testthat::is_identical_to(corrected_gam$first_derivative$upper)
  )

  # lower <= derivative <= upper
  testthat::expect_true(all(basic_gam$first_derivative$lower <=
    basic_gam$first_derivative$derivative &
    basic_gam$first_derivative$derivative <=
      basic_gam$first_derivative$upper))
  testthat::expect_true(all(corrected_gam$first_derivative$lower <=
    corrected_gam$first_derivative$derivative &
    corrected_gam$first_derivative$derivative <=
      corrected_gam$first_derivative$upper))
})

testthat::test_that("Test second derivative", {

  # second derivative is a data.frame or is NULL
  testthat::expect_true(is.data.frame(basic_gam$second_derivative))
  testthat::expect_true(is.data.frame(basic_gam_ys$second_derivative))
  testthat::expect_true(is.data.frame(corrected_gam$second_derivative))
  testthat::expect_true(is.data.frame(corrected_gam_ys$second_derivative))
  expect_null(basic_gam_bad$second_derivative)
  expect_null(basic_gam_bad_ys$second_derivative)
  expect_null(corrected_gam_bad$second_derivative)
  expect_null(corrected_gam_bad_ys$second_derivative)

  # second derivatives has right columns
  testthat::expect_that(
    basic_gam$second_derivative,
    has_names(expected = c(
      "smooth",
      "var",
      "data",
      "derivative",
      "se",
      "crit",
      "lower",
      "upper"
    ))
  )
  testthat::expect_true(all(names(basic_gam$second_derivative) ==
    names(basic_gam_ys$second_derivative)))
  testthat::expect_true(all(names(basic_gam$second_derivative) ==
    names(corrected_gam$second_derivative)))
  testthat::expect_true(all(names(corrected_gam$second_derivative) ==
    names(corrected_gam_ys$second_derivative)))

  # smooth column contains only s(year) for basic gam
  testthat::expect_true(unique(basic_gam$second_derivative$smooth) == "s(year)")
  testthat::expect_true(unique(basic_gam_ys$second_derivative$smooth) == "s(year)")
  testthat::expect_false(all(unique(corrected_gam$second_derivative$smooth) == "s(year)"))
  testthat::expect_false(all(unique(corrected_gam_ys$second_derivative$smooth) == "s(year)"))

  # smooth column contains s(year) and s(baseline_observations) if correction
  # baseline is applied
  testthat::expect_true(all(corrected_gam$second_derivative$smooth %in%
    c("s(year)", "s(baseline_observations)")))
  testthat::expect_true(all(corrected_gam_ys$second_derivative$smooth %in%
    c("s(year)", "s(baseline_observations)")))

  # var contains always value "year" if GAM is performed
  testthat::expect_true("year" %in% basic_gam$second_derivative$var)
  testthat::expect_true("year" %in% basic_gam_ys$second_derivative$var)
  testthat::expect_true("year" %in% corrected_gam$second_derivative$var)
  testthat::expect_true("year" %in% corrected_gam_ys$second_derivative$var)

  # if correction based on column baseline_observations, baseline_observations
  # is present in var
  testthat::expect_true("baseline_observations" %in% corrected_gam$second_derivative$var)
  testthat::expect_true("baseline_observations" %in%
    corrected_gam_ys$second_derivative$var)
  testthat::expect_false("baseline_observations" %in% basic_gam$second_derivative$var)
  testthat::expect_false("baseline_observations" %in% basic_gam_ys$second_derivative$var)

  # data contains numeric values
  testthat::expect_true(is.numeric(basic_gam$second_derivative$data))
  testthat::expect_true(is.numeric(basic_gam_ys$second_derivative$data))
  testthat::expect_true(is.numeric(corrected_gam$second_derivative$data))
  testthat::expect_true(is.numeric(corrected_gam_ys$second_derivative$data))

  # data column is equal to column year of df_gam for basic gam
  expect_equivalent(round(basic_gam$second_derivative$data),
    expected = df_gam$year
  )
  expect_equivalent(round(basic_gam_ys$second_derivative$data),
    expected = df_gam$year
  )

  # columns derivative, se, crit, lower, upper are numeric
  testthat::expect_true(is.numeric(basic_gam$second_derivative$derivative))
  testthat::expect_true(is.numeric(basic_gam$second_derivative$se))
  testthat::expect_true(is.numeric(basic_gam$second_derivative$crit))
  testthat::expect_true(is.numeric(basic_gam$second_derivative$lower))
  testthat::expect_true(is.numeric(basic_gam$second_derivative$upper))
  testthat::expect_true(is.numeric(basic_gam_ys$second_derivative$derivative))
  testthat::expect_true(is.numeric(basic_gam_ys$second_derivative$se))
  testthat::expect_true(is.numeric(basic_gam_ys$second_derivative$crit))
  testthat::expect_true(is.numeric(basic_gam_ys$second_derivative$lower))
  testthat::expect_true(is.numeric(basic_gam_ys$second_derivative$upper))
  testthat::expect_true(is.numeric(corrected_gam$second_derivative$derivative))
  testthat::expect_true(is.numeric(corrected_gam$second_derivative$se))
  testthat::expect_true(is.numeric(corrected_gam$second_derivative$crit))
  testthat::expect_true(is.numeric(corrected_gam$second_derivative$lower))
  testthat::expect_true(is.numeric(corrected_gam$second_derivative$upper))
  testthat::expect_true(is.numeric(corrected_gam_ys$second_derivative$derivative))
  testthat::expect_true(is.numeric(corrected_gam_ys$second_derivative$se))
  testthat::expect_true(is.numeric(corrected_gam_ys$second_derivative$crit))
  testthat::expect_true(is.numeric(corrected_gam_ys$second_derivative$lower))
  testthat::expect_true(is.numeric(corrected_gam_ys$second_derivative$upper))

  # number of evaluation years doesn't affect 2nd derivative values
  testthat::expect_that(
    basic_gam_ys$second_derivative$derivative,
    testthat::is_identical_to(basic_gam$second_derivative$derivative)
  )
  testthat::expect_that(
    corrected_gam$second_derivative$derivative,
    testthat::is_identical_to(corrected_gam$second_derivative$derivative)
  )
  testthat::expect_that(
    basic_gam_ys$second_derivative$se,
    testthat::is_identical_to(basic_gam$second_derivative$se)
  )
  testthat::expect_that(
    corrected_gam$second_derivative$se,
    testthat::is_identical_to(corrected_gam$second_derivative$se)
  )
  testthat::expect_that(
    basic_gam_ys$second_derivative$crit,
    testthat::is_identical_to(basic_gam$second_derivative$crit)
  )
  testthat::expect_that(
    corrected_gam$second_derivative$crit,
    testthat::is_identical_to(corrected_gam$second_derivative$crit)
  )
  testthat::expect_that(
    basic_gam_ys$second_derivative$lower,
    testthat::is_identical_to(basic_gam$second_derivative$lower)
  )
  testthat::expect_that(
    corrected_gam$second_derivative$lower,
    testthat::is_identical_to(corrected_gam$second_derivative$lower)
  )
  testthat::expect_that(
    basic_gam_ys$second_derivative$upper,
    testthat::is_identical_to(basic_gam$second_derivative$upper)
  )
  testthat::expect_that(
    corrected_gam$second_derivative$upper,
    testthat::is_identical_to(corrected_gam$second_derivative$upper)
  )

  # lower <= derivative <= upper
  testthat::expect_true(all(basic_gam$second_derivative$lower <=
    basic_gam$second_derivative$derivative &
    basic_gam$second_derivative$derivative <=
      basic_gam$second_derivative$upper))
  testthat::expect_true(all(corrected_gam$second_derivative$lower <=
    corrected_gam$second_derivative$derivative &
    corrected_gam$second_derivative$derivative <=
      corrected_gam$second_derivative$upper))
})

testthat::test_that("Test plot", {

  # plot is always an obejct of class ggplot
  testthat::expect_true(all(class(basic_gam$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(basic_gam_ys$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(corrected_gam$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(corrected_gam_ys$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(basic_gam_occ$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(basic_gam_taxon_key_as_number$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(basic_gam_taxon_key_as_string$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(basic_gam_taxon_key_and_name$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(basic_gam_bad$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(basic_gam_bad_ys$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(corrected_gam_bad$plot) == c("gg", "ggplot")))
  testthat::expect_true(all(class(corrected_gam_bad_ys$plot) == c("gg", "ggplot")))

  # All plot titles start with "GAM"
  title_plot <- basic_gam_name$plot$labels$title
  title_plot_occ <- basic_gam_occ$plot$labels$title
  title_plot_with_key_number <- basic_gam_taxon_key_as_number$plot$labels$title
  title_plot_with_key_string <- basic_gam_taxon_key_as_string$plot$labels$title
  title_plot_with_name <- basic_gam_name$plot$labels$title
  title_plot_with_key_and_name <- basic_gam_taxon_key_and_name$plot$labels$title
  titles <- c(
    title_plot,
    title_plot_occ,
    title_plot_with_key_number,
    title_plot_with_key_string,
    title_plot_with_name,
    title_plot_with_key_and_name
  )

  testthat::expect_true(all(substr(titles, start = 1, stop = 3) == "GAM"))

  # type of indicator follows the word GAM in title
  testthat::expect_equal(
    substr(title_plot,
      start = 5,
      stop = 5 + nchar(obs_indicator) - 1
    ),
    obs_indicator
  )
  testthat::expect_equal(
    substr(title_plot_occ,
      start = 5,
      stop = 5 + nchar(occ_indicator) - 1
    ),
    occ_indicator
  )

  # if provided, value of argument taxon_key is in title of plot
  testthat::expect_equal(
    substr(
      title_plot_with_key_string,
      start = nchar(title_plot_with_key_string) - nchar(taxon_key) + 1,
      stop = nchar(title_plot_with_key_string)
    ),
    taxon_key
  )

  testthat::expect_equal(
    substr(
      title_plot_with_key_number,
      start = nchar(title_plot_with_key_number) - nchar(taxon_key) + 1,
      stop = nchar(title_plot_with_key_number)
    ),
    taxon_key
  )

  # title of plot doens't change if taxon_key is char or numeric
  testthat::expect_equal(title_plot_with_key_string, title_plot_with_key_number)

  # if provided, value of argument name is in title of plot at the end

  testthat::expect_equal(
    substr(title_plot_with_name,
      start = nchar(title_plot_with_name) - nchar(name_sp) + 1,
      stop = nchar(title_plot_with_name)
    ),
    name_sp
  )

  # if both provided, values of taxon_key and name are in title of plot as
  # taxon_key_name
  testthat::expect_equal(
    substr(title_plot_with_key_and_name,
      start = nchar(title_plot_with_key_and_name) - nchar(name_sp) - nchar(taxon_key),
      stop = nchar(title_plot_with_key_and_name)
    ),
    paste(taxon_key, name_sp, sep = "_")
  )
})

testthat::test_that("Test warnings and messages.", {
  expect_warning(apply_gam(
    df = df_bad,
    y_var = "n_observations",
    eval_years = evaluation_year,
    baseline_var = "baseline_observations",
    verbose = TRUE
  ),
  "Too few data for applying GAM (correct_baseline).",
  fixed = TRUE
  )
  expect_warning(apply_gam(
    df = df_bad,
    y_var = "n_observations",
    eval_years = evaluation_year,
    baseline_var = "baseline_observations",
    name = "test-name",
    verbose = TRUE
  ),
  paste(
    "Too few data for applying GAM (correct_baseline)",
    "to test-name."
  ),
  fixed = TRUE
  )
  expect_warning(apply_gam(
    df = df_bad,
    y_var = "n_observations",
    eval_years = evaluation_year,
    baseline_var = "baseline_observations",
    name = "test-name",
    taxon_key = "2224970",
    verbose = TRUE
  ),
  paste(
    "Too few data for applying GAM (correct_baseline)",
    "to test-name (2224970)."
  ),
  fixed = TRUE
  )
  expect_warning(apply_gam(
    df = df_bad,
    y_var = "n_observations",
    eval_years = evaluation_year,
    baseline_var = "baseline_observations",
    taxon_key = "2224970",
    verbose = TRUE
  ),
  paste(
    "Too few data for applying GAM (correct_baseline)",
    "to taxon key: 2224970."
  ),
  fixed = TRUE
  )
  expect_warning(apply_gam(
    df = df_bad_p_values_occupancy,
    year = "year",
    y_var = "obs",
    taxonKey = "taxonKey",
    eval_years = evaluation_years,
    # type_indicator = "occupancy",
    baseline_var = "native_obs",
    verbose = TRUE
  ),
  paste(
    "GAM output cannot be used: p-values of all",
    "GAM smoothers are above 0.1."
  ),
  fixed = TRUE
  )

  expect_warning(apply_gam(df_bad_gam_not_perf,
    year = "year",
    y_var = "obs",
    taxonKey = "taxonKey",
    eval_years = evaluation_years,
    type_indicator = "observations",
    baseline_var = "native_obs",
    verbose = TRUE
  ),
  paste(
    "GAM (correct_baseline) cannot be performed or",
    "cannot converge."
  ),
  fixed = TRUE
  )

  expect_warning(
    apply_gam(df_gam,
      y_var = "year",
      eval_years = 2018,
      saveplot = FALSE,
      dir_name = "./graphs/",
      verbose = TRUE
    ),
    paste(
      "saveplot is FALSE: plots are not saved.",
      "Argument dir_name ignored."
    )
  )
})
