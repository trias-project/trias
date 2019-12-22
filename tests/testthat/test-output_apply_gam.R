context("test_output_apply_gam")

df_gam <- data.frame(
  taxonKey = rep(2224970, 19),
  canonicalName = rep("Palaemon macrodactylus", 19),
  year = seq(2001, 2019),
  n_observations = c(1, 5, 8, 12, 18, 23, 30, 40, 60, 40, 20, 10, 1, 
                     3, 10, 20, 35, 50, 80),
  baseline_observations = c(40, 50, 30, 120, 100, 30, 5000, 40, 100, 30, 20, 40,
                            100,304, 343, 423, 343, 20, 50),
  stringsAsFactors = FALSE
)

df_bad <- data.frame(
  taxonKey = rep(2224970, 3),
  canonicalName = rep("Palaemon macrodactylus", 3),
  year = seq(2017, 2019),
  n_observations = c(0, 0, 0),
  baseline_observations = c(0, 0, 0),
  stringsAsFactors = FALSE
)

evaluation_year <- 2018
evaluation_years <- c(2017, 2018)

basic_gam <- apply_gam(df = df_gam,
                       y_var = "n_observations",
                       eval_years = evaluation_year)
corrected_gam <- apply_gam(df = df_gam,
                          y_var = "n_observations",
                          eval_years = evaluation_year, 
                          baseline_var = "baseline_observations")

basic_gam_bad <- apply_gam(df = df_bad,
                       y_var = "n_observations",
                       eval_years = evaluation_year)
corrected_gam_bad <- apply_gam(df = df_bad,
                           y_var = "n_observations",
                           eval_years = evaluation_year, 
                           baseline_var = "baseline_observations")

basic_gam_ys <- apply_gam(df = df_gam,
                       y_var = "n_observations",
                       eval_years = evaluation_years)
corrected_gam_ys <- apply_gam(df = df_gam,
                           y_var = "n_observations",
                           eval_years = evaluation_years, 
                           baseline_var = "baseline_observations")

basic_gam_bad_ys <- apply_gam(df = df_bad,
                           y_var = "n_observations",
                           eval_years = evaluation_years)
corrected_gam_bad_ys <- apply_gam(df = df_bad,
                               y_var = "n_observations",
                               eval_years = evaluation_years, 
                               baseline_var = "baseline_observations")

testthat::test_that("Test length of output list", {
  # length of list output is not dependent on GAM output
  expect_equal(length(basic_gam), expected = 6)
  expect_equal(length(corrected_gam), expected = 6)
  expect_equal(length(basic_gam_bad), expected = 6)
  expect_equal(length(corrected_gam_bad), expected = 6)
  }
)

testthat::test_that("Test em_summary", {
  # em_summary is a data.frame
  expect_true(is.data.frame(basic_gam$em_summary))
  expect_true(is.data.frame(basic_gam_ys$em_summary))
  expect_true(is.data.frame(basic_gam_bad$em_summary))
  expect_true(is.data.frame(basic_gam_bad_ys$em_summary))
  expect_true(is.data.frame(corrected_gam$em_summary))
  expect_true(is.data.frame(corrected_gam_ys$em_summary))
  expect_true(is.data.frame(corrected_gam_bad$em_summary))
  expect_true(is.data.frame(corrected_gam_bad_ys$em_summary))
  # evaluation on one year
  expect_equal(nrow(basic_gam$em_summary),
               expected = length(evaluation_year))
  expect_equal(nrow(corrected_gam$em_summary),
               expected = length(evaluation_year))
  expect_equal(nrow(basic_gam_bad$em_summary),
               expected = length(evaluation_year))
  expect_equal(nrow(corrected_gam_bad$em_summary),
               expected = length(evaluation_year))
  # evaluation on two years
  expect_equal(nrow(basic_gam_ys$em_summary),
               expected = length(evaluation_years))
  expect_equal(nrow(corrected_gam_ys$em_summary),
               expected = length(evaluation_years))
  expect_equal(nrow(basic_gam_bad_ys$em_summary),
               expected = length(evaluation_years))
  expect_equal(nrow(corrected_gam_bad_ys$em_summary),
               expected = length(evaluation_years))
  # if GAM cannot be performed, em_summary and growth are NA
  expect_true(all(is.na(corrected_gam_bad$em_summary$em_status),
                  is.na(corrected_gam_bad$em_summary$growth),
                  is.na(corrected_gam_bad_ys$em_summary$em_status),
                  is.na(corrected_gam_bad_ys$em_summary$growth)))
  # if GAM is performed em_summary and growth are numeric
  expect_true(all(is.numeric(corrected_gam$em_summary$em_status),
                  is.numeric(corrected_gam$em_summary$growth),
                  is.numeric(corrected_gam_ys$em_summary$em_status),
                  is.numeric(corrected_gam_ys$em_summary$growth)))
  # if GAM is performed em_summary is one of 0,1,2,3
  expect_true(all(corrected_gam$em_summary$em_status %in% c(0, 1, 2, 3),
                  corrected_gam_ys$em_summary$em_status %in% c(0, 1, 2, 3)))
  }
)

testthat::test_that("Test output", {
  # output is a data.frame
  expect_true(is.data.frame(basic_gam$output))
  expect_true(is.data.frame(basic_gam_ys$output))
  expect_true(is.data.frame(basic_gam_bad$output))
  expect_true(is.data.frame(basic_gam_bad_ys$output))
  expect_true(is.data.frame(corrected_gam$output))
  expect_true(is.data.frame(corrected_gam_ys$output))
  expect_true(is.data.frame(corrected_gam_bad$output))
  expect_true(is.data.frame(corrected_gam_bad_ys$output))
  # output has all columns of input df
  expect_true(all(names(df_gam) %in% names(basic_gam$output)))
  expect_true(all(names(df_gam) %in% names(basic_gam_ys$output)))
  expect_true(all(names(df_bad) %in% names(basic_gam_bad$output)))
  expect_true(all(names(df_bad) %in% names(basic_gam_bad_ys$output)))
  expect_true(all(names(df_gam) %in% names(corrected_gam$output)))
  expect_true(all(names(df_gam) %in% names(corrected_gam_ys$output)))
  expect_true(all(names(df_bad) %in% names(corrected_gam_bad$output)))
  expect_true(all(names(df_bad) %in% names(corrected_gam_bad_ys$output)))
  # values of these columns are also identical
  expect_true(all(df_gam$taxonKey == basic_gam$output$taxonKey))
  expect_true(all(df_gam$taxonKey == basic_gam_ys$output$taxonKey))
  expect_true(all(df_bad$taxonKey == basic_gam_bad$output$taxonKey))
  expect_true(all(df_bad$taxonKey == basic_gam_bad_ys$output$taxonKey))
  expect_true(all(df_gam$taxonKey == corrected_gam$output$taxonKey))
  expect_true(all(df_gam$taxonKey == corrected_gam_ys$output$taxonKey))
  expect_true(all(df_bad$taxonKey == corrected_gam_bad$output$taxonKey))
  expect_true(all(df_bad$taxonKey == corrected_gam_bad_ys$output$taxonKey))
  expect_true(all(df_gam$canonicalName == basic_gam$output$canonicalName))
  expect_true(all(df_gam$canonicalName == basic_gam_ys$output$canonicalName))
  expect_true(all(df_bad$canonicalName == basic_gam_bad$output$canonicalName))
  expect_true(all(df_bad$canonicalName == 
                    basic_gam_bad_ys$output$canonicalName))
  expect_true(all(df_gam$canonicalName == corrected_gam$output$canonicalName))
  expect_true(all(df_gam$canonicalName == corrected_gam_ys$output$canonicalName))
  expect_true(all(df_bad$canonicalName == corrected_gam_bad$output$canonicalName))
  expect_true(all(df_bad$canonicalName == 
                    corrected_gam_bad_ys$output$canonicalName))
  expect_true(all(df_gam$year == basic_gam$output$year))
  expect_true(all(df_gam$year == basic_gam_ys$output$year))
  expect_true(all(df_bad$year == basic_gam_bad$output$year))
  expect_true(all(df_bad$year == basic_gam_bad_ys$output$year))
  expect_true(all(df_gam$year == corrected_gam$output$year))
  expect_true(all(df_gam$year == corrected_gam_ys$output$year))
  expect_true(all(df_bad$year == corrected_gam_bad$output$year))
  expect_true(all(df_bad$year == corrected_gam_bad_ys$output$year))
  expect_true(all(df_gam$n_observations == basic_gam$output$n_observations))
  expect_true(all(df_gam$n_observations == basic_gam_ys$output$n_observations))
  expect_true(all(df_bad$n_observations == basic_gam_bad$output$n_observations))
  expect_true(all(df_bad$n_observations == 
                    basic_gam_bad_ys$output$n_observations))
  expect_true(all(df_gam$n_observations == corrected_gam$output$n_observations))
  expect_true(all(df_gam$n_observations == corrected_gam_ys$output$n_observations))
  expect_true(all(df_bad$n_observations == corrected_gam_bad$output$n_observations))
  expect_true(all(df_bad$n_observations == 
                    corrected_gam_bad_ys$output$n_observations))
  
  expect_true(all(df_gam$baseline_observations == 
                    basic_gam$output$baseline_observations))
  expect_true(all(df_gam$baseline_observations == 
                    basic_gam_ys$output$baseline_observations))
  expect_true(all(df_bad$baseline_observations == 
                    basic_gam_bad$output$baseline_observations))
  expect_true(all(df_bad$baseline_observations == 
                    basic_gam_bad_ys$output$baseline_observations))
  expect_true(all(df_gam$baseline_observations == 
                    corrected_gam$output$baseline_observations))
  expect_true(all(df_gam$baseline_observations == 
                    corrected_gam_ys$output$baseline_observations))
  expect_true(all(df_bad$baseline_observations == 
                    corrected_gam_bad$output$baseline_observations))
  expect_true(all(df_bad$baseline_observations == 
                    corrected_gam_bad_ys$output$baseline_observations))
  
  # output has columns em1, em2, em, em_status, growth, method
  expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in% 
                    names(basic_gam$output)))
  expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in% 
                    names(basic_gam_ys$output)))
  expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in% 
                    names(basic_gam_bad$output)))
  expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in% 
                    names(basic_gam_bad_ys$output)))
  expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in% 
                    names(corrected_gam$output)))
  expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in% 
                    names(corrected_gam_ys$output)))
  expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in% 
                    names(corrected_gam_bad$output)))
  expect_true(all(c("em1", "em2", "em", "em_status", "growth", "method") %in% 
                    names(corrected_gam_bad_ys$output)))
  
  # fit values are zero or above
  expect_true(all(c(basic_gam$output$fit, 
                    basic_gam$output$ucl,
                    basic_gam$output$lcl,
                    corrected_gam$output$fit, 
                    corrected_gam$output$ucl,
                    corrected_gam$output$lcl) > 0))
  expect_true(all(c(basic_gam_ys$output$fit, 
                    basic_gam_ys$output$ucl,
                    basic_gam_ys$output$lcl,
                    corrected_gam_ys$output$fit, 
                    corrected_gam_ys$output$ucl,
                    corrected_gam_ys$output$lcl) > 0))
  # if GAM is not performed: fit columns are NA
  expect_true(all(is.na(c(basic_gam_bad$output$fit, 
                    basic_gam_bad$output$ucl,
                    basic_gam_bad$output$lcl,
                    corrected_gam_bad$output$fit, 
                    corrected_gam_bad$output$ucl,
                    corrected_gam_bad$output$lcl))))
  expect_true(all(is.na(c(basic_gam_bad_ys$output$fit, 
                    basic_gam_bad_ys$output$ucl,
                    basic_gam_bad_ys$output$lcl,
                    corrected_gam_bad_ys$output$fit, 
                    corrected_gam_bad_ys$output$ucl,
                    corrected_gam_bad_ys$output$lcl))))
  # lcl <= fit <= ucl
  expect_true(all(basic_gam$output$lcl <= basic_gam$output$fit &
                basic_gam$output$fit <= basic_gam$output$ucl))
  expect_true(all(basic_gam_ys$output$lcl <= basic_gam_ys$output$fit &
                    basic_gam_ys$output$fit <= basic_gam_ys$output$ucl))
  expect_true(all(corrected_gam$output$lcl <= corrected_gam$output$fit &
                    corrected_gam$output$fit <= corrected_gam$output$ucl))
  expect_true(all(corrected_gam_ys$output$lcl <= corrected_gam_ys$output$fit &
                    corrected_gam_ys$output$fit <= corrected_gam_ys$output$ucl))
  # amount of years for evaluation doesn't affect fit values
  expect_true(all(basic_gam$output$lcl == basic_gam_ys$output$lcl & 
                    basic_gam$output$fit == basic_gam_ys$output$fit &
                    basic_gam$output$ucl == basic_gam_ys$output$ucl))
  expect_true(all(corrected_gam$output$lcl == corrected_gam_ys$output$lcl & 
                    corrected_gam$output$fit == corrected_gam_ys$output$fit &
                    corrected_gam$output$ucl == corrected_gam_ys$output$ucl))
  # em_status is equal to em_summary for evaluation_years
  expect_true(all((basic_gam$output %>%
                dplyr::filter(year == evaluation_year) %>%
                pull(em_status)) == 
                (basic_gam$em_summary %>%
                   dplyr::filter(year == evaluation_year) %>%
                   pull(em_status))))
  expect_true(all((basic_gam_ys$output %>%
                 dplyr::filter(year %in% evaluation_years) %>%
                 pull(em_status)) == 
                (basic_gam_ys$em_summary %>%
                   dplyr::filter(year %in% evaluation_years) %>%
                   pull(em_status))))
  expect_true(all((corrected_gam$output %>%
                     dplyr::filter(year == evaluation_year) %>%
                     pull(em_status)) == 
                    (corrected_gam$em_summary %>%
                       dplyr::filter(year == evaluation_year) %>%
                       pull(em_status))))
  expect_true(all((corrected_gam_ys$output %>%
                     dplyr::filter(year %in% evaluation_years) %>%
                     pull(em_status)) == 
                    (corrected_gam_ys$em_summary %>%
                       dplyr::filter(year %in% evaluation_years) %>%
                       pull(em_status))))
  expect_true(all(is.na(corrected_gam_bad$output %>%
                 dplyr::filter(year == evaluation_year) %>%
                 pull(em_status))))
  expect_true(all(is.na(corrected_gam_bad_ys$output %>%
                 dplyr::filter(year %in% evaluation_years) %>%
                 pull(em_status))))
  expect_true(all(is.na(corrected_gam_bad$output %>%
                          dplyr::filter(year == evaluation_year) %>%
                          pull(em_status))))
  expect_true(all(is.na(corrected_gam_bad_ys$output %>%
                          dplyr::filter(year %in% evaluation_years) %>%
                          pull(em_status))))
  # amount of years for evaluation doesn't affect em1,  em2, em, em_status
  expect_true(all(basic_gam$output$em1 == basic_gam_ys$output$em1,
                  basic_gam$output$em2 == basic_gam_ys$output$em2,
                  basic_gam$output$em == basic_gam_ys$output$em,
                  basic_gam$output$em_status == corrected_gam_ys$output$em_status))
  expect_true(all(corrected_gam_ys$output$em1 == corrected_gam_ys$output$em1,
                  corrected_gam_ys$output$em2 == corrected_gam_ys$output$em2,
                  corrected_gam_ys$output$em == corrected_gam_ys$output$em,
                  corrected_gam_ys$output$em_status == corrected_gam_ys$output$em_status))
  
  # growth
  expect_true(is.numeric(basic_gam$output$growth))
  expect_true(is.numeric(basic_gam_ys$output$growth))
  expect_true(is.numeric(basic_gam_bad$output$growth))
  expect_true(is.numeric(basic_gam_bad_ys$output$growth))
  expect_true(is.numeric(corrected_gam$output$growth))
  expect_true(is.numeric(corrected_gam_ys$output$growth))
  expect_true(is.numeric(corrected_gam_bad$output$growth))
  expect_true(is.numeric(corrected_gam_bad_ys$output$growth))
  # add some checks about values of growth?
  
  # method is correctly returned
  expect_true(all(c(basic_gam$output$method,
                    basic_gam_bad$output$method,
                    basic_gam_ys$output$method,
                    basic_gam_bad_ys$output$method) == "basic"))
  expect_true(all(c(corrected_gam$output$method,
                    corrected_gam_bad$output$method,
                    corrected_gam_ys$output$method,
                    corrected_gam_bad_ys$output$method) == "correct_baseline"))
  
  }
)
