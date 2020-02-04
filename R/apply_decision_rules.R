#' Apply decision rules to time series and assess emerging status
#'
#' This function defines and applies some decision rules to assess emerging
#' status at a specific time.
#'
#' @param df df. A dataframe containing temporal data of one or more taxa. The
#'   column with taxa can be of class character, numeric or integers.
#' @param y_var character. Name of column of \code{df} containing variable to
#'   model. It has to be passed as string, e.g. \code{"occurrences"}.
#' @param eval_year numeric. Temporal value at which emerging status has to be
#'   evaluated. \code{eval_year} should be present in timeseries of at least one
#'   taxon.
#' @param year character. Name of column of \code{df} containing temporal
#'   values. It has to be passed as string, e.g. \code{"time"}. Default:
#'   \code{"year"}.
#' @param taxonKey character. Name of column of \code{df} containing taxon IDs.
#'   It has to be passed as string, e.g. \code{"taxon"}. Default:
#'   \code{"taxonKey"}.
#'
#' @return df. A dataframe (tibble) containing emerging status. Columns:
#'   \itemize{\item{\code{taxonKey}: column containing taxon ID. Column name
#'   equal to value of argument \code{taxonKey}.} \item{\code{year}: column
#'   containing temporal values. Column name equal to value of argument
#'   \code{year}. Column itself is equal to value of argument \code{eval_year}.
#'   So, if you apply decision rules on years 2018 (\code{eval_year = 2018}),
#'   you will get 2018 in this column.} \item{\code{em_status}: numeric.
#'   Emerging status, an integer between 0 and 3, based on output of decision
#'   rules (next columns).} \item{\code{dr_1}: logical. Output of decision rule
#'   1 answers to the question: does the time series contain only one positive
#'   value at evaluation year?} \item{\code{dr_2}: logical. Output of decision
#'   rule 2 answers to the question: is value at evaluation year above median
#'   value?} \item{\code{dr_3}: logical. Output of decision rule 3 answers to
#'   the question: does the time series contains only zeros in the five years
#'   before \code{eval_year}?} \item{\code{dr_4}: logical. Output of decision
#'   rule 4 answers to the question: is the value in column \code{y_var} the
#'   maximum ever observed up to \code{eval_year}?}}
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate filter select case_when rename %>% summarize pull
#'   group_by ungroup tally add_tally inner_join anti_join tibble n_distinct n
#' @importFrom tidyselect vars_pull enquo
#' @importFrom tibble as_tibble
#' @importFrom purrr map2 reduce
#' @importFrom rlang sym !! := .data
#' @importFrom stats median
#' @examples
#' \dontrun{
#' df <- tibble(
#'   taxonID = c(rep(1008955, 10), rep(2493598, 3)),
#'   y = c(seq(2009, 2018), seq(2016, 2018)),
#'   obs = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 3, 0)
#' )
#' apply_decision_rules(df,
#'   eval_year = c(2016, 2017, 2018),
#'   y_var = "obs",
#'   taxonKey = "taxonID",
#'   year = y
#' )
#' }
#'
apply_decision_rules <- function(df,
                                 y_var = "ncells",
                                 eval_year,
                                 year = "year",
                                 taxonKey = "taxonKey") {

  # Check right type of inputs
  assert_that(is.data.frame(df),
    msg = paste(
      paste(as.character(df), collapse = ","),
      "is not a data frame.",
      "Check value of argument df."
    )
  )

  map2(
    list(y_var, year, taxonKey),
    c("y_var", "year", "taxonKey"),
    function(x, y) {
      # Check right type of inputs
      assert_that(is.character(x),
        msg = paste0(
          paste(as.character(x), collapse = ","),
          " is not a character vector.",
          " Check value of argument ", y, "."
        )
      )
      # Check y_var, taxonKey, year, eval_year have length 1
      assert_that(length(x) == 1,
        msg = paste0(
          "Multiple values for argument ",
          paste0(y, collapse = ","),
          " provided."
        )
      )
    }
  )

  assert_that(is.numeric(eval_year),
    msg = paste(
      paste(as.character(eval_year), collapse = ","),
      "is not a numeric or integer vector.",
      "Check value of argument eval_year."
    )
  )

  assert_that(length(eval_year) == 1,
    msg = paste0("Multiple values for argument eval_year provided.")
  )

  map2(
    list(y_var, year, taxonKey),
    c("y_var", "year", "taxonKey"),
    function(x, y) {
      # Check y_var, year, taxonKey are present in df
      assert_that(x %in% names(df),
        msg = paste0(
          "The column ", x,
          " is not present in df. Check value of",
          " argument ", y, "."
        )
      )
    }
  )

  year <- vars_pull(names(df), !!enquo(year))
  taxonKey <- vars_pull(names(df), !!enquo(taxonKey))

  # Check eval_year is present in column year
  assert_that(eval_year %in% df[[year]],
    msg = paste(
      "Evaluation year not present in df.",
      "Check value of argument eval_year."
    )
  )

  # Check timeseries has distinct time values per each taxon (no duplicates)
  assert_that(all(df %>%
    group_by(!!sym(taxonKey)) %>%
    summarize(
      has_distinct_years = n_distinct(!!sym(year)) == n()
    ) %>%
    distinct(.data$has_distinct_years) %>%
    pull() == TRUE),
  msg = paste0(
    "Timeseries in column ",
    year,
    " of one or more taxa contain duplicates."
  )
  )

  # Check timeseries has no holes (consecutive years)
  taxa_not_consecutive_ts <-
    df %>%
    group_by(!!sym(taxonKey)) %>%
    summarize(
      has_all_years = n() == (max(!!sym(year)) - min(!!sym(year)) + 1)
    ) %>%
    filter(.data$has_all_years == FALSE)

  assert_that(nrow(taxa_not_consecutive_ts) == 0,
    msg = paste0(
      "The timeseries of these taxa are not valid ",
      "as they contain missing time values: ",
      paste(taxa_not_consecutive_ts[[taxonKey]], collapse = ", "),
      "."
    )
  )

  # Get all taxa in df
  taxon_keys <-
    df %>%
    distinct(!!sym(taxonKey)) %>%
    pull()

  # Find taxa whose timeseries don't contain eval_year, remove them and throw a
  # warning
  taxa_eval_out_of_min_max <-
    df %>%
    group_by(!!sym(taxonKey)) %>%
    summarize(
      min_ts = min(!!sym(year)),
      max_ts = max(!!sym(year))
    ) %>%
    filter(eval_year < .data$min_ts | eval_year > .data$max_ts)

  if (nrow(taxa_eval_out_of_min_max) > 0) {
    warning(paste0(
      "Taxa with timseries not containing evaluation year (",
      eval_year,
      "): ",
      paste(taxa_eval_out_of_min_max[[taxonKey]], collapse = ", ")
    ))
    df <-
      df %>%
      filter(!(!!sym(taxonKey)) %in% taxa_eval_out_of_min_max[[taxonKey]])
  }

  # Cut time series up to eval_year
  df <-
    df %>%
    group_by(!!sym(taxonKey)) %>%
    filter(!!sym(year) <= eval_year) %>%
    ungroup()

  # Rule 1: Time series with only one positive value at evaluation year
  # appearing at eval_year
  dr_1 <-
    df %>%
    group_by(!!sym(taxonKey)) %>%
    filter(!!sym(y_var) > 0) %>%
    add_tally(wt = NULL) %>%
    mutate(dr_1 = n == 1) %>%
    distinct(!!sym(taxonKey), dr_1)

  # Rule 2: last value (at eval_year) above median value?
  dr_2 <-
    df %>%
    group_by(!!sym(taxonKey)) %>%
    mutate(last_occ = ifelse(!!sym(year) == max(!!sym(year)),
      !!sym(y_var), -1
    )) %>%
    summarize(
      median_occ = median(!!sym(y_var)),
      last_occ = max(.data$last_occ)
    ) %>%
    mutate(dr_2 = .data$last_occ > .data$median_occ) %>%
    select(!!sym(taxonKey), .data$dr_2)

  # Rule 3: 0 in the last 5 years?
  dr_3 <-
    df %>%
    group_by(!!sym(taxonKey)) %>%
    filter(!!sym(year) > (max(!!sym(year)) - 5)) %>%
    tally(!!sym(y_var)) %>%
    mutate(dr_3 = n == 0) %>%
    select(!!sym(taxonKey), .data$dr_3)

  # Rule 4: last value (at eval_year) is the maximum ever observed?
  dr_4 <-
    df %>%
    group_by(!!sym(taxonKey)) %>%
    summarize(max_occ = max(!!sym(y_var))) %>%
    inner_join(df %>%
      filter(!!sym(year) == max(!!sym(year))) %>%
      ungroup() %>%
      rename(last_value = !!sym(y_var)),
    by = taxonKey
    ) %>%
    mutate(dr_4 = .data$last_value == .data$max_occ) %>%
    select(!!sym(taxonKey), .data$dr_4)

  # Join all decision rules together
  dr_all <-
    list(dr_1, dr_2, dr_3, dr_4) %>%
    reduce(inner_join, by = taxonKey)

  # convert to em status codes:
  # 0 = not emerging
  # 1 = unclear ((re)appearing at eyear is judged as unclear too)
  # 2 = potentially emerging
  # 3 = emerging

  em_dr <-
    dr_all %>%
    mutate(em_status = case_when(
      .data$dr_3 == TRUE ~ 0, # not emerging

      .data$dr_1 == FALSE & .data$dr_2 == TRUE &
        .data$dr_3 == FALSE & .data$dr_4 == TRUE ~ 3, # emerging

      .data$dr_1 == FALSE & .data$dr_2 == TRUE &
        .data$dr_3 == FALSE & .data$dr_4 == FALSE ~ 2, # potentially emerging

      (.data$dr_1 == TRUE & .data$dr_3 == FALSE) |
        (.data$dr_1 == FALSE & .data$dr_2 == FALSE & .data$dr_3 == FALSE) ~ 1 # unclear
    )) %>%
    mutate(!!sym(year) := eval_year) %>%
    select(
      !!sym(taxonKey),
      !!sym(year),
      .data$em_status,
      .data$dr_1,
      .data$dr_2,
      .data$dr_3,
      .data$dr_4
    ) %>%
    as_tibble()

  # Taxa which will be not evaluated (no data)
  taxon_keys_to_add <- taxon_keys[!taxon_keys %in% em_dr[[taxonKey]]]

  taxa_without_em <- tibble(
    !!sym(taxonKey) := taxon_keys_to_add,
    !!sym(year) := rep(eval_year, length(taxon_keys_to_add))
  )

  em_dr <-
    em_dr %>%
    bind_rows(taxa_without_em)

  return(em_dr)
}
