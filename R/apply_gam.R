#' Apply GAM to time series and assess emerging status
#'
#' This function applies generalized additive models (GAM) to assess emerging
#' status for a certain time window.
#'
#' @param df df. A dataframe containing temporal data.
#' @param y_var character. Name of column containing variable to model. It has
#'   to be passed as string, e.g. \code{"occurrences"}.
#' @param eval_years numeric. Temporal value(s) when emerging status has to be
#'   evaluated.
#' @param year character. Name of column containing temporal values. It has to
#'   be passed as string, e.g. \code{"time"}. Default: \code{"year"}.
#' @param taxonKey character. Name of column containing taxon IDs. It has to be
#'   passed as string, e.g. \code{"taxon"}. Default: \code{"taxonKey"}.
#' @param type_indicator character. One of \code{"observations"},
#'   \code{"occupancy"}. Used in title of the output plot. Default:
#'   \code{"observations"}.
#' @param baseline_var character. Name of the column containing values to use
#'   as additional covariate. Such covariate is introduced in the model to
#'   correct research effort bias. Default: \code{NULL}. If \code{NULL} internal
#'   variable \code{method_em = "basic"}, otherwise \code{method_em =
#'   "correct_baseline"}. Value of \code{method_em} will be part of title of
#'   output plot.
#' @param  p_max numeric. A value between 0 and 1. Default: 0.1.
#' @param taxon_key numeric, character. Taxon key the timeseries belongs to.
#'   Used exclusively in graph title and filename (if \code{saveplot = TRUE}).
#'   Default: \code{NULL}.
#' @param name character. Species name the timeseries belongs to. Used
#'   exclusively in graph title and filename (if \code{saveplot = TRUE}).
#'   Default: \code{NULL}.
#' @param df_title character. Any string you would like to add to graph titles
#'   and filenames (if \code{saveplot = TRUE}). The title is always composed of:
#'   \code{"GAM"} + \code{type_indicator} + \code{method_em} + \code{taxon_key}
#'   + \code{name} + \code{df_title} separated by underscore ("_"). Default:
#'   \code{NULL}.
#' @param x_label character. x-axis label of output plot. Default:
#'   \code{"year"}.
#' @param y_label character. y-axis label of output plot. Default:
#'   \code{"number of observations"}.
#' @param saveplot logical. If \code{TRUE} the plots are authomatically saved.
#'   Default: \code{FALSE}.
#' @param dir_name character. Path of directory where saving plots. If path
#'   doesn't exists, directory will be created. Example: "./output/graphs/". If
#'   \code{NULL}, plots are saved in current directory. Default: \code{NULL}.
#' @param verbose logical. If \code{TRUE} status of processing and possible
#'   issues are returned. Default: \code{FALSE}.
#'
#' @return list. List with six objects: \itemize{\item{\code{em_summary}: df. A
#'   data.frame summarizing the emerging status outputs. \code{em_summary}
#'   contains as many rows as the length of input variable \code{eval_year}. So,
#'   if you evaluate GAM on three years, \code{em_summary} will contain three
#'   rows. Columns: \itemize{\item{\code{"taxonKey"}: column containing taxon
#'   ID. Column name equal to value of argument \code{taxonKey}.}
#'   \item{\code{"year"}: column containing temporal values. Column name equal
#'   to value of argument \code{year}. Column itself is equal to value of
#'   argument \code{eval_years}. So, if you evaluate GAM on years 2017, 2018
#'   (\code{eval_years = c(2017, 2018)}), you will get these two values in this
#'   column.} \item{\code{em_status}: numeric. Emerging statuses, an integer
#'   between 0 and 3.} \item{\code{growth}: numeric. Lower limit of GAM
#'   confidence interval for the first derivative. It represents the lower
#'   guaranteed growth.} \item{\code{method}: character. GAM method, as defined
#'   by internal variable \code{method_em}: character. One of:
#'   \code{"correct_baseline"} and \code{"basic"}. See details above in
#'   description of argument \code{use_baseline}.}}} \item{\code{model}: gam
#'   object. The model as returned by GAM.} \item{\code{first_derivative}: df.
#'   Data.frame with details of first derivatives. It contains following
#'   columns: \itemize{\item{\code{smooth}: smoooth identifier. Example:
#'   \code{s(year)}.} \item{\code{var}: character. Column name the smoother is
#'   applied to.} \item{\code{data}: numeric. Data in columns defined by
#'   \code{var}.} \item{\code{derivative}: numeric. Value of first derivative.}
#'   \item{\code{se}: numeric. Standard error of \code{derivative}.}
#'   \item{\code{crit}: numeric. Critical value required such that
#'   \code{derivative + (se * crit)} and \code{derivative - (se * crit)} form
#'   the upper and lower bounds of the confidence interval on the first
#'   derivative of the estimated smooth at the specific confidence level. In our
#'   case the confidence level is hard-coded: 0.8. Then \code{crit <- qnorm(p =
#'   (1-0.8)/2, mean = 0, sd = 1, lower.tail = FALSE)}.}
#'   \item{\code{lower}: numeric. Lower bound of the confidence interval of the
#'   estimated smooth.} \item{\code{upper}: numeric. Upper bound of the
#'   confidence interval of the estimated smooth.}}}
#'   \item{\code{second_derivative}: df. Data.frame with details of second
#'   derivatives. Same columns as \code{first_derivatives}.} \item{\code{plot}:
#'   a ggplot2 object. Plot of GAM output and emerging. \code{NULL} if GAM
#'   cannot be applied.}}
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate filter select case_when rename left_join full_join
#'   %>%
#' @importFrom tidyselect vars_pull enquo
#' @importFrom tibble as_tibble
#' @importFrom purrr map2
#' @importFrom rlang sym !! :=
#' @importFrom stringr str_ends str_sub
#' @importFrom mgcv nb gam summary.gam
#' @importFrom stats formula predict
#' @importFrom gratia derivatives
#'
#' @examples
#' \dontrun{
#' df_gam <- tibble(
#'   taxonKey = rep(3003709, 24),
#'   canonicalName = rep("Rosa glauca", 24),
#'   year = seq(1995, 2018),
#'   n = c(
#'     1, 1, 0, 0, 0, 2, 0, 0, 1, 3, 1, 2, 0, 5, 0, 5, 4, 2, 1,
#'     1, 3, 3, 8, 10
#'   ),
#'   n_class = c(
#'     229, 555, 1116, 939, 919, 853, 442, 532, 623, 1178, 732, 371, 1053,
#'     1001, 1550, 1142, 1076, 1310, 922, 1773, 1637,
#'     1866, 2234, 2013
#'   )
#' )
#' # apply GAM to n without baseline as covariate
#' apply_gam(df_gam,
#'   y_var = "n",
#'   eval_years = 2018,
#'   taxon_key = 3003709,
#'   name = "Rosa glauca",
#'   verbose = TRUE
#' )
#' # apply GAM using baseline data in column n_class as covariate
#' apply_gam(df_gam,
#'   y_var = "n",
#'   eval_years = 2018,
#'   baseline_var = "n_class",
#'   taxon_key = 3003709,
#'   name = "Rosa glauca",
#'   verbose = TRUE
#' )
#' # apply GAM using n as occupancy values, evaluate on two years. No baseline
#' apply_gam(df_gam,
#'   y_var = "n",
#'   eval_years = c(2017, 2018),
#'   taxon_key = 3003709,
#'   type_indicator = "occupancy",
#'   name = "Rosa glauca",
#'   y_label = "occupancy",
#'   verbose = TRUE
#' )
#' # apply GAM using n as occupancy values and n_class as covariate (baseline)
#' apply_gam(df_gam,
#'   y_var = "n",
#'   eval_years = c(2017, 2018),
#'   baseline_var = "n_class",
#'   taxon_key = 3003709,
#'   type_indicator = "occupancy",
#'   name = "Rosa glauca",
#'   y_label = "occupancy",
#'   verbose = TRUE
#' )
#' # How to modify other arguments
#' apply_gam(df_gam,
#'   y_var = "n",
#'   eval_years = c(2017, 2018),
#'   baseline_var = "n_class",
#'   p_max = 0.3,
#'   taxon_key = "3003709",
#'   type_indicator = "occupancy",
#'   name = "Rosa glauca",
#'   df_title = "Belgium",
#'   x_label = "time (years)",
#'   y_label = "area of occupancy (km2)",
#'   saveplot = TRUE,
#'   dir_name = "./data/",
#'   verbose = TRUE
#' )
#' }

apply_gam <- function(df,
                      y_var,
                      eval_years,
                      year = "year",
                      taxonKey = "taxonKey",
                      type_indicator = "observations",
                      baseline_var = NULL,
                      p_max = 0.1,
                      taxon_key = NULL,
                      name = NULL,
                      df_title = NULL,
                      x_label = "year",
                      y_label = "Observations",
                      saveplot = FALSE,
                      dir_name = NULL,
                      verbose = FALSE) {
  if (is.numeric(taxon_key)) {
    taxon_key <- as.character(taxon_key)
  }

  # Check right type of inputs
  assert_that(is.data.frame(df),
    msg = paste(
      paste(as.character(df), collapse = ","),
      "is not a data frame.",
      "Check value of argument df."
    )
  )
  map2(
    list(y_var, year, taxonKey, type_indicator, x_label, y_label),
    c("y_var", "year", "taxonKey", "type_indicator", "x_label", "y_label"),
    function(x, y) {
      # Check right type of inputs
      assert_that(is.character(x),
        msg = paste0(
          paste(as.character(x), collapse = ","),
          " is not a character vector.",
          " Check value of argument ", y, "."
        )
      )
      # Check y_var, year, taxonKey, type_indicator have length 1
      assert_that(length(x) == 1,
        msg = paste0(
          "Multiple values for argument ",
          paste0(y, collapse = ","),
          " provided."
        )
      )
    }
  )
  assert_that(is.numeric(eval_years),
    msg = paste(
      paste(as.character(eval_years), collapse = ","),
      "is not a numeric or integer vector.",
      "Check value of argument eval_years."
    )
  )

  map2(
    list(baseline_var, taxon_key, name, df_title, dir_name),
    c("baseline_var", "taxon_key", "name", "df_title", "dir_name"),
    function(x, y) {
      # check argument type
      assert_that(is.null(x) | is.character(x),
        msg = paste0(
          paste(as.character(x), collapse = ","),
          " is not a character vector.",
          " Check value of argument ", y, "."
        )
      )
      # check length
      assert_that(length(x) < 2,
        msg = paste(
          "Multiple values for argument",
          y, "provided."
        )
      )
    }
  )

  map2(
    list(saveplot, verbose),
    c("saveplot", "verbose"),
    function(x, y) {
      assert_that(is.logical(x),
        msg = paste(
          paste(as.character(x), collapse = ","),
          "is not a logical vector.",
          "Check value of argument saveplot.",
          "Did you maybe use quotation marks?"
        )
      )
      assert_that(length(x) == 1,
        msg = paste("Multiple values for argument", y, "provided.")
      )
    }
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

  if (!is.null(baseline_var)) {
    # Check baseline_var is present in df
    assert_that(
      baseline_var %in% names(df),
      msg = paste0(
        "The column ", baseline_var,
        " is not present in df. Check value of argument baseline_var."
      )
    )
    method_em <- "correct_baseline"
  } else {
    method_em <- "basic"
  }

  if (isFALSE(saveplot)) {
    if (!is.null(dir_name)) {
      warning(paste(
        "saveplot is FALSE: plots are not saved.",
        "Argument dir_name ignored."
      ))
    }
  } else {
    if (!is.null(dir_name)) {
      dir.create(dir_name, showWarnings = FALSE)
    } else {
      # current directory
      dir_name <- "./"
    }
  }

  year <- vars_pull(names(df), !!enquo(year))
  taxonKey <- vars_pull(names(df), !!enquo(taxonKey))

  # Check eval_year is present in column year
  assert_that(all(eval_years %in% df[[year]]),
    msg = paste(
      "One or more evaluation years",
      "not present in df.",
      "Check value of argument eval_years."
    )
  )

  assert_that(is.numeric(p_max) && p_max >= 0 && p_max <= 1,
    msg = paste(
      "p_max is a p-value: it has to be a",
      "number between 0 and 1."
    )
  )

  # Check type_indicator is one of the two allowed values
  assert_that(type_indicator %in% c("observations", "occupancy"),
    msg = paste(
      "Invalid type_indicator.",
      "type_indicator has to be one of:",
      "observations, occupancy."
    )
  )

  if (verbose == TRUE) {
    print(paste0("Analyzing: ", name, "(", taxon_key, ")"))
  }

  if (nrow(df) > 0) {
    # Maximum minimum time series (year)
    fyear <- min(df[[year]], na.rm = TRUE) # first year
    lyear <- max(df[[year]], na.rm = TRUE) # last year

    # Define model to use for GAM
    maxk <- max(round((lyear - fyear) / 10, digits = 0), 5) # max number of knots
  }
  if (method_em == "correct_baseline") {
    fm <- paste0(
      y_var,
      " ~ s(",
      year,
      ", k = maxk, m = 3, bs = \"tp\") + s(",
      baseline_var,
      ")"
    )
    fm <- formula(fm)
  } else {
    method_em <- "basic"
    fm <- paste0(
      y_var,
      " ~ s(",
      year,
      ", k = maxk, m = 3, bs = \"tp\")"
    )
    fm <- formula(fm)
  }

  # Initialization
  output_model <- as_tibble(df)
  output_model <-
    output_model %>%
    mutate(
      fit = NA_real_,
      ucl = NA_real_,
      lcl = NA_real_,
      em1 = NA_real_,
      em2 = NA_real_,
      em = NA_real_,
      em_status = NA_real_,
      growth = NA_real_,
      method = method_em
    )
  model <- deriv1 <- deriv2 <- plot_gam <- summary_pv <- p_ok <- NULL
  emerging_status_output <-
    output_model %>%
    filter(!!sym(year) %in% eval_years) %>%
    select(!!sym(taxonKey), year, em_status, growth, method)

  if (nrow(df) > 3 & sum(df[[y_var]][2:nrow(df)]) != 0) {
    result <- tryCatch(expr = {
      model <- gam(
        formula = fm,
        family = nb(),
        data = df,
        method = "REML"
      )
      # Check that p-value of at least one smoother < 0.1
      summary_pv <- summary.gam(model)$s.pv
      p_ok <- ifelse(any(summary_pv < p_max), TRUE, FALSE)
    }, error = function(e) e, warning = function(w) w)

    if (class(result)[1] %in% c("simpleWarning", "simpleError")) {
      if (verbose) {
        warning(paste0(
          "GAM (",
          method_em,
          ") cannot be performed or cannot converge.\n"
        ))
      }
    } else {
      if (isFALSE(p_ok)) {
        if (verbose) {
          warning(paste0(
            "GAM output cannot be used: ",
            "p-values of all GAM smoothers are above ",
            p_max, ".\n"
          ))
        }
      } else {
        output_model <- df
        # Add method
        output_model <-
          output_model %>%
          mutate(method = method_em)
        # Predict to new data (5 values per year)
        temp <- predict(
          object = model,
          newdata = output_model,
          type = "iterms",
          interval = "prediction",
          se.fit = TRUE
        )

        # Calculate confidence intervals & backtransform to real scale
        intercept <- unname(model$coefficients[1])
        output_model$fit <- model$family$linkinv(temp$fit[, 1] + intercept)
        output_model$ucl <- model$family$linkinv(temp$fit[, 1] + intercept + temp$se.fit[, 1] * 1.96)
        output_model$lcl <- model$family$linkinv(temp$fit[, 1] + intercept - temp$se.fit[, 1] * 1.96)

        # Check that fit ucl and lcl are all above zero
        output_model <-
          output_model %>%
          mutate(
            fit = ifelse(fit < 0, 0, fit),
            ucl = ifelse(ucl < 0, 0, ucl),
            lcl = ifelse(lcl < 0, 0, lcl)
          )

        # Calculate first and second derivative + conf. interval
        deriv1 <- derivatives(model,
          type = "central", order = 1, level = 0.8,
          n = nrow(output_model), eps = 1e-4
        )
        deriv2 <- derivatives(model,
          type = "central", order = 2, level = 0.8,
          n = nrow(output_model), eps = 1e-4
        )

        # Emerging status based on first and second derivative
        em1 <-
          deriv1 %>%
          as_tibble() %>%
          filter(var == year) %>%
          mutate(em1 = case_when(
            lower < 0 & upper <= 0 ~ -1,
            lower < 0 & upper > 0 ~ 0,
            lower >= 0 & upper > 0 ~ 1
          )) %>%
          select(!!sym(year) := data, em1) %>%
          mutate(!!sym(year) := round(!!sym(year)))

        em2 <- deriv2 %>%
          as_tibble() %>%
          filter(var == year) %>%
          mutate(em2 = case_when(
            .$lower < 0 & .$upper <= 0 ~ -1,
            .$lower < 0 & .$upper > 0 ~ 0,
            .$lower >= 0 & .$upper > 0 ~ 1
          )) %>%
          select(!!sym(year) := data, em2) %>%
          mutate(!!sym(year) := round(!!sym(year)))

        em_level_gam <- full_join(em1, em2, by = year) %>%
          mutate(em = case_when(
            em1 == 1 & em2 == 1 ~ 4,
            em1 == 1 & em2 == 0 ~ 3,
            em1 == 1 & em2 == -1 ~ 2,
            em1 == 0 & em2 == 1 ~ 1,
            em1 == 0 & em2 == 0 ~ 0,
            em1 == 0 & em2 == -1 ~ -1,
            em1 == -1 & em2 == 1 ~ -2,
            em1 == -1 & em2 == 0 ~ -3,
            em1 == -1 & em2 == -1 ~ -4
          ))

        # Emerging status
        em_levels <-
          em_level_gam %>%
          mutate(em_status = case_when(
            em < 0 ~ 0, # not emerging
            em == 0 ~ 1, # unclear
            em < 3 ~ 2, # potentially emerging
            em >= 3 ~ 3 # emerging
          ))

        output_model <- left_join(output_model, em_levels, by = year)

        # Lower value of first dedrivative (minimal guaranted growth) if positive
        lower_deriv1 <-
          deriv1 %>%
          filter(var == year) %>%
          rename(!!sym(year) := data) %>%
          mutate(!!sym(year) := round(!!sym(year), digits = 0)) %>%
          mutate(growth = model$family$linkinv(lower)) %>%
          # mutate(growth = ifelse(lower >= 0, lower, NA_real_)) %>%
          select(!!sym(year), growth)

        # Add lower value of first derivative
        output_model <- left_join(output_model, lower_deriv1, by = "year")

        # Get emergin status summary for output
        emerging_status_output <-
          output_model %>%
          filter(!!sym(year) %in% eval_years) %>%
          select(!!sym(taxonKey), year, em_status, growth, method)

        # Create plot with conf. interval + colour for status
        ptitle <- paste("GAM",
          type_indicator,
          method_em,
          sep = "_"
        )
        if (!is.null(taxon_key)) {
          ptitle <- paste(ptitle, taxon_key, sep = "_")
        }
        if (!is.null(name)) {
          ptitle <- paste(ptitle, name, sep = "_")
        }
        if (!is.null(df_title)) {
          ptitle <- paste(ptitle, df_title, sep = "_")
        }
        plot_gam <- plot_ribbon_em(
          df_plot = output_model,
          x_axis = year,
          y_axis = y_var,
          x_label = x_label,
          y_label = y_label,
          ptitle = ptitle
        )
        if (saveplot == TRUE) {
          if (str_ends(dir_name, pattern = "/")) {
            # remove "/" at the end
            dir_name <- str_sub(dir_name, end = -2)
          }
          file_name <- paste0(dir_name, "/", ptitle, ".png")
          if (isTRUE(verbose)) {
            print(paste("Output plot:", file_name))
          }
          ggsave(filename = file_name, plot_gam)
        }
      }
    }
  } else {
    if (verbose) {
      if (!is.null(name) & !is.null(taxon_key)) {
        warning(paste0(
          "Too few data for applying GAM (",
          method_em,
          ") to ", name, " (", taxon_key, ").\n"
        ))
      } else {
        if (!is.null(name)) {
          warning(paste0(
            "Too few data for applying GAM (",
            method_em, ") to ", name, ".\n"
          ))
        } else {
          if (!is.null(taxon_key)) {
            warning(paste0(
              "Too few data for applying GAM (",
              method_em,
              ") to taxon key: ", taxon_key, ".\n"
            ))
          } else {
            warning(paste0(
              "Too few data for applying GAM (",
              method_em, ").\n"
            ))
          }
        }
      }
    }
  }

  return(list(
    em_summary = emerging_status_output,
    model = model,
    output = output_model,
    first_derivative = deriv1,
    second_derivative = deriv2,
    plot = plot_gam
  ))
}

#' Plot time series with confidence limits and emerging status
#'
#' @param df_plot: df. A data.frame containing data to plot.
#' @param x_axis: character. Name of column containing x-values. Default:
#'   \code{"year"}.
#' @param y_axis: character. Name of column containing y-values. Default:
#'   \code{"number of observations"}.
#' @param x_label: character. x-axis label. Default: \code{"x"}.
#' @param y_label: character. y-axis label. Default: \code{"y"}.
#' @param ptitle: character. Plot title. Default: \code{NULL}.
#' @param verbose: logical. If \code{TRUE}, informations about possible issues
#'   are returned. Default: \code{FALSE}.
#' @return a ggplot2 plot object.
#' @importFrom ggplot2 ggsave ggplot geom_point ylab ggtitle geom_ribbon
#'   element_text
#'   geom_line scale_colour_manual theme aes
#' @importFrom dplyr case_when %>%
plot_ribbon_em <- function(df_plot,
                           x_axis = "year",
                           y_axis = "obs",
                           x_label = "x",
                           y_label = "y",
                           ptitle = NULL,
                           verbose = FALSE) {
  colors_em <- c(
    "3" = "darkred",
    "2" = "orangered",
    "1" = "grey50",
    "0" = "darkgreen"
  )
  labels_em <- c(
    "3" = "emerging (3)",
    "2" = "pot. emerging (2)",
    "1" = "unclear (1)",
    "0" = "not emerging (0)"
  )
  g <-
    df_plot %>%
    ggplot(aes(x = year, y = get(y_axis))) +
    geom_point(color = "black") +
    ylab(y_label) +
    ggtitle(ptitle)

  if (all(
    all(abs(df_plot$lcl < 10^10)),
    all(abs(df_plot$ucl < 10^10)),
    all(abs(df_plot$fit < 10^10))
  )) {
    g <- g +
      geom_ribbon(aes(ymax = ucl, ymin = lcl),
        fill = grey(0.5),
        alpha = 0.4
      ) +
      geom_line(aes(x = year, y = fit), color = "grey50") +
      geom_point(aes(
        x = year,
        y = fit,
        color = factor(em_status)
      ),
      size = 2
      ) +
      scale_colour_manual(
        values = colors_em,
        labels = labels_em,
        name = "Emerging status"
      ) +
      theme(plot.title = element_text(size = 10))
  } else {
    if (isTRUE(verbose)) {
      warning(paste(
        "Fit values are too big to plot.",
        "Probably GAM fit values do not converge.",
        "Please, check carefully."
      ))
    }
  }

  return(g)
}
