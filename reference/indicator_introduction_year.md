# Plot number of new introductions per year.

Calculate how many new species has been introduced in a year.

## Usage

``` r
indicator_introduction_year(
  df,
  start_year_plot = 1920,
  smooth_span = 0.85,
  x_major_scale_stepsize = 10,
  x_minor_scale_stepsize = 5,
  facet_column = NULL,
  taxon_key_col = "key",
  first_observed = "first_observed",
  x_lab = "Year",
  y_lab = "Number of introduced alien species"
)
```

## Arguments

- df:

  A data frame.

- start_year_plot:

  Year where the plot starts from. Default: 1920.

- smooth_span:

  (numeric) Parameter for the applied
  [`loess`](https://rdrr.io/r/stats/loess.html) smoother. For more
  information on the appropriate value, see
  [`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html).
  Default: 0.85.

- x_major_scale_stepsize:

  (integer) Parameter that indicates the breaks of the x axis. Default:
  10.

- x_minor_scale_stepsize:

  (integer) Parameter that indicates the minor breaks of the x axis.
  Default: 5.

- facet_column:

  NULL or character. The column to use to create additional facet wrap
  plots underneath the main graph. When NULL, no facet graph are
  created. Valid facet options: `"family"`, `"order"`, `"class"`,
  `"phylum"`, `"kingdom"`, `"pathway_level1"`, `"locality"`,
  `"native_range"` or `"habitat"`. Default: NULL.

- taxon_key_col:

  character. Name of the column of `df` containing unique taxon IDs.
  Default: `key`.

- first_observed:

  character. Name of the column of `df` containing information about
  year of introduction. Default: `first_observed`.

- x_lab:

  NULL or character. to set or remove the x-axis label.

- y_lab:

  NULL or character. to set or remove the y-axis label.

## Value

A list with three slots:

- `plot`: ggplot2 object (or egg object if facets are used).

- `data_top_graph`: data.frame (tibble) with data used for the main plot
  (top graph) in `plot`.

- `data_facet_graph`: data.frame (tibble) with data used for the
  faceting plot in `plot`. If `facet_column` is NULL, NULL is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
library(readr)
datafile <- paste0(
  "https://raw.githubusercontent.com/trias-project/indicators/master/data/",
  "interim/data_input_checklist_indicators.tsv"
)
data <- read_tsv(datafile,
  na = "",
  col_types = cols(
    .default = col_character(),
    key = col_double(),
    nubKey = col_double(),
    speciesKey = col_double(),
    first_observed = col_double(),
    last_observed = col_double()
  )
)
# without facets
indicator_introduction_year(data)
# specify start year and smoother parameter
indicator_introduction_year(data,
  start_year_plot = 1940,
  smooth_span = 0.6
)
# with facets
indicator_introduction_year(data, facet_column = "kingdom")
# specify columns with year of first observed
indicator_introduction_year(data,
  first_observed = "first_observed"
)
# specify axis labels
indicator_introduction_year(data, x_lab = "YEAR", y_lab = NULL)
} # }
```
