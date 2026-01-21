# Create an interactive plot for the number of alien species per native region and year of introduction

Based on
[countYearProvince](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/uat/reporting-grofwild/R/countYearProvince.R)
plot from reporting - rshiny - grofwildjacht

## Usage

``` r
indicator_native_range_year(
  df,
  years = NULL,
  type = c("native_range", "native_continent"),
  x_major_scale_stepsize = 10,
  x_include_missing = FALSE,
  x_lab = "year",
  y_lab = "alien species",
  response_type = c("absolute", "relative", "cumulative"),
  relative = lifecycle::deprecated(),
  taxon_key_col = "key",
  first_observed = "first_observed"
)
```

## Arguments

- df:

  input data.frame.

- years:

  (numeric) vector years we are interested to. If `NULL` (default) all
  years from minimum and maximum of years of first observation are taken
  into account.

- type:

  character, native_range level of interest should be one of
  `c("native_range", "native_continent")`. Default: `"native_range"`. A
  column called as the selected `type` must be present in `df`.

- x_major_scale_stepsize:

  (integer) Parameter that indicates the breaks of the x axis. Default:
  10.

- x_include_missing:

  (logical) if `TRUE` all consecutive years are displayed on the x-axis,
  even if 0 records are available. If `FALSE` (default) years with 0
  count will be omitted and the x-axis is compressed. Range is
  determined by either `years` if specified, otherwise by the range of
  `first_observed` column in the `df`.

- x_lab:

  character string, label of the x-axis. Default: "year".

- y_lab:

  character string, label of the y-axis. Default: "number of alien
  species".

- response_type:

  (character) summary type of response to be displayed; should be one of
  `c("absolute", "relative", "cumulative")`. Default: `"absolute"`. If
  "absolute" the number per year and location is displayed; if
  "relative" each bar is standardised per year before stacking; if
  "cumulative" the cumulative number over years per location.

- relative:

  **\[deprecated\]** (logical) If `TRUE` each bar is standardised before
  stacking. Deprecated, use `response_type = "relative"` instead.

- taxon_key_col:

  character. Name of the column of `df` containing taxon IDs. Default:
  `"key"`.

- first_observed:

  (character) Name of the column in `data` containing temporal
  information about introduction of the alien species. Expressed as
  years.

## Value

list with:

- `static_plot`: ggplot object, for a given species the observed number
  per year and per native range is plotted in a stacked bar chart.

- `interactive_plot`: plotly object, for a given species the observed
  number per year and per native range is plotted in a stacked bar
  chart.

- `data`: data displayed in the plot, as a data.frame with:

  - `year`: year at which the species were introduced.

  - `native_range`: native range of the introduced species.

  - `n`: number of species introduced from the native range for a given
    year.

  - `total`: total number of species, from all around the world,
    introduced. during a given year.

  - `perc`: percentage of species introduced from the native range for a
    given year, `n`/`total`\*100.

## See also

Other checklist functions:
[`gbif_get_taxa()`](https://trias-project.github.io/trias/reference/gbif_get_taxa.md),
[`gbif_has_distribution()`](https://trias-project.github.io/trias/reference/gbif_has_distribution.md),
[`gbif_verify_keys()`](https://trias-project.github.io/trias/reference/gbif_verify_keys.md),
[`get_nubkeys()`](https://trias-project.github.io/trias/reference/get_nubkeys.md),
[`get_table_pathways()`](https://trias-project.github.io/trias/reference/get_table_pathways.md),
[`indicator_introduction_year()`](https://trias-project.github.io/trias/reference/indicator_introduction_year.md),
[`indicator_total_year()`](https://trias-project.github.io/trias/reference/indicator_total_year.md),
[`pathways_cbd()`](https://trias-project.github.io/trias/reference/pathways_cbd.md),
[`verify_taxa()`](https://trias-project.github.io/trias/reference/verify_taxa.md),
[`visualize_pathways_level1()`](https://trias-project.github.io/trias/reference/visualize_pathways_level1.md),
[`visualize_pathways_level2()`](https://trias-project.github.io/trias/reference/visualize_pathways_level2.md),
[`visualize_pathways_year_level1()`](https://trias-project.github.io/trias/reference/visualize_pathways_year_level1.md),
[`visualize_pathways_year_level2()`](https://trias-project.github.io/trias/reference/visualize_pathways_year_level2.md)

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
data <- data[data$locality == "Belgium", ]

# Specify the type of native range we are interested in
indicator_native_range_year(data, type = "native_continent")

# Specify the years we are interested in
indicator_native_range_year(data, years = 2010:2013)
indicator_native_range_year(data, years = c(2010, 2013))

# Specify the response type
indicator_native_range_year(data, response_type = "relative")
indicator_native_range_year(data, response_type = "cumulative")

# Include missing years on the x-axis
indicator_native_range_year(
  data,
  response_type = "cumulative",
  x_include_missing = TRUE
)
} # }
```
