# Create cumulative number of alien species indicator plot.

This function calculates the cumulative number of taxa introduced per
year. To do this, a column of input dataframe containing temporal
information about year of introduction is required.

## Usage

``` r
indicator_total_year(
  df,
  start_year_plot = 1940,
  x_major_scale_stepsize = 10,
  x_minor_scale_stepsize = 5,
  facet_column = NULL,
  taxon_key_col = "key",
  first_observed = "first_observed",
  x_lab = "Year",
  y_lab = "Cumulative number of alien species"
)
```

## Arguments

- df:

  df. Contains the data as produced by the Trias pipeline, with minimal
  columns.

- start_year_plot:

  numeric. Limit to use as start year of the plot. For scientific usage,
  the entire period could be relevant, but for policy purpose, focusing
  on a more recent period could be required. Default: 1940.

- x_major_scale_stepsize:

  integer. On which year interval labels are placed on the x axis.
  Default: 10.

- x_minor_scale_stepsize:

  integer. On which year interval minor breaks are placed on the x axis.
  Default: 5.

- facet_column:

  NULL or character. Name of the column to use to create additional
  facet wrap plots underneath the main graph. When NULL, no facet graph
  is included. It is typically one of the highest taxonomic ranks, e.g.
  `"kingdom"`, `"phylum"`, `"class"`, `"order"`, `"family"`. Other
  typical breakwdowns could be geographically related, e.g. `"country"`,
  `"locality"`, `"pathway"` of introduction or `"habitat"`. Default:
  `NULL`.

- taxon_key_col:

  character. Name of the column of `df` containing unique taxon IDs.
  Default: `key`.

- first_observed:

  character. Name of the column of `df` containing information about
  year of introduction. Default: `"first_observed"`.

- x_lab:

  NULL or character. To personalize or remove the x-axis label. Default:
  "Year.

- y_lab:

  NULL or character. To personalize or remove the y-axis label. Default:
  "Cumulative number of alien species".

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
start_year_plot <- 1900
x_major_scale_stepsize <- 25
x_minor_scale_stepsize <- 5
# without facets
indicator_total_year(data, start_year_plot, x_major_scale_stepsize)
# with facets
indicator_total_year(data, start_year_plot, facet_column = "kingdom")
# specify name of column containing year of introduction (first_observed)
indicator_total_year(data, first_observed = "first_observed")
# specify axis labels
indicator_total_year(data, x_lab = "YEAR", y_lab = NULL)
} # }
```
