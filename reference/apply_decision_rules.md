# Apply decision rules to time series and assess emerging status

This function defines and applies some decision rules to assess emerging
status at a specific time.

## Usage

``` r
apply_decision_rules(
  df,
  y_var = "ncells",
  eval_year,
  year = "year",
  taxonKey = "taxonKey"
)
```

## Arguments

- df:

  df. A dataframe containing temporal data of one or more taxa. The
  column with taxa can be of class character, numeric or integers.

- y_var:

  character. Name of column of `df` containing variable to model. It has
  to be passed as string, e.g. `"occurrences"`.

- eval_year:

  numeric. Temporal value at which emerging status has to be evaluated.
  `eval_year` should be present in timeseries of at least one taxon.

- year:

  character. Name of column of `df` containing temporal values. It has
  to be passed as string, e.g. `"time"`. Default: `"year"`.

- taxonKey:

  character. Name of column of `df` containing taxon IDs. It has to be
  passed as string, e.g. `"taxon"`. Default: `"taxonKey"`.

## Value

df. A dataframe (tibble) containing emerging status. Columns:

- `year`: column containing temporal values. Column name equal to value
  of argument `year`. Column itself is equal to value of argument
  `eval_year`. So, if you apply decision rules on years 2018
  (`eval_year = 2018`), you will get 2018 in this column.

- `em_status`: numeric. Emerging status, an integer between 0 and 3,
  based on output of decision rules (next columns). See details for more
  information.

- `dr_1`: logical. Output of decision rule 1 answers to the question:
  does the time series contain only one positive value at evaluation
  year?

- `dr_2`: logical. Output of decision rule 2 answers to the question: is
  value at evaluation year above median value?

- `dr_3`: logical. Output of decision rule 3 answers to the question:
  does the time series contains only zeros in the five years before
  `eval_year`?

- `dr_4`: logical. Output of decision rule 4 answers to the question: is
  the value in column `y_var` the maximum ever observed up to
  `eval_year`?

## Details

Based on the decision rules output we define the emergency status value,
`em`:

- `dr_3` is `TRUE`: `em = 0` (not emerging)

- `dr_1` and `dr_3` are `FALSE`, `dr_2` and `dr_4` are `TRUE`: `em = 3`
  (emerging)

- `dr_2` is `TRUE`, all others are `FALSE`: `em = 2` (potentially
  emerging

- (`dr_1` is `TRUE` and `dr_3` is `FALSE`) or (`dr_1`, `dr_2` and `dr_3`
  are `FALSE`): `em = 1` (unclear)

## See also

Other occurrence functions:
[`apply_gam()`](https://trias-project.github.io/trias/reference/apply_gam.md)

## Examples

``` r
df <- dplyr::tibble(
  taxonID = c(rep(1008955, 10), rep(2493598, 3)),
  y = c(seq(2009, 2018), seq(2016, 2018)),
  obs = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 3, 0)
)
apply_decision_rules(df,
  eval_year = 2016,
  y_var = "obs",
  taxonKey = "taxonID",
  year = "y"
)
#> # A tibble: 2 × 7
#>   taxonID     y em_status dr_1  dr_2  dr_3  dr_4 
#>     <dbl> <dbl>     <dbl> <lgl> <lgl> <lgl> <lgl>
#> 1 1008955  2016         1 FALSE FALSE FALSE FALSE
#> 2 2493598  2016         1 TRUE  FALSE FALSE TRUE 
```
