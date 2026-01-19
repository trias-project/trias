# Pathway count indicator table

Function to get number of taxa introduced by different pathways.
Possible breakpoints: taxonomic (kingdom + vertebrates/invertebrates),
temporal (lower limit year).

## Usage

``` r
get_table_pathways(
  df,
  category = NULL,
  from = NULL,
  n_species = 5,
  kingdom_names = "kingdom",
  phylum_names = "phylum",
  first_observed = "first_observed",
  species_names = "canonicalName"
)
```

## Arguments

- df:

  df.

- category:

  NULL or character. One of the kingdoms as given in GBIF:

  - "Plantae"

  - "Animalia"

  - "Fungi"

  - "Chromista"

  - "Archaea"

  - "Bacteria"

  - "Protozoa"

  - "Viruses"

  - "incertae sedis"

  It can also be one of the following not kingdoms: \#'

  - Not Chordata. Default: NULL.

- from:

  NULL or numeric. Year trade-off: if not `NULL` select only pathways
  related to taxa introduced during or after this year. Default: `NULL`.

- n_species:

  numeric. The maximum number of species to return as examples per
  pathway. For groups with less species than `n_species`, all species
  are given. Default: 5.

- kingdom_names:

  character. Name of the column of `df` containing information about
  kingdom. Default: `"kingdom"`.

- phylum_names:

  character. Name of the column of `df` containing information about
  phylum. This parameter is used only if `category` is one of:
  `"Chordata"`, `"Not Chordata"`. Default: `"phylum"`.

- first_observed:

  character. Name of the column of `df` containing information about
  year of introduction. Default: `"first_observed"`.

- species_names:

  character. Name of the column of `df` containing information about
  species names. Default: `"canonicalName"`.

## Value

a data.frame with 4 columns: `pathway_level1`, `pathway_level2`, `n`
(number of taxa) and `examples`.

## See also

Other checklist functions:
[`gbif_get_taxa()`](https://trias-project.github.io/trias/reference/gbif_get_taxa.md),
[`gbif_has_distribution()`](https://trias-project.github.io/trias/reference/gbif_has_distribution.md),
[`gbif_verify_keys()`](https://trias-project.github.io/trias/reference/gbif_verify_keys.md),
[`indicator_introduction_year()`](https://trias-project.github.io/trias/reference/indicator_introduction_year.md),
[`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md),
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
  na = "NA",
  col_types = cols(
    .default = col_character(),
    key = col_double(),
    nubKey = col_double(),
    speciesKey = col_double(),
    acceptedKey = col_double(),
    first_observed = col_double(),
    last_observed = col_double()
  )
)
get_table_pathways(data)
# Specify kingdom
get_table_pathways(data, "Plantae")
# with special categories, `Chordata` or `not Chordata`
get_table_pathways(data, "Chordata")
get_table_pathways(data, "Not Chordata")
# From 2000
get_table_pathways(data, from = 2000, first_observed = "first_observed")
# Specify number of species to include in examples
get_table_pathways(data, "Plantae", n_species = 8)
# Specify columns containing kingdom and species names
get_table_pathways(data,
  "Plantae",
  n_species = 8,
  kingdom_names = "kingdom",
  species_names = "canonicalName"
)
} # }
```
