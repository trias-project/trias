# Plot number of introduced taxa for CBD pathways level 2

Function to plot bar graph with number of taxa introduced by different
pathways at level 2, given a pathway level 1. Possible breakpoints:
taxonomic (kingdoms + vertebrates/invertebrates) and temporal (lower
limit year).

## Usage

``` r
visualize_pathways_level2(
  df,
  chosen_pathway_level1,
  category = NULL,
  from = NULL,
  facet_column = NULL,
  pathways = NULL,
  pathway_level1_names = "pathway_level1",
  pathway_level2_names = "pathway_level2",
  taxon_names = "key",
  kingdom_names = "kingdom",
  phylum_names = "phylum",
  first_observed = "first_observed",
  cbd_standard = TRUE,
  title = NULL,
  x_lab = "Number of introduced taxa",
  y_lab = "Pathways"
)
```

## Arguments

- df:

  df.

- chosen_pathway_level1:

  character. A pathway level 1. If CBD standard is followed (see
  argument `cbd_standard`), one of the level 1 pathways from
  `pathwayscbd`.

- category:

  `NULL` (default) or character. One of the kingdoms as given in GBIF or
  `Chordata` (the phylum) or `Not Chordata` (all other phyla of
  `Animalia`):

  1.  `Plantae`

  2.  `Animalia`

  3.  `Fungi`

  4.  `Chromista`

  5.  `Archaea`

  6.  `Bacteria`

  7.  `Protozoa`

  8.  `Viruses`

  9.  `incertae sedis`

  10. `Chordata`

  11. `Not Chordata`

- from:

  `NULL` or numeric. Year trade-off: if not `NULL` select only pathways
  related to taxa introduced during or after this year. Default: `NULL`.

- facet_column:

  `NULL` (default) or character. The column to use to create additional
  facet wrap bar graphs underneath the main graph. When `NULL`, no facet
  graph are created. One of `family`, `order`, `class`, `phylum`,
  `locality`, `native_range`, `habitat`. If column has another name,
  rename it before calling this function. Facet `phylum` is not allowed
  in combination with `category` equal to `"Chordata"` or
  `"Not Chordata"`. Facet `kingdom` is allowed only with category equal
  to `NULL`.

- pathways:

  character. Vector with pathways level 2 to visualize. The pathways are
  displayed following the order as in this vector. It may contain
  pathways not present in the column given by `pathway_level1_names`.

- pathway_level1_names:

  character. Name of the column of `df` containing information about
  pathways at level 1. Default: `pathway_level1`.

- pathway_level2_names:

  character. Name of the column of `df` containing information about
  pathways at level 2. Default: `pathway_level2`.

- taxon_names:

  character. Name of the column of `df` containing information about
  taxa. This parameter is used to uniquely identify taxa.

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

- cbd_standard:

  logical. If `TRUE` the values of pathway level 2 are checked based on
  CBD standard as in `pathwayscbd`. Error is returned if unmatched
  values are found. If `FALSE`, a warning is returned. Default: `TRUE`.

- title:

  `NULL` or character. Title of the graph. Default: `NULL`.

- x_lab:

  `NULL` or character. x-axis label. Default:
  `"Number of introduced taxa"`.

- y_lab:

  `NULL` or character. Title of the graph. Default: `"Pathways"`.

## Value

A list with three slots:

- `plot`: ggplot2 object (or egg object if facets are used). `NULL` if
  there are no data to plot.

- `data_top_graph`: data.frame (tibble) with data used for the main plot
  (top graph) in `plot`.

- `data_facet_graph`: data.frame (tibble) with data used for the
  faceting plot in `plot`. `NULL` is returned if `facet_column` is
  `NULL`.

## See also

Other checklist functions:
[`gbif_get_taxa()`](https://trias-project.github.io/trias/reference/gbif_get_taxa.md),
[`gbif_has_distribution()`](https://trias-project.github.io/trias/reference/gbif_has_distribution.md),
[`gbif_verify_keys()`](https://trias-project.github.io/trias/reference/gbif_verify_keys.md),
[`get_nubkeys()`](https://trias-project.github.io/trias/reference/get_nubkeys.md),
[`get_table_pathways()`](https://trias-project.github.io/trias/reference/get_table_pathways.md),
[`indicator_introduction_year()`](https://trias-project.github.io/trias/reference/indicator_introduction_year.md),
[`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md),
[`indicator_total_year()`](https://trias-project.github.io/trias/reference/indicator_total_year.md),
[`pathways_cbd()`](https://trias-project.github.io/trias/reference/pathways_cbd.md),
[`verify_taxa()`](https://trias-project.github.io/trias/reference/verify_taxa.md),
[`visualize_pathways_level1()`](https://trias-project.github.io/trias/reference/visualize_pathways_level1.md),
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
# All taxa
visualize_pathways_level2(data, chosen_pathway_level1 = "escape")

# Animalia
visualize_pathways_level2(data,
  chosen_pathway_level1 = "escape",
  category = "Animalia"
)

# Chordata
visualize_pathways_level2(
  df = data,
  chosen_pathway_level1 = "escape",
  category = "Chordata"
)

# Select some pathways only
visualize_pathways_level2(
  df = data, 
  chosen_pathway_level1 = "escape",
  pathways = c("pet", "horticulture")
)

# `pathways` can contain pathways not present in data
visualize_pathways_level2(
  df = data,
  chosen_pathway_level1 = "escape",
  category = "Chordata",
  pathways = c(
    "agriculture", # not present
    "forestry", # not present
    "pet", 
    "unknown"
  )
)

# facet phylum
visualize_pathways_level2(
  df = data,
  chosen_pathway_level1 = "escape",
  category = "Animalia",
  facet_column = "phylum"
)

# facet habitat
visualize_pathways_level2(
  df = data,
  chosen_pathway_level1 = "escape",
  facet_column = "habitat"
)

# Only taxa introduced from 1950
visualize_pathways_level2(
  df = data,
  chosen_pathway_level1 = "escape",
  from = 1950
)

# Add a title
visualize_pathways_level2(
  df = data,
  chosen_pathway_level1 = "escape",
  category = "Plantae",
  from = 1950,
  title = "Pathway level 2 (escape): Plantae, from 1950"
)

# Personalize axis labels
visualize_pathways_level2(
  df = data,
  chosen_pathway_level1 = "escape",
  x_lab = "Aantal taxa",
  y_lab = "pathways"
)
} # }
```
