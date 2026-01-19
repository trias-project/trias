# Get taxa information from GBIF

This function retrieves taxa information from GBIF. It is a higher level
function built on rgbif functions `name_usage()` and `name_lookup()`.

## Usage

``` r
gbif_get_taxa(
  taxon_keys = NULL,
  checklist_keys = NULL,
  origin = NULL,
  limit = NULL
)
```

## Arguments

- taxon_keys:

  (single numeric or character or a vector) a single key or a vector of
  keys. Not to use together with `checklist_keys`.

- checklist_keys:

  (single character or a vector) a datasetKey (character) or a vector of
  datasetkeys. Not to use together with `checklist_keys`.

- origin:

  (single character or a vector) filter by origin. It can take many
  inputs, and treated as OR (e.g., a or b or c) To be used only in
  combination with `checklist_keys`. Ignored otherwise.

- limit:

  With taxon_keys: limit number of taxa. With checklist_keys: limit
  number of taxa per each dataset. A warning is given if limit is higher
  than the length of taxon_keys or number of records in the
  checklist_keys (if string) or any of the checklist_keys (if vector)

## Value

A data.frame with all returned attributes for any taxa

## See also

Other checklist functions:
[`gbif_has_distribution()`](https://trias-project.github.io/trias/reference/gbif_has_distribution.md),
[`gbif_verify_keys()`](https://trias-project.github.io/trias/reference/gbif_verify_keys.md),
[`get_table_pathways()`](https://trias-project.github.io/trias/reference/get_table_pathways.md),
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
# A single numeric taxon_keys
gbif_get_taxa(taxon_keys = 1)
# A single character taxon_keys
gbif_get_taxa(taxon_keys = "1")
# Multiple numeric taxon_keys (vector)
gbif_get_taxa(taxon_keys = c(1, 2, 3, 4, 5, 6))
# Multiple character taxon_keys (vector)
gbif_get_taxa(taxon_keys = c("1", "2", "3", "4", "5", "6"))
# Limit number of taxa (coupled with taxon_keys)
gbif_get_taxa(taxon_keys = c(1, 2, 3, 4, 5, 6), limit = 3)
# A single checklist_keys (character)
gbif_get_taxa(checklist_keys = "b3fa7329-a002-4243-a7a7-cd066092c9a6")
# Multiple checklist_keys (vector)
gbif_get_taxa(checklist_keys = c(
  "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  "b3fa7329-a002-4243-a7a7-cd066092c9a6"
))
# Limit number of taxa (coupled with checklist_keys)
gbif_get_taxa(
  checklist_keys = c(
    "e4746398-f7c4-47a1-a474-ae80a4f18e92",
    "b3fa7329-a002-4243-a7a7-cd066092c9a6"
  ),
  limit = 30
)
# Filter by origin
gbif_get_taxa(
  checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
  origin = "source", limit = 3000
)
gbif_get_taxa(
  checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
  origin = c("source", "denormed_classification"), limit = 3000
)
} # }
```
