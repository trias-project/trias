# Retrieve unique GBIF Backbone taxon keys from a species checklist

This function is a wrapper around
[`rgbif::name_usage()`](https://docs.ropensci.org/rgbif/reference/name_usage.html)
to retrieve the (unique) GBIF Backbone taxon keys (nubKeys) from a
species checklist identified by its datasetKey. It allows to choose
whether to return synonyms or accepted taxa only. When
`allow_synonyms`is FALSE, the function makes individual API calls for
each nubKey in a loop. For checklists with many taxa, this could result
in a large number of sequential API requests and it can take a long time
to complete.

## Usage

``` r
get_nubkeys(datasetKey, allow_synonyms = TRUE)
```

## Arguments

- datasetKey:

  (character) Unique identifier of a GBIF species checklist.

- allow_synonyms:

  (logical) Default: TRUE. If `FALSE`, the accepted taxa the synonyms
  refer to are returned instead of the synonyms themselves.

## Value

A (unique) vector of GBIF Backbone taxon keys (nubKeys). If
`allow_synonyms` is `TRUE`, the keys are retrieved from
`rgbif::name_usage()$data` directly. If `allow_synonyms` is `FALSE`, the
accepted taxa keys are retrieved by calling
[`rgbif::name_usage()`](https://docs.ropensci.org/rgbif/reference/name_usage.html)
for each synonym key obtained from `rgbif::name_usage()$data` and
extracting the accepted taxa keys from them. The final keys are returned
as unique values.

## Details

Synonym relationships within the checklist itself are not taken into
account.

## See also

Other checklist functions:
[`gbif_get_taxa()`](https://trias-project.github.io/trias/reference/gbif_get_taxa.md),
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
dataset_key <- "e082b10e-476f-43c1-aa61-f8d92f33029a"
# Retrieve all GBIF Backbone taxon keys: synonyms are included
get_nubkeys(datasetKey = dataset_key)
#>  [1] 4561827 7505521 2295425 4567449 2294131 7520678 5190912 4568269 5795017
#> [10] 4598225 4598226 2296797 2296805 4565039 8014163 9291405 2294277 2296807
#> [19] 4562917 4565134 8624880 4565061 4565140 4564944 7540164 8745918 4567717
#> [28] 2295309 8045384
# Retrieve accepted taxa GBIF Backbone taxon keys
get_nubkeys(datasetKey = dataset_key, allow_synonyms = FALSE)
#>  [1] 4561827 7505521 2295425 4567449 2294131 7520678 5190912 4568269 5795017
#> [10] 4598225 4598226 9890104 2296805 4565039 8014163 2294275 2294277 2296807
#> [19] 4562917 4565134 8624880 4565061 4565140 4564944 7540164 8745918 4567717
#> [28] 2295309 8045384
```
