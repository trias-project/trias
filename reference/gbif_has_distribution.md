# Compare desired distribution information with actual one

This function compares GBIF distribution information based on a single
taxon key with user requests and returns a logical (TRUE or FALSE).
Comparison is case insensitive. User properties for each term are
treated as OR. It is a function built on rgbif function `name_usage()`.

## Usage

``` r
gbif_has_distribution(taxon_key, ...)
```

## Arguments

- taxon_key:

  (single numeric or character) a single taxon key.

- ...:

  one or more GBIF distribution properties and related values. Up to now
  it supports the following properties: country (and its synonym:
  countryCode), status (and its synonym: occurrenceStatus) and
  establishmentMeans.

## Value

a logical, TRUE or FALSE.

## See also

Other checklist functions:
[`gbif_get_taxa()`](https://trias-project.github.io/trias/reference/gbif_get_taxa.md),
[`gbif_verify_keys()`](https://trias-project.github.io/trias/reference/gbif_verify_keys.md),
[`get_nubkeys()`](https://trias-project.github.io/trias/reference/get_nubkeys.md),
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
# IMPORTANT! 
# examples could fail as long as `status` (`occurrenceStatus`) is used due to
# an issue of the GBIF API: see https://github.com/gbif/gbif-api/issues/94

# numeric taxonKey, atomic parameters
gbif_has_distribution(145953242,
  country = "BE",
  status = "PRESENT",
  establishmentMeans = "INTRODUCED"
)

# character taxonKey, distribution properties as vectors, treated as OR
gbif_has_distribution("145953242",
  country = c("NL", "BE"),
  status = c("PRESENT", "DOUBTFUL")
)

# use alternative names: countryCode, occurrenceStatus.
# Function works. Warning is given.
gbif_has_distribution("145953242",
  countryCode = c("NL", "BE"),
  occurrenceStatus = c("PRESENT", "DOUBTFUL")
)

# Case insensitive
gbif_has_distribution("145953242",
  country = "be",
  status = "PRESENT",
  establishmentMeans = "InTrOdUcEd"
)
} # }
```
