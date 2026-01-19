# Check keys against GBIF Backbone Taxonomy

This function performs three checks:

- `keys` are valid GBIF taxon keys. That means that adding a key at the
  end of the URL https://www.gbif.org/species/ returns a GBIF page
  related to a taxa.

- `keys` are taxon keys of the GBIF Backbone Taxonomy checklist. That
  means that adding a key at the end of the URL
  https://www.gbif.org/species/ returns a GBIF page related to a taxa of
  the GBIF Backbone.)

- `keys` are synonyms of other taxa (taxonomicStatus neither `ACCEPTED`
  nor `DOUBTFUL`).

## Usage

``` r
gbif_verify_keys(keys, col_keys = "key")
```

## Arguments

- keys:

  (character or numeric) a vector, a list, or a data.frame containing
  the keys to verify.

- col_keys:

  (character) name of column containing keys in case `keys` is a
  data.frame.

## Value

a data.frame with the following columns:

- `key`: (numeric) keys as input keys.

- `is_taxonKey`: (logical) is the key a valid GBIF taxon key?

- `is_from_gbif_backbone`: (logical) is the key a valid taxon key from
  GBIF Backbone Taxonomy checklist?

- `is_synonym`: (logical) is the key related to a synonym (not
  `ACCEPTED` or `DOUBTFUL`)?

If a key didn't pass the first check (`is_taxonKey` = `FALSE`) then `NA`
for other two columns. If a key didn't pass the second check
(`is_from_gbif_backbone` = `FALSE`) then `is_synonym` = `NA`.

## See also

Other checklist functions:
[`gbif_get_taxa()`](https://trias-project.github.io/trias/reference/gbif_get_taxa.md),
[`gbif_has_distribution()`](https://trias-project.github.io/trias/reference/gbif_has_distribution.md),
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
# input is a vector
keys1 <- c(
  "12323785387253", # invalid GBIF taxonKey
  "194877248", # valid taxonKey, not a GBIF Backbone key
  "1000693", # a GBIF Backbone key, synonym
  "1000310", # a GBIF Backbone key, accepted
  NA, NA
)
# input is a df
keys2 <- data.frame(
  keys = keys1,
  other_col = sample.int(40, size = length(keys1)),
  stringsAsFactors = FALSE
)
# input is a named list
keys3 <- keys1
names(keys3) <- purrr::map_chr(
  c(1:length(keys3)),
  ~ paste(sample(c(0:9, letters, LETTERS), 3),
    collapse = ""
  )
)
# input keys are numeric
keys4 <- as.numeric(keys1)

gbif_verify_keys(keys1)
gbif_verify_keys(keys2, col_keys = "keys")
gbif_verify_keys(keys3)
gbif_verify_keys(keys4)
} # }
```
