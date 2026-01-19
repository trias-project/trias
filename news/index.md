# Changelog

## trias 3.1.3

- Function
  [`pathways_cbd()`](https://trias-project.github.io/trias/reference/pathways_cbd.md)
  is deprecated. Use data frame `pathwayscbd` directly
  ([\#134](https://github.com/trias-project/trias/issues/134)).
- Add vignettes about checklist and occurrence based functions
  ([\#170](https://github.com/trias-project/trias/issues/170)).

## trias 3.1.2

- Allow empty pathways in
  [`visualize_pathways_level1()`](https://trias-project.github.io/trias/reference/visualize_pathways_level1.md)
  and
  [`visualize_pathways_level2()`](https://trias-project.github.io/trias/reference/visualize_pathways_level2.md)
  functions
  ([\#168](https://github.com/trias-project/trias/issues/168)).

## trias 3.1.1

- Fix a bug in
  [`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md)
  when `response_type = "cumulative"` and `x_include_missing = TRUE`
  ([\#166](https://github.com/trias-project/trias/issues/166)).
  Unit-tests for
  [`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md)
  are also extended.
- Add some more examples in documentation of
  [`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md).
- Improved the logic for determining x-axis scale steps to better handle
  cases with a small range of years.
- Fix
  [`gbif_verify_keys()`](https://trias-project.github.io/trias/reference/gbif_verify_keys.md)
  and its tests. It appeared a strange anomaly while checking if a key
  is a valid taxon key. Also, made use for the first time of the rgbif
  patch ([\#805](https://github.com/trias-project/trias/issues/805)) to
  avoid GBIF API issues for Windows users
  ([\#165](https://github.com/trias-project/trias/issues/165)).

## trias 3.1.0

- Improve
  [`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md):
  Add option `x_include_missing` for including missing years as gaps on
  x-axis in the plot
  ([\#163](https://github.com/trias-project/trias/issues/163)).

## trias 3.0.3

- Align code in
  [`verify_taxa()`](https://trias-project.github.io/trias/reference/verify_taxa.md)
  to tidyselect 1.1.0: using an external vector in selections was
  deprecated in tidyselect 1.1.0.

## trias 3.0.2

- Improve
  [`apply_gam()`](https://trias-project.github.io/trias/reference/apply_gam.md)
  to avoid emerging status change for negligible near zero values
  ([\#150](https://github.com/trias-project/trias/issues/150)).
- Show all emerging status values in the legend of the plots returned by
  [`apply_gam()`](https://trias-project.github.io/trias/reference/apply_gam.md)
  ([\#152](https://github.com/trias-project/trias/issues/152)).
- `CITATION.cff` is updated automatically via GitHub Actions
  ([\#151](https://github.com/trias-project/trias/issues/151)).

## trias 3.0.1

- Function `get_cred()` is not exported anymore
  ([\#146](https://github.com/trias-project/trias/issues/146)).
  Internally used only.
- Add `CITATION.cff`
  ([\#139](https://github.com/trias-project/trias/issues/139))

## trias 3.0.0

- Allow to specify x-scale stepsize for
  [`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md)
  ([\#143](https://github.com/trias-project/trias/issues/143))
- Allow to specify `response_type` for
  [`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md)
  to display “absolute”, “relative” or “cumulative” values
  (inbo/alien-species-portal#119).
- Deprecate `relative` argument in
  [`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md)
  in favor of `response_type` argument.
- Fix duplicate filtering for
  [`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md)
  ([\#145](https://github.com/trias-project/trias/issues/145))
- x-axis breaks for
  [`indicator_native_range_year()`](https://trias-project.github.io/trias/reference/indicator_native_range_year.md)
  and
  [`indicator_introduction_year()`](https://trias-project.github.io/trias/reference/indicator_introduction_year.md)
  are prettified by using an help function, `nice_seq()`.
