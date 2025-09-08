# trias 3.1.1

- Fix a bug in `indicator_native_range_year()` when `response_type = "cumulative"` and `x_include_missing = TRUE` (#166). Unit-tests for `indicator_native_range_year()` are also extended.
- Add some more examples in documentation of `indicator_native_range_year()`.
- Improved the logic for determining x-axis scale steps to better handle cases with a small range of years.
- Fix `gbif_verify_keys()` and its tests. It appeared a strange anomaly while checking if a key is a valid taxon key. Also, made use for the first time of the rgbif patch (#805) to avoid GBIF API issues for Windows users (#165).

# trias 3.1.0

- Improve `indicator_native_range_year()`: Add option `x_include_missing` for including missing years as gaps on x-axis in the plot (#163).

# trias 3.0.3

- Align code in `verify_taxa()` to tidyselect 1.1.0: using an external vector in
selections was deprecated in tidyselect 1.1.0.

# trias 3.0.2

- Improve `apply_gam()` to avoid emerging status change for negligible near zero values (#150).
- Show all emerging status values in the legend of the plots returned by `apply_gam()` (#152).
- `CITATION.cff` is updated automatically via GitHub Actions (#151).

# trias 3.0.1

- Function `get_cred()` is not exported anymore (#146). Internally used only.
- Add `CITATION.cff` (#139)

# trias 3.0.0

- Allow to specify x-scale stepsize for `indicator_native_range_year()` (#143)
- Allow to specify `response_type` for `indicator_native_range_year()` to display "absolute", "relative" or "cumulative" values (inbo/alien-species-portal#119).
- Deprecate `relative` argument in `indicator_native_range_year()` in favor of `response_type` argument.
- Fix duplicate filtering for `indicator_native_range_year()` (#145)
- x-axis breaks for `indicator_native_range_year()` and `indicator_introduction_year()` are prettified by using an help function, `nice_seq()`.
