# trias 3.0.0

- Allow to specify x-scale stepsize for `indicator_native_range_year()` (#143)
- Allow to specify `response_type` for `indicator_native_range_year()` to display "absolute", "relative" or "cumulative" values (inbo/alien-species-portal#119).
- Deprecate `relative` argument in `indicator_native_range_year()` in favor of `response_type` argument.
- Fix duplicate filtering for `indicator_native_range_year()` (#145)
- x-axis breaks for `indicator_native_range_year()` and `indicator_introduction_year()` are prettified by using an help function, `nice_seq()`.
