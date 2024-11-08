# trias 3.0.0

- Allow to specify x-scale stepsize for indicator_native_range_year() (#143)
- Deprecate `relative` argument in indicator_native_range_year() in favor of `response_type` argument.
- Allow to specify response type for indicator_native_range_year() to display "absolute", "relative" or "cumulative" numbers. 
- Fix duplicate filtering for indicator_native_range_year()
- x-axis breaks for indicator_native_range_year() and indicator_introduction_year() are now determined by nice_seq()
