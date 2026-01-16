# Historical climate sf objects

These sf objects contain worldwide climate classifications for different
year intervals.

## Usage

``` r
observed
```

## Format

`observed` is a list of 5 sf objects:

- `1901-1925`: observed climate data from 1901 up to 1925

- `1925-1950`: observed climate data from 1926 up to 1950

- `1950- 1975`: observed climate data from 1951 up to 1975

- `1976-2000`: observed climate data from 1976 up to 2000

- `1980-2016`: observed climate data from 1980 up to 2016

Each sf object contains 3 variables:

- `ID`: polygon identifier

- `GRIDCODE`: grid value corresponding to a climate zone

- `geometry`: the coordinates that define the polygon's shape

## Source

These objects originate from [Rubel & Kottek
2010](http://dx.doi.org/10.1127/0941-2948/2010/0430), except the last
one, with data from 1980 to 2016, which is based on [Beck et al.
2018](https://doi.org/10.1038/sdata.2018.214).

## See also

Other climate data:
[`future`](https://trias-project.github.io/trias/reference/future.md),
[`legends`](https://trias-project.github.io/trias/reference/legends.md)
