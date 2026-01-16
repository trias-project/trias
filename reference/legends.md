# Legends for climate shapefiles

Legends for climate shapefiles

## Usage

``` r
legends
```

## Format

`legends` contains two data.frames, `KG_A1FI` and `KG_Beck`, matching
Koppen-Geiger climate zones to A1FI and Beck scenarios respectively.

Each data.frame contains two columns:

- `GRIDCODE`: (numeric) grid value corresponding to a climate zone

- `Classification`: (character) Koppen-Geiger climate classification
  value

- `Description`: (character) verbose description of the Koppen-Geiger
  climate zone, e.g. "Tropical rainforest climate"

- `Group`: (character) group the Koppen-Geiger climate zone belongs to,
  e.g. "Tropical"

- `Precipitation Type`: (character) Type of precipitations associated to
  the climate zone, e.g. "Rainforest"

- `Level of Heat`: (character) Heat level associated to the climate
  zone, e.g. "Cold"

## Source

[Koppen-Geiger climate
zones](https://hess.copernicus.org/articles/11/1633/2007/)

## See also

Other climate data:
[`future`](https://trias-project.github.io/trias/reference/future.md),
[`observed`](https://trias-project.github.io/trias/reference/observed.md)
