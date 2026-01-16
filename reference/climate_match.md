# Create a set of climate matching outputs

This function creates a set of climate matching outputs for a species or
set of species for a region or nation.

## Usage

``` r
climate_match(
  region,
  taxon_key,
  zip_file,
  scenario = "all",
  n_limit,
  cm_limit,
  coord_unc,
  BasisOfRecord,
  maps = TRUE
)
```

## Arguments

- region:

  (optional character) the full name of the target nation or region
  region can also be a custom region (sf object).

- taxon_key:

  (character or vector) containing GBIF - taxonkey(s)

- zip_file:

  (optional character) The path (inclu. extension) of a zipfile from a
  previous GBIF-download. This zipfile should contain data of the
  species specified by the taxon_key

- scenario:

  (character) the future scenarios we are interested in. (default) all
  future scenarios are used.

- n_limit:

  (optional numeric) the minimal number of total observations a species
  must have to be included in the outputs

- cm_limit:

  (optional numeric) the minimal percentage of the total number of
  observations within the climate zones of the region a species must
  have to be included in the outputs

- coord_unc:

  (optional numeric) the maximal coordinate uncertainty a observation
  can have to be included in the analysis

- BasisOfRecord:

  (optional character) an additional filter for observations based on
  the GBIF field "BasisOfRecord"

- maps:

  (boolean) indicating whether the maps should be created. (default)
  TRUE, the maps are created.

## Value

list with:

- `unfiltered`: a dataframe containing a summary per species and climate
  classification. The climate classification is a result of a overlay of
  the observations, filtered by coord_unc & BasisOfRecord, with the
  climate at the time of observation

- `cm`: a dataframe containing the per scenario overlap with the future
  climate scenarios for the target nation or region and based on the
  `unfiltered` dataframe

- `filtered`: the climate match dataframe on which the n_limit &
  climate_limit thresholds have been applied

- `future`: a dataframe containing a list per scenario of future climate
  zones in the target nation or region

- `spatial` a sf object containing the observations used in the analysis

- `current_map` a leaflet object displaying the degree of worldwide
  climate match with the climate from 1980 till 2016

- `future_maps` a list of leaflet objects for each future climate
  scenario, displaying the degree of climate match

- `single_species_maps` a list of leaflet objects per taxon_key
  displaying the current and future climate scenarios

## Examples

``` r
if (FALSE) { # \dontrun{
region <- "europe"

# provide GBIF taxon_key(s)
taxon_key <- c(2865504, 5274858)

# download zip_file from GBIF
# goto https://www.gbif.org/occurrence/download/0001221-210914110416597

zip_file <- "./<path to zip_file>/0001221-210914110416597.zip"

# calculate all climate match outputs
# with GBIF download
climate_match(region,
              taxon_key, 
              n_limit = 90,
              cm_limit = 0.2
)
# calculate only data climate match outputs
# using a pre-downloaded zip_file
climate_match(region,
              taxon_key, 
              zip_file,
              n_limit = 90,
              cm_limit = 0.2,
              maps = FALSE
)
# calculate climate match outputs based 
# on human observations with a 100m 
# coordinate uncertainty
climate_match(region,
              taxon_key, 
              zip_file,
              n_limit = 90,
              cm_limit = 0.2,
              coord_unc = 100,
              BasisOfRecord = "HUMAN_OBSERVATION",
              maps = FALSE
)
} # }
```
