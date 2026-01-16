# Update the list with all downloads

This function opens a (tab-separated) text file containing all
occurrence downloads from GBIF and updates the status of all downloads
with status `RUNNING` or `PREPARING`. If the specified download is not
present it will be add.

## Usage

``` r
update_download_list(
  file,
  download_to_add,
  input_checklist,
  url_doi_base = "https://doi.org/"
)
```

## Arguments

- file:

  text file (tab separated) containing all occurrence downloads from
  GBIF. File should contain the following columns:

  - `gbif_download_key`: GBIF download keys, e.g.
    0012078-181003121212138

  - `input_checklist`: filename or link to readable text file containing
    the list of queried taxa

  - `gbif_download_created`: datetime of the GBIF download creation as
    returned by `rgbif` function `occ_download_meta`

  - `gbif_download_status`: status of the GBIF download as returned by
    as returned by `rgbif` function `occ_download_meta`, e.g. `RUNNING`,
    `PREPARING`, `SUCCEDEED`

  - `gbif_download_doi`: DOI link as returned by as returned by `rgbif`
    function `occ_download_meta` (e.g. `10.15468/dl.kg0uxf`) +
    `url_doi_base` value (see below)

- download_to_add:

  character. A GBIF download key to be added to file.

- input_checklist:

  text file with taxon keys whose occurrences you want to download

- url_doi_base:

  character. doi base URL; `url_doi_base` + doi form a link to a page
  with download information. Default: "https://doi.org/".

## Value

message with the performed updates

## Details

If a download key is passed which is not present in the file it will be
added as a new line.
