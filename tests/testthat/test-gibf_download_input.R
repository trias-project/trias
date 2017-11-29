context("gibf_download_input")

test_that("Test gbif_download(URL)", {
  expect_error(gbif_download(
    taxa = paste0("https://raw.gihbusnt.com/trias-project/",
                  "alien-plants-belgium/afbd2805de77afd79fb",
                  "74669c403d40f1416661b/data/processed/taxon.csv"), 
    stop("The file doesn't exists.")))
})

test_that("Test gbif_download(country)", {
  expect_error(gbif_download(country = 'B', 
  stop("The country code is not a valid ISO 3166-1 alpha-2 code.")))
})

test_that("Test gbif_download(gbif_species_taxonKey)", {
  expect_error(gbif_download(
    taxa = paste0("https://raw.githubusercontent.com/trias-project/",
                  "alien-plants-belgium/afbd2805de77afd79fb74669c403d40f1416661b/",
                  "data/processed/taxon.csv"),
    country = "BE",
    output = "./data/output/gbif_downloads.csv",
    stop("gbif_species_taxonKey column not present in the csv file")))
})
