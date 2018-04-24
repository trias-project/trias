#' @importFrom readr read_tsv
context("output_gbif_verify_synonyms")

# correct inputs
taxa_in <- data.frame(
  backbone_taxonKey = c(2360181, 2427092, 2651108, 5228676),
  backbone_scientificName = c("Aspius aspius (Linnaeus, 1758)",
                              "Rana catesbeiana Shaw, 1802",
                              "Polystichum tsus-simense (Hook.) J.Sm.",
                              "Apus apus (Linnaeus, 1758)"),
  backbone_acceptedKey = c(5851603, 2427091, 4046493, 5228676),
  backbone_accepted = c("Leuciscus aspius (Linnaeus, 1758)",
                        "Lithobates catesbeianus (Shaw, 1802)",
                        "Polystichum luctuosum (Kunze) Moore.",
                        "Apus apus (Linnaeus, 1758)"),
  backbone_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM", "ACCEPTED"),
  stringsAsFactors = FALSE)

verified_synonyms_in <- data.frame(
  backbone_taxonKey = c(2427092,
                        2651108,
                        6723),
  backbone_scientificName = c("Rana catesbeiana Shaw, 1802",
                              "Polystichum tsus-tsus-tsus (Hook.) Captain",
                              "Lemnaceae"),
  backbone_accepted = c("Lithobates dummyus (Batman, 2018)",
                        "Polystichum luctuosum (Kunze) Moore.",
                        "Araceae"),
  backbone_acceptedKey = c(2427091,
                           4046493,
                           6979),
  backbone_taxonomicStatus = c("SYNONYM", 
                               "SYNONYM", 
                               "SYNONYM"),
  date_added = as.Date(c("2018-01-01",
                         "2018-01-01",
                         "2018-01-01")),
  verified_key = c(2427091,
                   4046493,
                   6979),
  remarks = c("dummy example 1: backbone_accepted should be updated",
              "dummy example 2: backbone_scientificName should be updated",
              "dummy example 3: nothing should be changed"),
  stringsAsFactors = FALSE)

# correct output
output <- gbif_verify_synonyms(taxa = taxa_in, 
                               verified_synonyms = verified_synonyms_in)

testthat::test_that("output type", {
  expect_type(output, "list")
  expect_true(is.data.frame(output$verified_synonyms))
  expect_true(is.data.frame(output$new_synonyms))
  expect_true(is.data.frame(output$unused_synonyms))
  expect_true(is.data.frame(output$updated_scientificName))
  expect_true(is.data.frame(output$updated_accepted))})


output_verified_synonyms <- 
  readr::read_tsv(file = paste0("./data_test_output_gbif_verify_synonyms/",
                               "output_verified_synonyms.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    backbone_taxonKey = readr::col_number(),
                    backbone_acceptedKey = readr::col_number(),
                    verified_key = readr::col_number()))
output_new_synonyms <- 
  readr::read_tsv(file = paste0("./data_test_output_gbif_verify_synonyms/",
                                "output_new_synonyms.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    backbone_taxonKey = readr::col_number(),
                    backbone_acceptedKey = readr::col_number()))
output_unused_synonyms <- 
  readr::read_tsv(file = paste0("./data_test_output_gbif_verify_synonyms/",
                                "output_unused_synonyms.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    backbone_taxonKey = readr::col_number(),
                    backbone_acceptedKey = readr::col_number(),
                    verified_key = readr::col_number()))
output_updated_scientificName <- 
  readr::read_tsv(file = paste0("./data_test_output_gbif_verify_synonyms/",
                                "output_updated_scientificName.tsv"))
output_updated_accepted <- 
  readr::read_tsv(file = paste0("./data_test_output_gbif_verify_synonyms/",
                                "output_updated_accepted.tsv"))

testthat::test_that("output data.frames are correct", {
  expect_equal(output$verified_synonyms, output_verified_synonyms)
  expect_equal(output$new_synonyms, output_new_synonyms)
  expect_equal(output$unused_synonyms, output_unused_synonyms)
  expect_equal(output$updated_scientificName, output_updated_scientificName)
  expect_equal(output$updated_accepted, output_updated_accepted)
  })