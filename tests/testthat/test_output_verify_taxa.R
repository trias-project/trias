#' @importFrom readr read_tsv cols col_date col_number
context("output_verify_taxa")

# correct inputs
taxa_in <- data.frame(
  checklist_scientificName = c("Aspius aspius",
                               "Rana catesbeiana",
                               "Polystichum tsus-simense J.Smith",
                               "Apus apus (Linnaeus, 1758)",
                               "Begonia x semperflorens hort.",
                               "Rana catesbeiana",
                               "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley"),
  checklist_datasetKey = c("98940a79-2bf1-46e6-afd6-ba2e85a26f9f",
                           "e4746398-f7c4-47a1-a474-ae80a4f18e92",
                           "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                           "39653f3e-8d6b-4a94-a202-859359c164c5",
                           "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                           "b351a324-77c4-41c9-a909-f30f77268bc4",
                           "9ff7d317-609b-4c08-bd86-3bc404b77c42"),
  backbone_taxonKey = c(2360181, 2427092, 2651108, 5228676, NA, 2427092, NA),
  backbone_scientificName = c("Aspius aspius (Linnaeus, 1758)",
                              "Rana catesbeiana Shaw, 1802",
                              "Polystichum tsus-simense (Hook.) J.Sm.",
                              "Apus apus (Linnaeus, 1758)",
                              NA,
                              "Rana catesbeiana Shaw, 1802",
                              NA),
  backbone_kingdom = c("Animalia", "Animalia", "Plantae", 
                       "Plantae", NA, "Animalia", NA),
  backbone_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM",
                               "ACCEPTED", NA, "SYNONYM", NA),
  backbone_acceptedKey = c(5851603, 2427091, 4046493, NA, NA, 2427091, NA),
  backbone_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)",
                            "Lithobates catesbeianus (Shaw, 1802)",
                            "Polystichum luctuosum (Kunze) Moore.",
                            NA, NA,
                            "Lithobates catesbeianus (Shaw, 1802)",
                            NA),
  backbone_issues = c("ORIGINAL_NAME_DERIVED", NA,
                      "ORIGINAL_NAME_DERIVED", NA, NA, NA, NA),
  stringsAsFactors = FALSE)

verified_taxa_in <- data.frame(
  checklist_scientificName = c("Rana catesbeiana",
                               "Polystichum tsus-simense J.Smith",
                               "Lemnaceae",
                               "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley"),
  backbone_scientificName = c("Rana catesbeiana Shaw, 1802",
                              "Polystichum tsus-tsus-tsus (Hook.) Captain",
                              "Lemnaceae",
                              NA),
  backbone_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM", NA),
  backbone_acceptedName = c("Lithobates dummyus (Batman, 2018)",
                            "Polystichum luctuosum (Kunze) Moore.",
                            "Araceae",
                            NA),
  backbone_taxonKey = c(2427092, 2651108, 6723,NA),
  backbone_acceptedKey = c(2427091, 4046493, 6979, NA),
  verified_key = c(2427091,
                   4046493,
                   6979,
                   "2805420,2805363"),
  backbone_kingdom = c("Animalia", "Plantae", "Plantae", NA),
  date_added = as.Date(c("2018-07-01",
                         "2018-07-01",
                         "2018-07-01",
                         "2018-07-16")),
  backbone_issues = c(NA_character_, NA_character_, NA_character_,
                      NA_character_),
  checklists = c("e4746398-f7c4-47a1-a474-ae80a4f18e92",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "e4746398-f7c4-47a1-a474-ae80a4f18e92,39653f3e-8d6b-4a94-a202-859359c164c5",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42"),
  remarks = c("dummy example 1: backbone_acceptedName and checklists should be updated",
              "dummy example 2: backbone_scientificName and backbone_issues should be updated",
              "dummy example 3: nothing should be changed",
              "dummy example 4: multiple keys in verified_key are allowed"),
  stringsAsFactors = FALSE)

# correct output
output <- verify_taxa(taxa = taxa_in, verified_taxa = verified_taxa_in)

testthat::test_that("output type", {
  expect_type(output, "list")
  expect_true(is.data.frame(output$verified_taxa))
  expect_true(is.data.frame(output$new_synonyms))
  expect_true(is.data.frame(output$new_unmatched_taxa))
  expect_true(is.data.frame(output$unused_taxa))
  expect_true(is.data.frame(output$updated_scientificName))
  expect_true(is.data.frame(output$updated_acceptedName))
  expect_true(is.data.frame(output$updated_backbone_issues))
  expect_true(is.data.frame(output$duplicates_taxa))})


output_verified_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                               "output_verified_taxa.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    backbone_taxonKey = readr::col_number(),
                    backbone_acceptedKey = readr::col_number(),
                    verified_key = readr::col_character()),
                  na = "")
output_new_synonyms <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_new_synonyms.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    backbone_taxonKey = readr::col_number(),
                    backbone_acceptedKey = readr::col_number(),
                    verified_key = readr::col_character()))

output_new_unmatched_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_new_unmatched_taxa.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    backbone_taxonKey = readr::col_number(),
                    backbone_acceptedKey = readr::col_number()))

output_unused_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_unused_taxa.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    backbone_taxonKey = readr::col_number(),
                    backbone_acceptedKey = readr::col_number(),
                    verified_key = readr::col_character()))

output_updated_scientificName <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_updated_scientificName.tsv"))
output_updated_acceptedName <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_updated_acceptedName.tsv"))

output_updated_backbone_issues <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_updated_backbone_issues.tsv"))

output_duplicates_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_duplicates_taxa.tsv"))

testthat::test_that("output data.frames are correct", {
  expect_equal(output$verified_taxa, output_verified_taxa)
  expect_equal(output$new_synonyms, output_new_synonyms)
  expect_equal(output$unused_taxa, output_unused_taxa)
  expect_equal(output$updated_scientificName, output_updated_scientificName)
  expect_equal(output$updated_acceptedName, output_updated_acceptedName)
  })