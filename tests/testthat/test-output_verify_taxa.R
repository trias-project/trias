context("output_verify_taxa")

taxa_in <- data.frame(
  scientificName = c("Aspius aspius", "Rana catesbeiana", 
                     "Polystichum tsus-simense J.Smith", 
                     "Apus apus (Linnaeus, 1758)",
                     "Begonia x semperflorens hort.", 
                     "Rana catesbeiana", 
                     "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley", 
                     "Atyaephyra desmaresti", 
                     "Ferrissia fragilis",
                     "Ferrissia fragilis",
                     "Ferrissia fragilis"),
  datasetKey = c("98940a79-2bf1-46e6-afd6-ba2e85a26f9f",
                 "e4746398-f7c4-47a1-a474-ae80a4f18e92",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "39653f3e-8d6b-4a94-a202-859359c164c5",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "b351a324-77c4-41c9-a909-f30f77268bc4",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "289244ee-e1c1-49aa-b2d7-d379391ce265",
                 "289244ee-e1c1-49aa-b2d7-d379391ce265",
                 "3f5e930b-52a5-461d-87ec-26ecd66f14a3",
                 "1f3505cd-5d98-4e23-bd3b-ffe59d05d7c2"),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)",
                        "Rana catesbeiana Shaw, 1802",
                        "Polystichum tsus-simense (Hook.) J.Sm.",
                        "Apus apus (Linnaeus, 1758)",
                        NA,
                        "Rana catesbeiana Shaw, 1802",
                        NA,
                        "Atyaephyra desmarestii (Millet, 1831)",
                        "Ferrissia fragilis (Tryon, 1863)",
                        "Ferrissia fragilis (Tryon, 1863)",
                        "Ferrissia fragilis (Tryon, 1863)"),
  bb_key = c(2360181, 2427092, 2651108, 5228676, NA, 2427092, NA,
             4309705, 2291152, 2291152, 2291152),
  bb_kingdom = c("Animalia", "Animalia", "Plantae",
                 "Plantae", NA, "Animalia",
                 NA, "Animalia", "Animalia", "Animalia", "Animalia"),
  bb_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM", "ACCEPTED",
                         NA, "SYNONYM", NA, "HOMOTYPIC_SYNONYM",
                         "SYNONYM", "SYNONYM", "SYNONYM"),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)",
                      "Lithobates catesbeianus (Shaw, 1802)",
                      "Polystichum luctuosum (Kunze) Moore.",
                      NA, NA,
                      "Lithobates catesbeianus (Shaw, 1802)",
                      NA,
                      "Hippolyte desmarestii Millet, 1831",
                      "Ferrissia californica (Rowell, 1863)",
                      "Ferrissia californica (Rowell, 1863)",
                      "Ferrissia californica (Rowell, 1863)"),
  bb_acceptedKey = c(5851603, 2427091, 4046493, NA, NA, 2427091, NA,
                     6454754, 9520065, 9520065, 9520065),
  issues = c("ORIGINAL_NAME_DERIVED", NA, "ORIGINAL_NAME_DERIVED", NA,
             "RANK_INVALID,BACKBONE_MATCH_NONE", NA, NA,
             "CONFLICTING_BASIONYM_COMBINATION", NA, NA, NA),
  stringsAsFactors = FALSE)

verified_taxa_in <- data.frame(
  scientificName = c("Rana catesbeiana",
                     "Polystichum tsus-simense J.Smith",
                     "Lemnaceae",
                     "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley",
                     "Begonia x semperflorens hort.",
                     "Ferrissia fragilis"),
  bb_scientificName = c("Rana catesbeiana Shaw, 1802",
                        "Polystichum tsus-tsus-tsus (Hook.) Captain",
                        "Lemnaceae",
                        NA, NA,
                        "Ferrissia fragilis (Tryon, 1863)"),
  bb_key = c(2427092, 2651108, 6723, NA, NA, 2291152),
  bb_kingdom = c("Animalia", "Plantae", "Plantae", NA, NA, "Animalia"),
  bb_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM", NA, NA, "SYNONYM"),
  bb_acceptedName = c("Lithobates dummyus (Batman, 2018)",
                      "Polystichum luctuosum (Kunze) Moore.",
                      "Araceae",
                      NA, NA, "Ferrissia californica (Rowell, 1863)"),
  bb_acceptedKey = c(2427091, 4046493, 6979, NA, NA, 9520065),
  issues = c(NA, NA, NA, NA, NA, "ORIGINAL_NAME_DERIVED"),
  verification_key = c(2427091,
                       4046493,
                       6979,
                       "2805420,2805363",
                       NA, NA),
  date_added = as.Date(c("2018-07-01",
                         "2018-07-01",
                         "2018-07-01",
                         "2018-07-16",
                         "2018-07-16",
                         "2018-07-01")),
  checklists = c("e4746398-f7c4-47a1-a474-ae80a4f18e92",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "e4746398-f7c4-47a1-a474-ae80a4f18e92,39653f3e-8d6b-4a94-a202-859359c164c5",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "289244ee-e1c1-49aa-b2d7-d379391ce265"),
  remarks = c("dummy example 1: bb_acceptedName and checklists should be updated.",
              "dummy example 2: bb_scientificName and issues should be updated.",
              "dummy example 3: add 'Unused taxa.' at the end of remarks.",
              "dummy example 4: multiple keys in verification_key are allowed.",
              "dummy example 5: issues should be updated.",
              "dummy example 6: issues and checklists should be updated."),
  stringsAsFactors = FALSE)

# output
output <- verify_taxa(taxa = taxa_in, verified_taxa = verified_taxa_in)

testthat::test_that("output type", {
  expect_type(output, "list")
  expect_true(is.data.frame(output$verified_taxa))
  expect_true(is.data.frame(output$new_synonyms))
  expect_true(is.data.frame(output$new_unmatched_taxa))
  expect_true(is.data.frame(output$unused_taxa))
  expect_true(is.data.frame(output$updated_scientificName))
  expect_true(is.data.frame(output$updated_acceptedName))
  expect_true(is.data.frame(output$updated_issues))
  expect_true(is.data.frame(output$duplicates_taxa))})


output_verified_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                               "output_verified_taxa.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    bb_key = readr::col_number(),
                    bb_acceptedKey = readr::col_number(),
                    verification_key = readr::col_character()),
                  na = "")
output_new_synonyms <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_new_synonyms.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    bb_key = readr::col_number(),
                    bb_acceptedKey = readr::col_number(),
                    verification_key = readr::col_character()))

output_new_unmatched_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_new_unmatched_taxa.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    bb_key = readr::col_number(),
                    bb_acceptedKey = readr::col_number()))

output_unused_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_unused_taxa.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    bb_key = readr::col_number(),
                    bb_acceptedKey = readr::col_number(),
                    verification_key = readr::col_character()))

output_updated_scientificName <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_updated_scientificName.tsv"))
output_updated_acceptedName <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_updated_acceptedName.tsv"))

output_updated_issues <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_updated_issues.tsv"),
                  col_types = readr::cols(
                    bb_key = readr::col_number()))

output_duplicates_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_duplicates_taxa.tsv"),
                  col_types = readr::cols(
                    date_added = readr::col_date(format = "%Y-%m-%d"),
                    bb_key = readr::col_number(),
                    bb_acceptedKey = readr::col_number(),
                    verification_key = readr::col_character()),
                  na = "")

testthat::test_that("output data.frames are correct", {
  expect_equal(output$verified_taxa %>% select(-date_added), 
               output_verified_taxa %>% select(-date_added))
  expect_equal(output$new_synonyms %>% select(-date_added), 
               output_new_synonyms %>% select(-date_added))
  expect_equal(output$new_unmatched_taxa %>% select(-date_added), 
               output_new_unmatched_taxa %>% select(-date_added))
  expect_equal(output$unused_taxa, output_unused_taxa)
  expect_equal(output$updated_scientificName, output_updated_scientificName)
  expect_equal(output$updated_acceptedName, output_updated_acceptedName)
  expect_equal(output$updated_issues, 
               output_updated_issues)
  expect_equal(output$duplicates_taxa, 
               output_duplicates_taxa)
  })
