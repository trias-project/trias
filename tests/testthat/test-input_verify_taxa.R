context("input_verify_taxa")
taxa_in <- data.frame(
  scientificName = c("Aspius aspius",
                     "Rana catesbeiana",
                     "Polystichum tsus-simense J.Smith",
                     "Apus apus (Linnaeus, 1758)",
                     "Begonia x semperflorens hort.",
                     "Rana catesbeiana",
                     "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley"),
  datasetKey = c("98940a79-2bf1-46e6-afd6-ba2e85a26f9f",
                 "e4746398-f7c4-47a1-a474-ae80a4f18e92",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "39653f3e-8d6b-4a94-a202-859359c164c5",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "b351a324-77c4-41c9-a909-f30f77268bc4",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42"),
  bb_key = c(2360181, 2427092, 2651108, 5228676, NA, 2427092, NA),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)",
                        "Rana catesbeiana Shaw, 1802",
                        "Polystichum tsus-simense (Hook.) J.Sm.",
                        "Apus apus (Linnaeus, 1758)",
                        NA,
                        "Rana catesbeiana Shaw, 1802",
                        NA),
  bb_kingdom = c("Animalia", "Animalia", "Plantae", 
                 "Plantae", NA, "Animalia", NA),
  bb_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM",
                         "ACCEPTED", NA, "SYNONYM", NA),
  bb_acceptedKey = c(5851603, 2427091, 4046493, NA, NA, 2427091, NA),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)",
                      "Lithobates catesbeianus (Shaw, 1802)",
                      "Polystichum luctuosum (Kunze) Moore.",
                      NA, NA,
                      "Lithobates catesbeianus (Shaw, 1802)",
                      NA),
  issues = c("ORIGINAL_NAME_DERIVED", NA,
             "ORIGINAL_NAME_DERIVED", NA, NA, NA, NA),
  stringsAsFactors = FALSE)

verified_taxa_in <- data.frame(
  scientificName = c("Rana catesbeiana",
                               "Polystichum tsus-simense J.Smith",
                               "Lemnaceae",
                               "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley"),
  bb_scientificName = c("Rana catesbeiana Shaw, 1802",
                              "Polystichum tsus-tsus-tsus (Hook.) Captain",
                              "Lemnaceae",
                              NA),
  bb_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM", NA),
  bb_acceptedName = c("Lithobates dummyus (Batman, 2018)",
                            "Polystichum luctuosum (Kunze) Moore.",
                            "Araceae",
                            NA),
  bb_key = c(2427092, 2651108, 6723,NA),
  bb_acceptedKey = c(2427091, 4046493, 6979, NA),
  verification_key = c(2427091, 4046493, 6979, "2805420,2805363"),
  bb_kingdom = c("Animalia", "Plantae", "Plantae", NA),
  date_added = as.Date(c("2018-07-01",
                         "2018-07-01",
                         "2018-07-01",
                         "2018-07-16")),
  issues = c(NA, NA, NA, NA),
  checklists = c("e4746398-f7c4-47a1-a474-ae80a4f18e92",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                 "e4746398-f7c4-47a1-a474-ae80a4f18e92,39653f3e-8d6b-4a94-a202-859359c164c5",
                 "9ff7d317-609b-4c08-bd86-3bc404b77c42"),
  remarks = c("dummy example 1: backbone_acceptedName and checklists should be updated",
              "dummy example 2: backbone_scientificName and backbone_issues should be updated",
              "dummy example 3: nothing should be changed",
              "dummy example 4: multiple keys in verification_key are allowed"),
  stringsAsFactors = FALSE)

testthat::test_that("taxa is a data frame", {
  expect_error(verify_taxa(taxa = 3, 
                                    verified_taxa = data.frame(test = c(23))), 
               "taxa is not a data frame")
  expect_error(verify_taxa(taxa = c("23"), 
                                    verified_taxa = data.frame(test = c(23))), 
               "taxa is not a data frame")})


testthat::test_that("verified_taxa is a data frame", {
  expect_error(verify_taxa(taxa = taxa_in, 
                                    verified_taxa = 3),
               "verified_taxa is not a data frame")
  expect_error(verify_taxa(taxa = taxa_in, 
                                    verified_taxa = c("3")),
               "verified_taxa is not a data frame")})

# wrong taxa input
taxa_test1 <- data.frame(
  bad_checklist_scientificName_colname = c("Aspius aspius"),
  bad_backbone_scientificName_colname = c("Aspius aspius (Linnaeus, 1758)"),
  bad_backbone_taxonomicStatus_colname = c("SYNONYM"),
  bad_backbone_acceptedName_colname = c("Leuciscus aspius (Linnaeus, 1758)"),
  bad_backbone_taxonKey_colname = c(2360181),
  bad_backbone_acceptedKey_colname = c(5851603),
  bad_backbone_kingdom_colname = c("Animalia"),
  bad_backbone_issues = c(NA_character_),
  bad_checklist_datasetKey_colname = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  stringsAsFactors = FALSE)

taxa_test2 <- data.frame(
  scientificName = c("Aspius aspius"),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_key = c(2360181),
  # bb_acceptedKey is missing
  bb_kingdom = c("Animalia"),
  issues = c(NA_character_),
  datasetKey = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  stringsAsFactors = FALSE)

testthat::test_that("taxa column names are correct", {
expect_error(verify_taxa(taxa = taxa_test1, 
                                  verified_taxa = verified_taxa_in),
             "Elements 1, 2, 3, 4, 5, 6, 7, 8, 9 of name_col_taxa %in% names(taxa) are not true", 
             fixed = TRUE)
expect_error(verify_taxa(taxa = taxa_test2, 
                                  verified_taxa = verified_taxa_in),
             "Elements 6 of name_col_taxa %in% names(taxa) are not true",
             fixed = TRUE)})

# wrong verified_taxa input
verified_taxa_test1 <- data.frame(
  bad_checklist_scientificName_colname = c("Aspius aspius"),
  bad_backbone_scientificName_colname = c("Aspius aspius (Linnaeus, 1758)"),
  bad_backbone_taxonomicStatus_colname = c("SYNONYM"),
  bad_backbone_acceptedName_colname = c("Leuciscus aspius (Linnaeus, 1758)"),
  bad_backbone_taxonKey_colname = c(2360181),
  bad_backbone_acceptedKey_colname = c(5851603),
  bad_verification_key_colname = c(2427091),
  bad_backbone_kingdom_colname = c("Animalia"),
  bad_date_added_colname = c("2018-01-01"),
  bad_backbone_issues_colname = c(NA),
  bad_remarks_colname = c("dummy example 1: backbone_accepted should be updated"),
  bad_checklists_colname = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  stringsAsFactors = FALSE)

verified_taxa_test2 <- data.frame(
  scientificName = c("Aspius aspius"),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_key = c(2360181),
  bb_acceptedKey = c(5851603),
  verification_key = c(2427091),
  # backbone_kingdom column missing
  # date_added column missing
  issues = c(NA),
  remarks = c("dummy example 1: backbone_accepted should be updated"),
  # checklists column missing
  stringsAsFactors = FALSE)

testthat::test_that("verify_taxa column names are correct", {
  expect_error(verify_taxa(taxa = taxa_in, 
                                    verified_taxa = verified_taxa_test1),
               paste("1, 2, 3, 4, 5, ... of name_col_verified %in%", 
                     "names(verified_taxa) are not true"), 
               fixed = TRUE)
  expect_error(verify_taxa(taxa = taxa_in, 
                                    verified_taxa = verified_taxa_test2),
               paste("Elements 8, 9, 12 of name_col_verified %in%", 
                     "names(verified_taxa) are not true"),
               fixed = TRUE)})
