context("input_verify_taxa")

# import correct inputs
source("input_dfs_tests_verify_taxa.R")

testthat::test_that("taxa is a data frame", {
  expect_error(verify_taxa(taxa = 3, 
                                    taxa_to_verify = taxa_to_verify_in), 
               "taxa is not a data frame")
  expect_error(verify_taxa(taxa = c("23"), 
                                    taxa_to_verify = taxa_to_verify_in), 
               "taxa is not a data frame")})


testthat::test_that("taxa_to_verify is a data frame", {
  expect_error(verify_taxa(taxa = taxa_in, 
                                    taxa_to_verify = 3),
               "taxa_to_verify is not a data frame")
  expect_error(verify_taxa(taxa = taxa_in, 
                                    taxa_to_verify = c("3")),
               "taxa_to_verify is not a data frame")})

# wrong taxa inputs
taxa_test1 <- data.frame(
  bad_checklist_taxonKey_colname = c(123452),
  bad_checklist_scientificName_colname = c("Aspius aspius"),
  bad_checklist_datasetKey_colname = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  bad_backbone_taxonKey_colname = c(2360181),
  bad_backbone_scientificName_colname = c("Aspius aspius (Linnaeus, 1758)"),
  bad_backbone_kingdom_colname = c("Animalia"),
  bad_backbone_rank_colname = c("SPECIES"),
  bad_backbone_taxonomicStatus_colname = c("SYNONYM"),
  bad_backbone_acceptedKey_colname = c(5851603),
  bad_backbone_acceptedName_colname = c("Leuciscus aspius (Linnaeus, 1758)"),
  stringsAsFactors = FALSE)

# missing column
taxa_test2 <- data.frame(
  taxonKey = c(123452),
  scientificName = c("Aspius aspius"),
  datasetKey = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  # bb_acceptedKey is missing
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  stringsAsFactors = FALSE)

testthat::test_that("taxa column names are correct", {
expect_error(verify_taxa(taxa = taxa_test1, 
                                  taxa_to_verify = taxa_to_verify_in),
             paste("Elements 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 of", 
                   "name_col_taxa %in% names(taxa) are not true"), 
             fixed = TRUE)
expect_error(verify_taxa(taxa = taxa_test2, 
                                  taxa_to_verify = taxa_to_verify_in),
             "Elements 9 of name_col_taxa %in% names(taxa) are not true",
             fixed = TRUE)})

# inconsitency about unmatched taxa
taxa_test3 <- data.frame(
  taxonKey = c(123452),
  scientificName = c("Aspius aspius"),
  datasetKey = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  bb_key = c(NA_integer_),
  bb_scientificName = c(NA_character_),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(3483948),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  stringsAsFactors = FALSE)

testthat::test_that("consistency of 'taxa' about GBIF backbone info columns", {
  expect_error(verify_taxa(taxa = taxa_test3, 
                           taxa_to_verify = taxa_to_verify_in),
               paste("Columns related to GBIF Backbone information should be", 
                     "all empty for unmatched taxa (no backbone key)."), 
               fixed = TRUE)
})

# wrong colnames as input for taxa_to_verify
taxa_to_verify_test1 <- data.frame(
  bad_checklist_taxonKey = c(12341),
  bad_checklist_scientificName_colname = c("Aspius aspius"),
  bad_datasetKey_colname = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  bad_backbone_taxonKey_colname = c(2360181),
  bad_backbone_scientificName_colname = c("Aspius aspius (Linnaeus, 1758)"),
  bad_backbone_kingdom_colname = c("Animalia"),
  bad_backbone_rank_colname = c("SPECIES"),
  bad_backbone_taxonomicStatus_colname = c("SYNONYM"),
  bad_backbone_acceptedKey_colname = c(5851603),
  bad_backbone_acceptedName_colname = c("Leuciscus aspius (Linnaeus, 1758)"),
  bad_backbone_acceptedKingdom_colname = c("Animalia"),
  bad_backbone_acceptedrank_colname = c("SPECIES"),
  bad_backbone_acceptedTaxonomicStatus_colname = c("ACCEPTED"),
  bad_verificationKey_colname = c(2427091),
  bad_remarks_colname = c("dummy example 1: backbone_accepted should be updated"),
  bad_dateAdded_colname = c(as.Date("2018-01-01")),
  bad_outdated = c(FALSE),
  stringsAsFactors = FALSE)

# missing columns
taxa_to_verify_test2 <- data.frame(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  # datasetKey column missing
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  # bb_kingdom column missing
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(5851603),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  # bb_acceptedKingdom
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = c("dummy example 1: backbone_accepted should be updated"),
  # dateAdded column missing
  outdated = c(FALSE),
  stringsAsFactors = FALSE)

# inconsistency bb_acceptedName - bb_acceptedKey
taxa_to_verify_test3 <- data.frame(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474-ae80a4f18e92"),
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(NA_integer_),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_acceptedKingdom = c("Animalia"),
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = c("dummy example 1: backbone_accepted should be updated"),
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE),
  stringsAsFactors = FALSE)

# accepted taxa present (only synonyms and unmatched taxa allowed.)
taxa_to_verify_test4 <- data.frame(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474-ae80a4f18e92"),
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("ACCEPTED"),
  bb_acceptedKey = c(5851603),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_acceptedKingdom = c("Animalia"),
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE),
  stringsAsFactors = FALSE)

# outdated must to be TRUE or FALSE.
taxa_to_verify_test5 <- data.frame(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474-ae80a4f18e92"),
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(5851603),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_acceptedKingdom = c("Animalia"),
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(NA),
  stringsAsFactors = FALSE)

testthat::test_that("verify_taxa column names are correct", {
  expect_error(verify_taxa(taxa = taxa_in, 
                           taxa_to_verify = taxa_to_verify_test1),
               paste("1, 2, 3, 4, 5, ... of name_col_verified %in%", 
                     "names(taxa_to_verify) are not true"), 
               fixed = TRUE)
  expect_error(verify_taxa(taxa = taxa_in, 
                           taxa_to_verify = taxa_to_verify_test2),
               paste("Elements 3, 6, 11, 16 of name_col_verified %in%", 
                     "names(taxa_to_verify) are not true"),
               fixed = TRUE)
  })

testthat::test_that("synonym relations are inconsistent", {
  expect_error(verify_taxa(taxa = taxa_in,
                           taxa_to_verify = taxa_to_verify_test3),
    "bb_acceptedName and bb_acceptedKey should be both NA or both present.",
    fixed = TRUE)
  })

testthat::test_that("accepted taxa in taxa_to_verify input", {
  expect_error(verify_taxa(taxa = taxa_in,
                           taxa_to_verify = taxa_to_verify_test4),
               "Only synonyms and unmatched taxa allowed in taxa_to_verify.",
               fixed = TRUE)
})

testthat::test_that("restrictions on input columns of taxa_to_verify", {
  expect_error(verify_taxa(taxa = taxa_in,
                           taxa_to_verify = taxa_to_verify_test5),
    "Only logicals(TRUE/FALSE) allowed in 'outdated' of taxa_to_verify.",
               fixed = TRUE)
})
