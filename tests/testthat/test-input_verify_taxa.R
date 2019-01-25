context("input_verify_taxa")

# import correct inputs
source("input_dfs_tests_verify_taxa.R")

testthat::test_that("taxa is a data frame", {
  expect_error(
    verify_taxa(
      taxa = 3,
      verification = my_verification
    ),
    "taxa is not a data frame"
  )
  expect_error(
    verify_taxa(
      taxa = c("23"),
      verification = my_verification
    ),
    "taxa is not a data frame"
  )
})


testthat::test_that("verification is a data frame", {
  expect_error(
    verify_taxa(
      taxa = my_taxa,
      verification = 3
    ),
    "verification is not a data frame"
  )
  expect_error(
    verify_taxa(
      taxa = my_taxa,
      verification = c("3")
    ),
    "verification is not a data frame"
  )
})

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
  stringsAsFactors = FALSE
)

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
  stringsAsFactors = FALSE
)

testthat::test_that("taxa column names are correct", {
  expect_error(verify_taxa(
    taxa = taxa_test1,
    verification = my_verification
  ),
  paste(
    "Elements 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 of",
    "name_col_taxa %in% names(taxa) are not true"
  ),
  fixed = TRUE
  )
  expect_error(verify_taxa(
    taxa = taxa_test2,
    verification = my_verification
  ),
  "Elements 9 of name_col_taxa %in% names(taxa) are not true",
  fixed = TRUE
  )
})

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
  stringsAsFactors = FALSE
)

testthat::test_that("consistency of 'taxa' about GBIF backbone info columns", {
  expect_error(
    verify_taxa(
      taxa = taxa_test3,
      verification = my_verification
    ),
    "Columns with GBIF Backbone info should be empty for unmatched taxa.",
    fixed = TRUE
  )
})

# wrong colnames as input for verification
verification_test1 <- data.frame(
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
  bad_verifiedBy_colname = c("Damiano Oldoni"),
  bad_dateAdded_colname = c(as.Date("2018-01-01")),
  bad_outdated = c(FALSE),
  stringsAsFactors = FALSE
)

# missing columns
verification_test2 <- data.frame(
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
  verifiedBy = c("Dami Oldi"),
  # dateAdded column missing
  outdated = c(FALSE),
  stringsAsFactors = FALSE
)

# inconsistency bb_acceptedName - bb_acceptedKey
verification_test3 <- data.frame(
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
  verifiedBy = c("Damiano Oldoni"),
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE),
  stringsAsFactors = FALSE
)

# accepted taxa present (only synonyms and unmatched taxa allowed.)
verification_test4 <- data.frame(
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
  verifiedBy = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE),
  stringsAsFactors = FALSE
)

# outdated must to be TRUE or FALSE.
verification_test5 <- data.frame(
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
  verifiedBy = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(NA),
  stringsAsFactors = FALSE
)

# datasetKey should be 36 characters long
verification_test6 <- data.frame(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474-ae80a4f18e92,other stuff"),
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
  verifiedBy = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE),
  stringsAsFactors = FALSE
)

# commas not allowed in datasetKey
verification_test7 <- data.frame(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474,ae80a4f18e92"),
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
  verifiedBy = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE),
  stringsAsFactors = FALSE
)

testthat::test_that("verify_taxa column names are correct", {
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test1
  ),
  paste(
    "1, 2, 3, 4, 5, ... of name_col_verification %in%",
    "names(verification) are not true"
  ),
  fixed = TRUE
  )
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test2
  ),
  paste(
    "Elements 3, 6, 11, 17 of name_col_verification %in%",
    "names(verification) are not true"
  ),
  fixed = TRUE
  )
})

testthat::test_that("synonym relations are inconsistent", {
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test3
  ),
  "bb_acceptedName and bb_acceptedKey should be both NA or both present.",
  fixed = TRUE
  )
})

testthat::test_that("accepted taxa in verification input", {
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test4
  ),
  "Only synonyms and unmatched taxa allowed in verification.",
  fixed = TRUE
  )
})

testthat::test_that("restrictions on input columns of verification", {
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test5
  ),
  "Only logicals (TRUE/FALSE) allowed in 'outdated' of verification.",
  fixed = TRUE
  )
})

testthat::test_that("valid datsetKey values", {
  expect_error(
    verify_taxa(
      taxa = my_taxa,
      verification = verification_test6
    ),
    paste(
      "Incorrect datesetKey:", verification_test6$datasetKey,
      "Is expected to be 36-character UUID."
    )
  )
  expect_error(
    verify_taxa(
      taxa = my_taxa,
      verification = verification_test7
    ),
    paste(
      "Incorrect datesetKey:", verification_test7$datasetKey,
      "Is expected to be 36-character UUID."
    )
  )
})
