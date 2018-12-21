context("output_verify_taxa")

# import correct inputs
source("input_dfs_tests_verify_taxa.R")

# output
output1 <- verify_taxa(taxa = my_taxa, verification = my_verification)
output2 <- verify_taxa(taxa = my_taxa)

testthat::test_that("output structure", {
  expect_type(output1, "list")
  expect_type(output2, "list")
  expect_true(length(output1) == 3)
  expect_true(length(output2) == 3)
  expect_type(output1$info, "list")
  expect_type(output2$info, "list")
  expect_true(length(output1$info) == 8)
  expect_true(length(output2$info) == 8)
  expect_true(is.data.frame(output1$taxa))
  expect_true(is.data.frame(output2$taxa))
  expect_true(is.data.frame(output1$verification))
  expect_true(is.data.frame(output2$verification))
  expect_true(is.data.frame(output1$info$new_synonyms))
  expect_true(is.data.frame(output2$info$new_synonyms))
  expect_true(is.data.frame(output1$info$new_unmatched_taxa))
  expect_true(is.data.frame(output2$info$new_unmatched_taxa))
  expect_true(is.data.frame(output1$info$outdated_unmatched_taxa))
  expect_true(is.data.frame(output2$info$outdated_unmatched_taxa))
  expect_true(is.data.frame(output1$info$outdated_synonyms))
  expect_true(is.data.frame(output2$info$outdated_synonyms))
  expect_true(is.data.frame(output1$info$updated_bb_scientificName))
  expect_true(is.data.frame(output2$info$updated_bb_scientificName))
  expect_true(is.data.frame(output1$info$updated_bb_acceptedName))
  expect_true(is.data.frame(output2$info$updated_bb_acceptedName))
  expect_true(is.data.frame(output1$info$duplicates))
  expect_true(is.data.frame(output2$info$duplicates))
  expect_true(is.data.frame(output1$info$check_verificationKey))
  expect_true(is.data.frame(output2$info$check_verificationKey))
})

testthat::test_that("consitency input - output", {
  expect_true(nrow(output1$taxa) == nrow(my_taxa))
  expect_true(nrow(output2$taxa) == nrow(my_taxa))
  expect_true(ncol(output1$taxa) == ncol(my_taxa) + 1)
  expect_true(ncol(output2$taxa) == ncol(my_taxa) + 1)
  expect_true(all(output1$taxa$taxonKey == my_taxa$taxonKey))
  expect_true(all(output2$taxa$taxonKey == my_taxa$taxonKey))
  expect_true(
    nrow(output1$verification) ==
      nrow(my_verification) +
        nrow(output1$info$new_synonyms) +
        nrow(output1$info$new_unmatched_taxa)
  )
  expect_true(
    nrow(output2$verification) ==
      nrow(output2$info$new_synonyms) +
        nrow(output2$info$new_unmatched_taxa)
  )
  expect_true(nrow(output1$verification %>%
    filter(!is.na(verificationKey))) <=
    nrow(output1$info$check_verificationKey))
  expect_true(
    nrow(my_taxa %>%
      filter(bb_taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL"))) ==
      nrow(output2$taxa %>%
        filter(!is.na(verificationKey)))
  )
  expect_true(all(output1$info$new_synonyms$outdated == FALSE))
  expect_true(all(output2$info$new_synonyms$outdated == FALSE))
  expect_true(all(output1$info$new_unmatched_taxa$outdated == FALSE))
  expect_true(all(output2$info$new_unmatched_taxa$outdated == FALSE))
  expect_true(all(output2$verification$outdated == FALSE))
  expect_true(all(output1$info$outdated_unmatched_taxa$outdated == TRUE))
  expect_true(all(output2$info$outdated_unmatched_taxa$outdated == TRUE))
  expect_true(all(output1$info$outdated_synonyms$outdated == TRUE))
  expect_true(all(output2$info$outdated_synonyms$outdated == TRUE))
  expect_true(
    nrow(output1$info$outdated_synonyms) + 
      nrow(output1$info$outdated_unmatched_taxa) == 
      nrow(dplyr::filter(output1$verification, outdated == TRUE))
  )
  expect_true(
    nrow(output2$info$outdated_synonyms) + 
      nrow(output2$info$outdated_unmatched_taxa) == 
      nrow(dplyr::filter(output2$verification, outdated == TRUE))
  )
})

col_types_verification <- readr::cols(
  taxonKey = readr::col_double(),
  scientificName = readr::col_character(),
  datasetKey = readr::col_character(),
  bb_key = readr::col_double(),
  bb_scientificName = readr::col_character(),
  bb_kingdom = readr::col_character(),
  bb_rank = readr::col_character(),
  bb_taxonomicStatus = readr::col_character(),
  bb_acceptedKey = readr::col_double(),
  bb_acceptedName = readr::col_character(),
  bb_acceptedKingdom = readr::col_character(),
  bb_acceptedRank = readr::col_character(),
  bb_acceptedTaxonomicStatus = readr::col_character(),
  verificationKey = readr::col_character(),
  remarks = readr::col_character(),
  dateAdded = readr::col_date(format = "%Y-%m-%d"),
  outdated = readr::col_logical()
)

col_types_output_taxa <- readr::cols(
  taxonKey = readr::col_double(),
  scientificName = readr::col_character(),
  datasetKey = readr::col_character(),
  bb_key = readr::col_double(),
  bb_scientificName = readr::col_character(),
  bb_kingdom = readr::col_character(),
  bb_rank = readr::col_character(),
  bb_taxonomicStatus = readr::col_character(),
  bb_acceptedName = readr::col_character(),
  bb_acceptedKey = readr::col_double(),
  verificationKey = readr::col_character(),
  taxonID = readr::col_character()
)

col_types_updated_names <- readr::cols(
  taxonKey = readr::col_double(),
  bb_key = readr::col_double(),
  bb_acceptedKey = readr::col_double()
)

output1_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_taxa.tsv"
    ),
    col_types = col_types_output_taxa
  )
output2_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_taxa.tsv"
    ),
    col_types = col_types_output_taxa
  )

output1_verification <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_verification.tsv"
    ),
    col_types = col_types_verification
  )
output2_verification <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_verification.tsv"
    ),
    col_types = col_types_verification
  )

output1_new_synonyms <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_new_synonyms.tsv"
    ),
    col_types = col_types_verification
  )
output2_new_synonyms <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_new_synonyms.tsv"
    ),
    col_types = col_types_verification
  )

output1_new_unmatched_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_new_unmatched_taxa.tsv"
    ),
    col_types = col_types_verification
  )
output2_new_unmatched_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_new_unmatched_taxa.tsv"
    ),
    col_types = col_types_verification
  )

output1_outdated_unmatched_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_outdated_unmatched_taxa.tsv"
    ),
    col_types = col_types_verification
  )

output2_outdated_unmatched_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_outdated_unmatched_taxa.tsv"
    ),
    col_types = col_types_verification
  )

output1_outdated_synonyms <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_outdated_synonyms.tsv"
    ),
    col_types = col_types_verification
  )

output2_outdated_synonyms <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_outdated_synonyms.tsv"
    ),
    col_types = col_types_verification
  )

output1_updated_bb_scientificName <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_updated_bb_scientificName.tsv"
    ),
    col_types = col_types_updated_names
  )

output2_updated_bb_scientificName <-
  readr::read_tsv(
    file = paste0(
    "./data_test_output_verify_taxa/",
    "output2_updated_bb_scientificName.tsv"
    ),
    col_types = col_types_updated_names
  )

output1_updated_bb_acceptedName <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_updated_bb_acceptedName.tsv"
    ),
    col_types = col_types_updated_names
  )

output2_updated_bb_acceptedName <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_updated_bb_acceptedName.tsv"
    ),
    col_types = col_types_updated_names
  )

output1_duplicates <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_duplicates.tsv"
    ),
    col_types = readr::cols(n = readr::col_integer())
  )
output2_duplicates <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_duplicates.tsv"
    ),
    col_types = readr::cols(n = readr::col_integer())
  )

testthat::test_that("output data.frames are correct", {
  expect_equivalent(output1$taxa, output1_taxa)
  expect_equivalent(output2$taxa, output2_taxa)
  
  expect_equivalent(
    output1$verification %>%
      # new synonyms and unmatched get date of today
      dplyr::select(-dateAdded),
    output1_verification %>%
      # new synonyms and unmatched got paste date
      dplyr::select(-dateAdded)
  )
  expect_equivalent(
    output2$verification %>%
      # new synonyms and unmatched get date of today
      dplyr::select(-dateAdded),
    output2_verification %>%
      # new synonyms and unmatched got paste date
      dplyr::select(-dateAdded)
  )
  
  expect_equivalent(
    output1$info$new_synonyms %>%
      # new synonyms get date of today
      dplyr::select(-dateAdded),
    output1_new_synonyms %>%
      # unmatched got past date
      dplyr::select(-dateAdded)
  )
  expect_equivalent(
    output2$info$new_synonyms %>%
      # new synonyms get date of today
      dplyr::select(-dateAdded),
    output2_new_synonyms %>%
      # unmatched got past date
      dplyr::select(-dateAdded)
  )
  
  expect_equivalent(
    output1$info$new_unmatched_taxa %>%
      # unmatched get date of today
      dplyr::select(-dateAdded),
    output1_new_unmatched_taxa %>%
      # unmatched got past date
      dplyr::select(-dateAdded)
  )
  expect_equivalent(
    output2$info$new_unmatched_taxa %>%
      # unmatched get date of today
      dplyr::select(-dateAdded),
    output2_new_unmatched_taxa %>%
      # unmatched got past date
      dplyr::select(-dateAdded)
  )
  
  expect_equivalent(
    output1$info$outdated_unmatched_taxa, 
    output1_outdated_unmatched_taxa
  )
  expect_equivalent(
    output2$info$outdated_unmatched_taxa, 
    output2_outdated_unmatched_taxa
  )
  
  expect_equivalent(
    output1$info$outdated_synonyms, 
    output1_outdated_synonyms
  )
  expect_equivalent(
    output2$info$outdated_synonyms, 
    output2_outdated_synonyms
  )
  
  expect_equivalent(
    output1$info$updated_bb_scientificName,
    output1_updated_bb_scientificName
  )
  expect_equivalent(
    output2$info$updated_bb_scientificName,
    output2_updated_bb_scientificName
  )
  
  expect_equivalent(
    output1$info$updated_bb_acceptedName,
    output1_updated_bb_acceptedName
  )
  expect_equivalent(
    output2$info$updated_bb_acceptedName,
    output2_updated_bb_acceptedName
  )
  
  expect_equivalent(output1$info$duplicates, output1_duplicates)
  expect_equivalent(output2$info$duplicates, output2_duplicates)
  # check_verification_key df no tested here: output of another TrIAS function
  # only check 0 rows with output2
  expect_true(nrow(output2$info$check_verificationKey) == 0)
})
