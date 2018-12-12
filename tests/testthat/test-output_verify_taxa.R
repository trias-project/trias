context("output_verify_taxa")

# import correct inputs
source("input_dfs_tests_verify_taxa.R")

# output
output <- verify_taxa(taxa = taxa_in, taxa_to_verify = taxa_to_verify_in)

testthat::test_that("output structure", {
  expect_type(output, "list")
  expect_true(length(output) == 3)
  expect_type(output$info, "list")
  expect_true(length(output$info) == 7)
  expect_true(is.data.frame(output$taxa))
  expect_true(is.data.frame(output$taxa_to_verify))
  expect_true(is.data.frame(output$info$new_synonyms))
  expect_true(is.data.frame(output$info$new_unmatched_taxa))
  expect_true(is.data.frame(output$info$outdated_taxa))
  expect_true(is.data.frame(output$info$updated_bb_scientificName))
  expect_true(is.data.frame(output$info$updated_bb_acceptedName))
  expect_true(is.data.frame(output$info$duplicates))
  expect_true(is.data.frame(output$info$check_verificationKey))
})

testthat::test_that("consitency input - output", {
  expect_true(nrow(output$taxa) == nrow(taxa_in))
  expect_true(ncol(output$taxa) == ncol(taxa_in) + 1)
  expect_true(all(output$taxa$taxonKey == taxa_in$taxonKey))
  expect_true(
    nrow(output$taxa_to_verify) == 
      nrow(taxa_to_verify_in) + 
      nrow(output$info$new_synonyms) + 
      nrow(output$info$new_unmatched_taxa)
    )
  expect_true(nrow(taxa_to_verify_in %>%
                     filter(!is.na(verificationKey))) <= 
                nrow(output$info$check_verificationKey))
  expect_true(all(output$info$new_synonyms$outdated == FALSE))
  expect_true(all(output$info$new_unmatched_taxa$outdated == FALSE))
})

col_types_taxa_to_verify <- readr::cols(
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
  verificationKey = readr::col_character()
)

output_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_taxa.tsv"),
                  col_types = col_types_output_taxa)

output_taxa_to_verify <-
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_taxa_to_verify.tsv"),
                  col_types = col_types_taxa_to_verify)

output_new_synonyms <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_new_synonyms.tsv"),
                  col_types = col_types_taxa_to_verify)

output_new_unmatched_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_new_unmatched_taxa.tsv"),
                  col_types = col_types_taxa_to_verify)

output_outdated_taxa <- 
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_outdated_taxa.tsv"),
                  col_types = col_types_taxa_to_verify)

output_updated_bb_scientificName <-
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_updated_bb_scientificName.tsv"))

output_updated_bb_acceptedName <-
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_updated_bb_acceptedName.tsv"))

output_duplicates <-
  readr::read_tsv(file = paste0("./data_test_output_verify_taxa/",
                                "output_duplicates.tsv"),
                  col_types = readr::cols(n = readr::col_integer()))

testthat::test_that("output data.frames are correct", {
  expect_equivalent(output$taxa, output_taxa)
  expect_equivalent(output$taxa_to_verify %>%
                      # new synonyms and unmatched get date of today
                      select(-dateAdded),
               output_taxa_to_verify %>%
                 # new synonyms and unmatched got paste date
                 select(-dateAdded))
  expect_equivalent(output$info$new_synonyms %>%
                      # new synonyms get date of today
                      select(-dateAdded), 
                    output_new_synonyms %>%
                      # unmatched got past date 
                      select(-dateAdded)) 
  expect_equivalent(output$info$new_unmatched_taxa %>%
                      # unmatched get date of today
                      select(-dateAdded), 
                    output_new_unmatched_taxa %>%
                      # unmatched got past date 
                      select(-dateAdded)) 
  expect_equivalent(output$info$outdated_taxa, output_outdated_taxa)
  expect_equivalent(output$info$updated_bb_scientificName, 
               output_updated_bb_scientificName)
  expect_equivalent(output$info$updated_bb_acceptedName, 
               output_updated_bb_acceptedName)
  expect_equivalent(output$info$duplicates, output_duplicates)
  # check_verification_key df no tested here: output of another TrIAS function
  })
