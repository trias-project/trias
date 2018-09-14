context("output_gbif_get_taxa")


testthat::test_that("output if taxon_keys is numeric", {
  get_taxa_taxon_key_3 <- readr::read_tsv(file = 
                          paste0("./data_test_output_gbif_get_taxa/",
                                 "gbif_get_taxa_taxon_key_3_numeric.tsv"))
  get_taxa_taxon_key_3 %<>% 
    mutate(origin = str_to_lower(origin)) %>%
    select(-lastCrawled, -lastInterpreted, -issues) 
  
  aa <- gbif_get_taxa(taxon_keys = 3, limit = 1)
  aa %<>% 
    mutate(origin = str_to_lower(origin)) %>%
    select(-lastCrawled, -lastInterpreted, -issues)
  
  expect_true(all(get_taxa_taxon_key_3 == aa, na.rm = TRUE))
})

testthat::test_that("output if taxon_keys is character", {
  # file saved with write.table()
  # write.table(get_taxa_taxon_key_2,
  # "./tests/testthat/data_test_output_gbif_get_taxa/get_taxa_taxon_key_2_character.tsv",
  # sep = "\t", quote = FALSE, row.names = FALSE)
  get_taxa_taxon_key_2 <- readr::read_tsv(file = 
                          paste0("./data_test_output_gbif_get_taxa/",
                                 "gbif_get_taxa_taxon_key_2_character.tsv"))
  get_taxa_taxon_key_2 %<>% 
    mutate(origin = str_to_lower(origin)) %>%
    select(-lastCrawled, -lastInterpreted, -issues) 
  
  aa <- gbif_get_taxa(taxon_keys = "2", limit = 1)
  aa %<>% 
    mutate(origin = str_to_lower(origin)) %>%
    select(-lastCrawled, -lastInterpreted, -issues)
  
  expect_true(all(get_taxa_taxon_key_2 == aa, na.rm = TRUE))
})

testthat::test_that("taxon_keys is (numeric) vector", {
  get_taxa_taxon_key_1_6 <- readr::read_tsv(file = 
                            paste0("./data_test_output_gbif_get_taxa/",
                                   "gbif_get_taxa_taxon_key_1_6_numeric.tsv"))
  get_taxa_taxon_key_1_6 %<>% 
    mutate(origin = str_to_lower(origin)) %>%
    select(-lastCrawled, -lastInterpreted, -issues)
  
  aa <- gbif_get_taxa(taxon_keys = c(1,2,3,4,5,6))
  aa %<>% 
    mutate(origin = str_to_lower(origin)) %>%
    select(-lastCrawled, -lastInterpreted, -issues)
  
  expect_true(all(get_taxa_taxon_key_1_6 == aa, na.rm = TRUE))
})

testthat::test_that("taxon_keys is (character) vector", {
  keys = c("7", "3")
  get_taxa_taxon_key_char_vector <- readr::read_tsv(file = 
                                    paste0("./data_test_output_gbif_get_taxa/",
                                           "gbif_get_taxa_taxon_key_char_vector.tsv"))
  get_taxa_taxon_key_char_vector %<>% 
    mutate(origin = str_to_lower(origin))
  
  aa <- gbif_get_taxa(taxon_keys = keys)
  aa %<>% 
    mutate(origin = str_to_lower(origin)) %>%
    select(-lastCrawled, -lastInterpreted, 
           -issues, -remarks, -nomenclaturalStatus)
    
  expect_true(all(
    get_taxa_taxon_key_char_vector == aa, na.rm = TRUE))
})

testthat::test_that("limit < number of taxon_keys", {
  keys = c(7, 3, 7765738)
  get_taxa_taxon_key_lim_2 <- readr::read_tsv(file = 
                                      paste0("./data_test_output_gbif_get_taxa/",
                                      "gbif_get_taxa_taxon_key_lim_2.tsv"))
  get_taxa_taxon_key_lim_2 %<>% mutate(origin = str_to_lower(origin))
  get_taxa_taxon_key_lim_2 %<>% 
    mutate(origin = str_to_lower(origin)) %>%
    select(-lastCrawled, -lastInterpreted, 
           -issues, -remarks, -nomenclaturalStatus)
  
  aa <- gbif_get_taxa(taxon_keys = keys,limit = 2)
  aa %<>% 
    mutate(origin = str_to_lower(origin)) %>%
    select(-lastCrawled, -lastInterpreted, 
           -issues, -remarks, -nomenclaturalStatus)
  
  expect_true(all(get_taxa_taxon_key_lim_2 == aa, na.rm = TRUE))
})

testthat::test_that("checklist is a character, valid limit", {
  get_taxa_checklist <- readr::read_tsv(file = paste0("./data_test_output_gbif_get_taxa/",
                                              "gbif_get_taxa_checklist.tsv"))
  get_taxa_checklist %<>% mutate(origin = str_to_lower(origin))
  aa <- gbif_get_taxa(checklist_keys = "3f8a1297-3259-4700-91fc-acc4170b27ce",
                      limit = 10)
  aa %<>% 
    mutate(origin = str_to_lower(origin))
  
  expect_true(all(get_taxa_checklist == aa, na.rm = TRUE))
})

testthat::test_that("checklist is a vector, valid limit", {
  get_taxa_checklists <- readr::read_tsv(file = paste0("./data_test_output_gbif_get_taxa/",
                                               "gbif_get_taxa_checklists.tsv"))
  get_taxa_checklists %<>% mutate(origin = str_to_lower(origin))

    aa <- gbif_get_taxa(
    checklist_keys = c("3f8a1297-3259-4700-91fc-acc4170b27ce",
                       "c6aaec5d-716e-46a1-826b-dc324feb4ede"),
    limit = 5)
  aa %<>% mutate(origin = str_to_lower(origin))

  expect_true(all(get_taxa_checklists == aa, na.rm = TRUE))
})

testthat::test_that("check single origin", {
  aa_denormed <- gbif_get_taxa(checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                               origin = "denormed_classification", limit = 10)
  
  expect_true(nrow(aa_denormed) == 10)
})

testthat::test_that("check multiple origins", {
  aa_origin <- gbif_get_taxa(checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                    origin = c("denormed_classification","source"), 
                    limit = 3000)
  
  aa <- gbif_get_taxa(checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
                      limit = 3000)
  
  expect_true(nrow(aa) == nrow(aa_origin))
})