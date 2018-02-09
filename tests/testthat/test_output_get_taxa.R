context("output_get_taxa")


testthat::test_that("output if taxon_keys is numeric", {
  get_taxa_taxon_key_3 <- readr::read_delim(paste0("./data_test_output_get_taxa/",
                                            "get_taxa_taxon_key_3_numeric.tsv"), 
                                            "\t", escape_double = FALSE, 
                                            col_types = cols(
                                              lastCrawled = col_character(), 
                                              lastInterpreted = col_character(),
                                              lastInterpreted = col_character(), 
                                              nomenclaturalStatus = col_logical(),
                                              issues = col_logical()), 
                                            trim_ws = TRUE)
  expect_true(all(get_taxa_taxon_key_3 == get_taxa(taxon_keys = 3, limit = 1), 
                  na.rm = TRUE))
})

testthat::test_that("output if taxon_keys is character", {
  # file saved with write.table()
  # write.table(get_taxa_taxon_key_2,
  # "./tests/testthat/data_test_output_get_taxa/get_taxa_taxon_key_2_character.tsv",
  # sep = "\t", quote = FALSE, row.names = FALSE)
  get_taxa_taxon_key_2 <- readr::read_delim(paste0("./data_test_output_get_taxa/",
                                                   "get_taxa_taxon_key_2_character.tsv"),
                                            "\t", escape_double = FALSE,
                                            col_types = cols(
                                              lastCrawled = col_character(),
                                              lastInterpreted = col_character(),
                                              lastInterpreted = col_character(),
                                              nomenclaturalStatus = col_logical(),
                                              issues = col_logical()),
                                            trim_ws = TRUE)
  expect_true(all(get_taxa_taxon_key_2 == get_taxa(taxon_keys = "2",
                                                   limit = 1), na.rm = TRUE))
})

testthat::test_that("taxon_keys is (numeric) vector", {
  # file saved with write.table()
  # write.table(get_taxa_taxon_key_2,
  # "./tests/testthat/data_test_output_get_taxa/get_taxa_taxon_key_2_character.tsv",
  # sep = "\t", quote = FALSE, row.names = FALSE)
  get_taxa_taxon_key_1_6 <- readr::read_delim(
    "./data_test_output_get_taxa/get_taxa_taxon_key_1_6_numeric.tsv", "\t",
    escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
    col_types = cols(lastCrawled = col_character(),
                     lastInterpreted = col_character(),
                     lastInterpreted = col_character(),
                     nomenclaturalStatus = col_logical(),
                     issues = col_logical()),
    trim_ws = TRUE)
  expect_true(all(get_taxa_taxon_key_1_6 == get_taxa(taxon_keys = c(1,2,3,4,5,6)),
                  na.rm = TRUE))
})

testthat::test_that("taxon_keys is (character) vector", {
  # file saved with write.table()
  # write.table(get_taxa_taxon_key_2,
  # "./tests/testthat/data_test_output_get_taxa/get_taxa_taxon_key_2_character.tsv",
  # sep = "\t", quote = FALSE, row.names = FALSE)
  keys = c("134086855", "134086856", "134086857", "134086858", "134086859",
           "134086860", "134086861", "134086863", "134086865", "134086867",
           "134086868", "134086869", "134086870", "134086871", "134086872",
           "134086873", "134086875", "134086876", "134086877", "134086878")
  get_taxa_taxon_key_char_vector <- readr::read_delim(
    "./data_test_output_get_taxa/get_taxa_taxon_key_char_vector.tsv", "\t",
    escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
    col_types = cols(lastCrawled = col_character(),
                     lastInterpreted = col_character(),
                     lastInterpreted = col_character(),
                     nomenclaturalStatus = col_logical(),
                     issues = col_logical()),
    trim_ws = TRUE)
  expect_true(all(get_taxa_taxon_key_char_vector == get_taxa(taxon_keys = keys),
                  na.rm = TRUE))
})

testthat::test_that("limit < number of taxon_keys", {
  keys = c("134086855", "134086856", "134086857", "134086858", "134086859",
           "134086860", "134086861", "134086863", "134086865", "134086867",
           "134086868", "134086869", "134086870", "134086871", "134086872",
           "134086873", "134086875", "134086876", "134086877", "134086878")
  get_taxa_taxon_key_char_vector_lim_10 <- readr::read_delim(
    "./data_test_output_get_taxa/get_taxa_taxon_key_char_vector_limit_10.tsv", "\t",
    escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
    col_types = cols(lastCrawled = col_character(),
                     lastInterpreted = col_character(),
                     lastInterpreted = col_character(),
                     nomenclaturalStatus = col_logical(),
                     issues = col_logical()),
    trim_ws = TRUE)
  expect_true(all(
    get_taxa_taxon_key_char_vector_lim_10 == get_taxa(taxon_keys = keys,
                                                      limit = 10),
                  na.rm = TRUE))
})

testthat::test_that("checklist is a character, valid limit", {
  # file saved with write.table()
  # write.table(get_taxa_taxon_key_2,
  # "./tests/testthat/data_test_output_get_taxa/get_taxa_taxon_key_2_character.tsv",
  # sep = "\t", quote = FALSE, row.names = FALSE)
  get_taxa_checklist <- readr::read_delim(
    "./data_test_output_get_taxa/get_taxa_checklist_limit.tsv", "\t",
    escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
    col_types = cols(lastCrawled = col_character(),
                     lastInterpreted = col_character(),
                     lastInterpreted = col_character(),
                     nomenclaturalStatus = col_logical(),
                     issues = col_logical()),
    trim_ws = TRUE)
  expect_true(all(get_taxa_checklist == get_taxa(checklist_keys = 
                                        "46261ec5-38e8-44c9-b8e9-edaddf99fa29",
                                        limit = 10),
                  na.rm = TRUE))
})

testthat::test_that("checklist is a vector, valid limit", {
  get_taxa_checklists <- readr::read_delim(
    "./data_test_output_get_taxa/get_taxa_checklists.tsv", "\t",
    escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
    col_types = cols(lastCrawled = col_character(),
                     lastInterpreted = col_character(),
                     lastInterpreted = col_character(),
                     nomenclaturalStatus = col_logical()),
    trim_ws = TRUE)
  test <- get_taxa(
    checklist_keys = c("46261ec5-38e8-44c9-b8e9-edaddf99fa29",
                       "e4746398-f7c4-47a1-a474-ae80a4f18e92"),
    limit = 5)
  test %<>% select(-issues)
  expect_true(all(get_taxa_checklists == test, na.rm = TRUE))
})
