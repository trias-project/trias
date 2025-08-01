context("output_gbif_get_taxa")


testthat::test_that("output if taxon_keys is numeric", {
  skip_on_os("windows")
  get_taxa_taxon_key_3 <- readr::read_tsv(
    file =
      paste0(
        "./data_test_output_gbif_get_taxa/",
        "gbif_get_taxa_taxon_key_3_numeric.tsv"
      )
  )
  get_taxa_taxon_key_3 <- get_taxa_taxon_key_3 %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  aa <- gbif_get_taxa(taxon_keys = 3, limit = 1)
  aa <- aa %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  testthat::expect_true(all(get_taxa_taxon_key_3 == aa, na.rm = TRUE))
})

testthat::test_that("output if taxon_keys is character", {
  skip_on_os("windows")
  # file saved with write.table()
  # write.table(get_taxa_taxon_key_2,
  # "./tests/testthat/data_test_output_gbif_get_taxa/get_taxa_taxon_key_2_character.tsv",
  # sep = "\t", quote = FALSE, row.names = FALSE)
  get_taxa_taxon_key_2 <- readr::read_tsv(
    file =
      paste0(
        "./data_test_output_gbif_get_taxa/",
        "gbif_get_taxa_taxon_key_2_character.tsv"
      )
  )
  get_taxa_taxon_key_2 <- get_taxa_taxon_key_2 %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  aa <- gbif_get_taxa(taxon_keys = "2", limit = 1)
  aa <- aa %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  testthat::expect_true(all(get_taxa_taxon_key_2 == aa, na.rm = TRUE))
})

testthat::test_that("taxon_keys is (numeric) vector", {
  skip_on_os("windows")
  get_taxa_taxon_key_1_6 <- readr::read_tsv(
    file =
      paste0(
        "./data_test_output_gbif_get_taxa/",
        "gbif_get_taxa_taxon_key_1_6_numeric.tsv"
      )
  )
  get_taxa_taxon_key_1_6 <- get_taxa_taxon_key_1_6 %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  aa <- gbif_get_taxa(taxon_keys = c(1, 2, 3, 4, 5, 6))
  aa <- aa %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  testthat::expect_true(all(get_taxa_taxon_key_1_6 == aa, na.rm = TRUE))
})

testthat::test_that("taxon_keys is (character) vector", {
  skip_on_os("windows")
  keys <- c("7", "3")
  get_taxa_taxon_key_char_vector <- readr::read_tsv(
    file =
      paste0(
        "./data_test_output_gbif_get_taxa/",
        "gbif_get_taxa_taxon_key_char_vector.tsv"
      )
  )
  get_taxa_taxon_key_char_vector <- get_taxa_taxon_key_char_vector %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  aa <- gbif_get_taxa(taxon_keys = keys)
  aa <- aa %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  testthat::expect_true(all(
    get_taxa_taxon_key_char_vector == aa,
    na.rm = TRUE
  ))
})

testthat::test_that("limit < number of taxon_keys", {
  skip_on_os("windows")
  keys <- c(7, 3, 7765738)
  get_taxa_taxon_key_lim_2 <- readr::read_tsv(
    file =
      paste0(
        "./data_test_output_gbif_get_taxa/",
        "gbif_get_taxa_taxon_key_lim_2.tsv"
      )
  )
  get_taxa_taxon_key_lim_2 <- get_taxa_taxon_key_lim_2 %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  aa <- gbif_get_taxa(taxon_keys = keys, limit = 2)
  aa <- aa %>%
    dplyr::mutate(origin = tolower(origin)) %>%
    dplyr::select(
      key, scientificName, nubKey, taxonID, kingdom,
      kingdomKey, datasetKey, constituentKey, origin
    )

  testthat::expect_true(all(get_taxa_taxon_key_lim_2 == aa, na.rm = TRUE))
})

testthat::test_that("checklist is a character, valid limit", {
  skip_on_os("windows")
  limit <- 10
  aa <- gbif_get_taxa(
    checklist_keys = "3f8a1297-3259-4700-91fc-acc4170b27ce",
    limit = limit
  )
  testthat::expect_true(nrow(aa) == limit)
})

testthat::test_that("checklist is a vector, valid limit", {
  skip_on_os("windows")
  limit <- 5
  checklist_keys <- c(
    "3f8a1297-3259-4700-91fc-acc4170b27ce",
    "c6aaec5d-716e-46a1-826b-dc324feb4ede"
  )
  aa <- gbif_get_taxa(
    checklist_keys = checklist_keys,
    limit = limit
  )
  testthat::expect_true(nrow(aa) == limit * length(checklist_keys))
})

testthat::test_that("check single origin", {
  skip_on_os("windows")
  aa_denormed <- gbif_get_taxa(
    checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
    origin = "denormed_classification", limit = 10
  )

  testthat::expect_true(nrow(aa_denormed) == 10)
})

testthat::test_that("check multiple origins", {
  skip_on_os("windows")
  testthat::expect_warning(
    aa_origin <- gbif_get_taxa(
      checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
      origin = c("denormed_classification", "source"),
      limit = 3000
    )
  )

  testthat::expect_warning(
    aa <- gbif_get_taxa(
      checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
      limit = 3000
    )
  )

  testthat::expect_true(nrow(aa) == nrow(aa_origin))
})
