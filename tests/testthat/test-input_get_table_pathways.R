context("input_get_table_pathways")

input_test_df <- data.frame(
  key = c(152543101, 152543102, 152543110, 152543115, 152543120),
  canonicalName = c(
    "Gyrodactylus proterorhini",
    "Aphanomyces astaci",
    "Scutigera coleoptrata",
    "Thecaphora oxalidis",
    "Tricellaria inopinata"
  ),
  kingdom = c(
    "Animalia",
    "Chromista",
    "Animalia",
    "Fungi",
    "Animalia"
  ),
  phylum = c(
    "Platyhelminthes",
    "Oomycota",
    "Arthropoda",
    "Basidiomycota",
    "Bryozoa"
  ),
  first_observed = c(
    2011,
    2018,
    1830,
    2014,
    2011
  ),
  pathway_level1 = c(
    "contaminant",
    "contaminant",
    NA_character_,
    NA_character_,
    NA_character_
  ),
  pathway_level2 = c(
    "animal_parasite",
    "animal_parasite",
    NA_character_,
    NA_character_,
    NA_character_
  ),
  stringsAsFactors = FALSE
)

testthat::test_that("Param: df", {
  expect_error(get_table_pathways(c(1, 2)), "df is not a data frame.")
})

testthat::test_that("Param: category", {
  expect_error(
    get_table_pathways(input_test_df, category = 29),
    paste0(
      "Category has to be a character. ",
      "One of: Plantae, Animalia, Fungi, Chromista, Archaea, Bacteria, ",
      "Protozoa, Viruses, incertae sedis, Chordata, Not Chordata."
    ),
    fixed = TRUE
  )
  expect_error(
    get_table_pathways(input_test_df, category = "bad category"),
    paste0(
      "Category not correct. Choose one of: ",
      "Plantae, Animalia, Fungi, Chromista, Archaea, Bacteria, ",
      "Protozoa, Viruses, incertae sedis, Chordata, Not Chordata."
    ),
    fixed = TRUE
  )
})

testthat::test_that("Param: from", {
  expect_error(
    get_table_pathways(input_test_df, from = "1980"),
    msg = "Parameter 'from' should be a number (year).",
    fixed = TRUE
  )
  expect_error(
    get_table_pathways(input_test_df, from = 30000),
    paste0(
      "Invalid year in 'from'. ",
      "Choose a year smaller than ",
      substr(Sys.Date(), start = 1, stop = 4)
    ),
    fixed = TRUE
  )
  expect_error(
    get_table_pathways(input_test_df, from = -1980),
    msg = "Parameter 'from' should be a positive number."
  )
})

testthat::test_that("Param: n_species", {
  expect_error(
    get_table_pathways(input_test_df, n_species = "5"),
    msg = "Parameter 'n_species' should be a number."
  )
  expect_error(
    get_table_pathways(input_test_df, n_species = -5),
    msg = "Parameter 'n_species' should be a positive number."
  )
  expect_error(
    get_table_pathways(input_test_df, n_species = 5.5),
    msg = "Parameter 'n_species' should be an integer."
  )
})

testthat::test_that("Param: kingdom", {
  expect_error(
    get_table_pathways(input_test_df, kingdom_names = 4),
    msg = "Parameter 'kingdom_names' should be a character."
  )
  expect_error(
    get_table_pathways(input_test_df, kingdom_names = "not_a_column"),
    msg = paste0(
      "These columns exist in colnames but not in your dataframe: ",
      "not_a_column and these exist in your dataframe but not in ",
      "colnames: ", paste(names(input_test_df), collapse = " ")
    )
  )
})

testthat::test_that("Param: phylum_names", {
  expect_error(
    get_table_pathways(input_test_df, category = "Chordata", phylum_names = 4),
    msg = "Parameter 'phylum_names' should be a character."
  )
  expect_error(
    get_table_pathways(input_test_df,
      category = "Chordata",
      phylum_names = "phy"
    ),
    msg = paste0(
      "These columns exist in colnames but not in your dataframe: ",
      "phy and these exist in your dataframe but not in ",
      "colnames: ", paste(names(input_test_df), collapse = " ")
    )
  )
})

testthat::test_that("Param: year_of_introduction", {
  expect_error(
    get_table_pathways(input_test_df, from = 2000, first_observed = 5),
    msg = "Parameter 'year_of_introduction' should be a character."
  )
  expect_error(
    get_table_pathways(input_test_df, from = 2000, first_observed = "yy"),
    msg = paste0(
      "These columns exist in colnames but not in your dataframe: ",
      "yy and these exist in your dataframe but not in ",
      "colnames: ", paste(names(input_test_df), collapse = " ")
    )
  )
})

testthat::test_that("Param: species_names", {
  expect_error(
    get_table_pathways(input_test_df, species_names = 4),
    msg = "Parameter 'species_names' should be a character."
  )
  expect_error(
    get_table_pathways(input_test_df, species_names = "sp"),
    msg = paste0(
      "These columns exist in colnames but not in your dataframe: ",
      "sp and these exist in your dataframe but not in ",
      "colnames: ", paste(names(input_test_df), collapse = " ")
    )
  )
})
