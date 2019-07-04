input_test_df <- data.frame(
  key = c(152543101, 152543102, 152543110, 152543115, 152543120),
  canonicalName = c(
    "Gyrodactylus proterorhini",
    "Aphanomyces astaci",
    "Scutigera coleoptrata",
    "Thecaphora oxalidis",
    "Tricellaria inopinata"
  ),
  scientificName = c(
    "Gyrodactylus proterorhini Ergens, 1967",
    "Aphanomyces astaci Schikora",
    "Scutigera coleoptrata Linnaeus, 1758",
    "Thecaphora oxalidis (Ellis & Tracy) M.Lutz, R.Bauer & Piatek",
    "Tricellaria inopinata d'Hondt & Occhipinti Ambrogi, 1985"
  ),
  species = c(
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
# kingdom column is not the default value
input_test_df_kingdom <-
  input_test_df %>%
  rename(kingdom_species = kingdom)
# phylum column is not the default value
input_test_df_phylum <-
  input_test_df %>%
  rename(phylum_species = phylum)
# year_of_introduction column is not the default value
input_test_df_year <-
  input_test_df %>%
  rename(first_obs = first_observed)
# species_names column is not the default value
input_test_df_other_name <-
  input_test_df %>%
  rename(other_name = species)
# test on large input
input_test_df_large <- readr::read_tsv(
  paste0("./data_test_output_get_table_pathways/",
    "input1_get_table_pathways.tsv",
    na = ""
  )
)

# Output basic usage : default values for all params
output_test_df_basic <- data.frame(
  pathway_level1 = c("contaminant", "unknown"),
  pathway_level2 = c("animal_parasite", ""),
  n = as.integer(c(2, 3)),
  examples = c(
    "Aphanomyces astaci, Gyrodactylus proterorhini",
    "Thecaphora oxalidis, Tricellaria inopinata, Scutigera coleoptrata"
  ),
  stringsAsFactors = FALSE
)

testthat::test_that("Basic usage: default values", {
  pathways_default <- get_table_pathways(input_test_df)
  pathways_default_large <- get_table_pathways(input_test_df_large)
  # same cols
  expect_true(all(names(pathways_default) == names(output_test_df_basic)))
  expect_true(all(names(pathways_default) == names(pathways_default_large)))
  # same number of rows (pathways combinations)
  expect_true(nrow(pathways_default) == nrow(output_test_df_basic))
  # large input, more pathways, more rows
  expect_true(nrow(pathways_default_large) > nrow(pathways_default))
  # large input, more taxa sharing same pathways, higher n value
  expect_true(pathways_default_large %>%
    filter(pathway_level1 == "contaminant" &
      pathway_level2 == "animal_parasite") %>%
    pull(n) > (output_test_df_basic %>%
    filter(pathway_level1 == "contaminant" &
      pathway_level2 == "animal_parasite") %>%
    pull(n)))
  # same content of dfs. No examples take into account as they are randomly
  # selected
  expect_equal(
    pathways_default %>% select(-examples),
    output_test_df_basic %>% select(-examples)
  )
})

testthat::test_that("Use with 'category'", {
  output_test_df_animals <- data.frame(
    pathway_level1 = c("contaminant", "unknown"),
    pathway_level2 = c("animal_parasite", ""),
    n = as.integer(c(1, 2)),
    examples = c(
      "Gyrodactylus proterorhini",
      "Scutigera coleoptrata, Tricellaria inopinata"
    ),
    stringsAsFactors = FALSE
  )

  pathways_plants <- get_table_pathways(input_test_df, category = "Plantae")
  pathways_animals <- get_table_pathways(input_test_df, category = "Animalia")
  pathways_animals_2 <- get_table_pathways(input_test_df_kingdom,
    category = "Animalia",
    kingdom = "kingdom_species"
  )
  pathways_chordata <- get_table_pathways(input_test_df, category = "Chordata")
  pathways_not_chordata <- get_table_pathways(input_test_df,
    category = "Not Chordata"
  )
  pathways_not_chordata_2 <- get_table_pathways(input_test_df_phylum,
    category = "Not Chordata",
    phylum = "phylum_species"
  )
  expect_equal(nrow(pathways_plants), 0)
  expect_equal(nrow(pathways_animals), 2)
  expect_true(is.character(pathways_plants$pathway_level1))
  expect_true(is.character(pathways_plants$pathway_level2))
  expect_true(is.character(pathways_plants$examples))
  expect_true(is.integer(pathways_plants$n))
  expect_true(all(class(pathways_plants) == class(pathways_animals)))
  expect_true(all(names(pathways_plants) == names(pathways_animals)))
  expect_equal(
    pathways_animals %>% select(-examples),
    output_test_df_animals %>% select(-examples)
  )
  # All animals are Not Chordata
  expect_equal(
    pathways_not_chordata %>% select(-examples),
    pathways_animals %>% select(-examples)
  )
  # pathway Chordata is empty as well
  expect_equal(pathways_chordata, pathways_plants)
  # same result if kingdom col has different name than default
  expect_equal(
    pathways_animals %>% select(-examples),
    pathways_animals_2 %>% select(-examples)
  )
  # same result if phylum col has different name than default
  expect_equal(
    pathways_not_chordata %>% select(-examples),
    pathways_not_chordata_2 %>% select(-examples)
  )
})

testthat::test_that("Use with 'from'", {
  output_test_df_2012 <- data.frame(
    pathway_level1 = c("contaminant", "unknown"),
    pathway_level2 = c("animal_parasite", ""),
    n = as.integer(c(1, 1)),
    examples = c("Aphanomyces astaci", "Thecaphora oxalidis"),
    stringsAsFactors = FALSE
  )
  output_test_df_2018 <- data.frame(
    pathway_level1 = "contaminant",
    pathway_level2 = "animal_parasite",
    n = as.integer(1),
    examples = "Aphanomyces astaci",
    stringsAsFactors = FALSE
  )
  pathways_1000 <- get_table_pathways(input_test_df, from = 1000)
  pathways_2012 <- get_table_pathways(input_test_df, from = 2012)
  pathways_2018 <- get_table_pathways(input_test_df, from = 2018)
  # from is earlier than any first_observed value
  expect_equal(
    pathways_1000 %>%
      select(-examples),
    output_test_df_basic %>%
      select(-examples)
  )
  expect_true(sum(pathways_1000$n) > sum(pathways_2012$n))
  expect_true(sum(pathways_2012$n) > sum(pathways_2018$n))
  expect_true(nrow(pathways_2012) == 2)
  expect_equal(pathways_2012, output_test_df_2012)
  expect_true(nrow(pathways_2018) == 1)
  expect_equal(pathways_2018, output_test_df_2018)
  expect_equal(pathways_2018, pathways_2012[1, ])
  expect_equal(
    pathways_2018,
    get_table_pathways(input_test_df_year,
      from = 2018,
      year_introduction = "first_obs"
    )
  )
})

testthat::test_that("Use with 'n_species'", {
  pathways_n_species_10 <- get_table_pathways(input_test_df, n_species = 10)
  pathways_n_species_1 <- get_table_pathways(input_test_df, n_species = 1)
  pathways_n_species_3_large_df <- get_table_pathways(input_test_df_large,
    n_species = 3
  )
  expect_equal(
    pathways_n_species_10 %>%
      select(-examples),
    get_table_pathways(input_test_df) %>%
      select(-examples)
  )
  expect_true(all(purrr::map_lgl(
    pathways_n_species_10$examples,
    ~length(str_split(., ",")) <= 10
  )))
  expect_true(all(purrr::map_lgl(
    pathways_n_species_1$examples,
    ~length(str_split(., ",")) == 1
  )))
  expect_true(all(purrr::map_lgl(
    pathways_n_species_3_large_df$examples,
    ~length(str_split(., ",")) <= 3
  )))
})

testthat::test_that("Use with 'species_names'", {
  pathways_scientificName <- get_table_pathways(input_test_df,
    n_species = 1,
    species_names = "scientificName"
  )
  pathways_species <- get_table_pathways(input_test_df,
    from = 2018,
    n_species = 1,
    species_names = "species"
  )
  pathways_other <- get_table_pathways(input_test_df_other_name,
    from = 2018,
    n_species = 1,
    species_names = "other_name"
  )
  scientific_names <- pathways_scientificName %>% pull(examples)
  expect_true(all(purrr::map_lgl(
    scientific_names,
    ~. %in% input_test_df$scientificName
  )))
  expect_equal(pathways_species, pathways_other)
})
