input_test_df <- data.frame(
  key = c(152543101, 152543102, 152543110, 152543115, 152543120),
  canonicalName = c("Gyrodactylus proterorhini", 
                    "Aphanomyces astaci", 
                    "Scutigera coleoptrata",
                    "Thecaphora oxalidis", 
                    "Tricellaria inopinata"),
  kingdom = c("Animalia",
              "Chromista", 
              "Animalia", 
              "Fungi", 
              "Animalia"),
  phylum = c("Platyhelminthes",
             "Oomycota",
             "Arthropoda",
             "Basidiomycota",
             "Bryozoa"),
  first_observed = c(2011, 
                     2018, 
                     1830, 
                     2014, 
                     2011),
  pathway_level1 = c("contaminant", 
                     "contaminant",
                     NA_character_,
                     NA_character_,
                     NA_character_),
  pathway_level2 = c("animal_parasite",
                     "animal_parasite",
                     NA_character_,
                     NA_character_,
                     NA_character_),
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

# Output basic usage : default values for all params
output_test_df_basic <- data.frame(
  pathway_level1 = c("contaminant", "unknown"),
  pathway_level2 = c("animal_parasite", ""),
  n = as.integer(c(2, 3)),
  examples = c(
    "Aphanomyces astaci, Gyrodactylus proterorhini",
    "Thecaphora oxalidis, Tricellaria inopinata, Scutigera coleoptrata"),
  stringsAsFactors = FALSE
)

testthat::test_that("Basic usage: default values", {
  pathways_default <- get_table_pathways(input_test_df)
  # same cols
  expect_true(all(names(pathways_default) == names(output_test_df_basic)))
  # same number of rows (pathways combinations)
  expect_true(nrow(pathways_default) == nrow(output_test_df_basic))
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
      "Scutigera coleoptrata, Tricellaria inopinata"),
    stringsAsFactors = FALSE
  )
  
  pathways_plants <- get_table_pathways(input_test_df, category = "Plantae")
  pathways_animals <- get_table_pathways(input_test_df, category = "Animalia")
  pathways_animals_2 <- get_table_pathways(input_test_df_kingdom, 
                                           category = "Animalia",
                                           kingdom = "kingdom_species")
  pathways_chordata <- get_table_pathways(input_test_df, category = "Chordata")
  pathways_not_chordata <- get_table_pathways(input_test_df, 
                                              category = "Not Chordata")
  pathways_not_chordata_2 <- get_table_pathways(input_test_df_phylum, 
                                              category = "Not Chordata",
                                              phylum = "phylum_species")
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
    pathways_animals_2 %>% select(-examples))
  # same result if phylum col has different name than default
  expect_equal(
    pathways_not_chordata %>% select(-examples), 
    pathways_not_chordata_2 %>% select(-examples))
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
  expect_equal(pathways_1000 %>%
                select(-examples),
              output_test_df_basic %>%
                select(-examples))
  expect_true(sum(pathways_1000$n) > sum(pathways_2012$n))
  expect_true(sum(pathways_2012$n) > sum(pathways_2018$n))
  expect_true(nrow(pathways_2012) == 2)
  expect_equal(pathways_2012, output_test_df_2012)
  expect_true(nrow(pathways_2018) == 1)
  expect_equal(pathways_2018, output_test_df_2018)
  expect_equal(pathways_2018, pathways_2012[1,])
})