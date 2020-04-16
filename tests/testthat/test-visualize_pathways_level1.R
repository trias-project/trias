context("test_visualize_pathways_level1")

# input df
input_test_df <- read.delim(
  paste0("./data_test_pathways/",
         "input_data_pathways.tsv"),
  sep = "\t",
  stringsAsFactors = FALSE
)

valide_pathways <- 
  pathways_cbd() %>%
  distinct(pathway_level1) %>%
  pull()

invalid_pathways_df <- input_test_df[1:6,]

no_cbd_values <- c(
  "corrrrrriddor",
  "releaseeeee",
  "essscapppeee"
  )

invalid_pathways_df$pathway_level1 <- c(
  "corridor",
  "escape",
  "unaided",
  no_cbd_values
)

na_pathways <- input_test_df[1:6,]
na_pathways$pathway_level1[3:4] <- c(NA_character_, NA_character_)
n_taxa_na <- unique(na_pathways$key[3:4])

empty_pathways <-  input_test_df[1:6,]
empty_pathways$pathway_level1[3] <- ""
n_taxa_empty <- unique(empty_pathways$key[3])

na_empty_pathways <- na_pathways
na_empty_pathways$pathway_level1[5:6] <- c("", "")
n_taxa_na_empty <- unique(na_empty_pathways$key[3])

categories <- c(
  "Plantae",
  "Animalia",
  "Fungi",
  "Chromista",
  "Archaea",
  "Bacteria",
  "Protozoa",
  "Viruses",
  "incertae sedis",
  "Chordata",
  "Not Chordata"
)

output_general <- visualize_pathways_level1(input_test_df)
output_with_facet <- visualize_pathways_level1(input_test_df,
                                               facet_column = "habitat")

testthat::test_that("Argument: df", {
  expect_error(visualize_pathways_level1(3),
               "`df` must be a data frame.")
})

testthat::test_that("Argument: from", {
  expect_error(visualize_pathways_level1(input_test_df,
                                    from = "1950"),
               "`from` must be a number.")
  expect_error(visualize_pathways_level1(input_test_df,
                                    from = 2900),
               paste0("`from` must be less than ",
                     format(Sys.Date(), "%Y"),
                     "."))
})

testthat::test_that("Argument: category", {
  expect_error(
    visualize_pathways_level1(input_test_df, category = 3),
    paste0(
      "`category` must be a character. One of: ",
      paste0(categories, collapse = ", "),
      "."
    )
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                         category = "not nice"),
    paste0(
      "`category` is not correct. Choose one of: ",
      paste0(categories, collapse = ", "),
      "."
    ))
})

testthat::test_that("Argument: facet_column", {
  expect_error(visualize_pathways_level1(input_test_df,
                                         facet_column = 5),
               msg = "Argument facet_column has to be NULL or a character.")
  expect_error(visualize_pathways_level1(input_test_df,
                                         facet_column = "strange_col"))
})

testthat::test_that("Argument: taxon_names", {
  expect_error(
    visualize_pathways_level1(input_test_df, taxon_names = input_test_df),
    msg = "`taxon_names` must be a character.")
  expect_error(
    visualize_pathways_level1(input_test_df, taxon_names = "blablabla"))
})

testthat::test_that("Argument: kingdom_names", {
  expect_error(
    visualize_pathways_level1(input_test_df, kingdom_names = input_test_df),
    msg = "`kingdom_names` must be a character.")
  expect_error(
    visualize_pathways_level1(input_test_df, kingdom_names = "blablabla"))
})

testthat::test_that("Argument: phylum_names", {
  expect_error(
    visualize_pathways_level1(input_test_df, phylum_names = TRUE),
    msg = "`phylum_names` must be a character.")
  expect_error(
    visualize_pathways_level1(input_test_df,
                              category = "Chordata",
                              phylum_names = "blablabla"))
})

testthat::test_that("Argument: first_observed", {
  expect_error(
    visualize_pathways_level1(input_test_df, first_observed = 4),
    "`first_observed` must be a character.")
  expect_error(
    visualize_pathways_level1(input_test_df,
                              first_observed = "strange_colname"))
})
testthat::test_that("Argument: title labels", {
  expect_error(
    visualize_pathways_level1(input_test_df, title = TRUE),
    "`title` must be a character or NULL.")
  expect_error(
    visualize_pathways_level1(input_test_df, x_lab = input_test_df),
    "`x_lab` must be a character or NULL.")
  expect_error(
    visualize_pathways_level1(input_test_df, y_lab = 4),
    "`y_lab` must be a character or NULL.")
})

testthat::test_that("Test CBD standard compliance", {
  message_invalid_pathways <-
    paste0(
      "No CBD standard pathways level 1 value(s) in column `pathway_level1`: ",
      paste0(no_cbd_values, collapse = ", "),
      ". Valid pathways values: ",
      paste0(valide_pathways, collapse = ", "),
      "."
    )
  expect_warning(visualize_pathways_level1(invalid_pathways_df,
                                           cbd_standard = FALSE),
                 message_invalid_pathways,
                 fixed = TRUE
  )
  expect_error(visualize_pathways_level1(invalid_pathways_df,
                                         cbd_standard = TRUE),
               message_invalid_pathways,
               fixed = TRUE
  )
})

testthat::test_that("Test empty pathway_level1 transformation to unknown", {
  expect_warning(visualize_pathways_level1(na_pathways),
                 paste(
                   nrow(n_taxa_na),
                   "taxa have no information about pathway level 1.",
                   "Set to 'unknown'."),
                 fixed = TRUE
  )
  expect_warning(visualize_pathways_level1(empty_pathways),
                 paste(
                   nrow(n_taxa_empty),
                   "taxa have no information about pathway level 1.",
                   "Set to 'unknown'."),
                 fixed = TRUE
  )
  expect_warning(visualize_pathways_level1(na_empty_pathways),
                 paste(
                   nrow(n_taxa_na_empty),
                   "taxa have no information about pathway level 1.",
                   "Set to 'unknown'."),
                 fixed = TRUE
  )
  
})

testthat::test_that("Test output class", {
  expect_type(output_general, type = "list")
  expect_type(output_with_facet, type = "list")
  expect_s3_class(output_general, class = "gg")
  expect_s3_class(output_with_facet, class = "egg")
})
