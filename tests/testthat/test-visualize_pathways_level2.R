context("test_visualize_pathways_level2")

# input df
input_test_df <- read.delim(
  paste0(
    "./data_test_pathways/",
    "input_data_pathways.tsv"
  ),
  sep = "\t",
  stringsAsFactors = FALSE
)


valid_pathways_escape <-
  pathways_cbd() %>%
  filter(pathway_level1 == "escape") %>%
  distinct(pathway_level2) %>%
  pull()

invalid_pathways_df <- input_test_df[1:6, ]
invalid_pathways_df$pathway_level1 <- rep("escape", 6)

no_cbd_values <- c(
  "petttt",
  "farmmm",
  "FoOd_BaiT"
)

invalid_pathways_df$pathway_level2 <- c(
  "research",
  "forestry",
  "zoo",
  no_cbd_values
)

na_pathways <- input_test_df[1:6, ]
na_pathways$pathway_level1 <- rep("escape", 6)
na_pathways$pathway_level2[1:2] <- "aquaculture"
na_pathways$pathway_level2[5:6] <- "zoo"
na_pathways$pathway_level2[3:4] <- c(NA_character_, NA_character_)
taxa_na <- unique(na_pathways$key[3:4])

empty_pathways <- input_test_df[1:6, ]
empty_pathways$pathway_level1 <- rep("escape", 6)
empty_pathways$pathway_level2[1:2] <- "aquaculture"
empty_pathways$pathway_level2[4:6] <- "zoo"
empty_pathways$pathway_level2[3] <- ""
taxa_empty <- unique(empty_pathways$key[3])

na_empty_pathways <- na_pathways
na_empty_pathways$pathway_level2[5:6] <- c("", "")
taxa_na_empty <- unique(na_empty_pathways$key[3:6])

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

pathways_selection_escape <- c("agriculture", "zoo")
pathways_selection_inverted_escape <- c("zoo", "agriculture")

output_general <- visualize_pathways_level2(
  input_test_df,
  chosen_pathway_level1 = "escape"
)
output_with_facet <- visualize_pathways_level2(
  input_test_df,
  chosen_pathway_level1 = "escape",
  facet_column = "habitat"
)
output_less_pathways_escape <- visualize_pathways_level2(
  input_test_df,
  chosen_pathway_level1 = "escape",
  pathways = pathways_selection_escape
)
output_less_pathways_inverted_escape <- visualize_pathways_level2(
  input_test_df,
  chosen_pathway_level1 = "escape",
  pathways = pathways_selection_inverted_escape
)

empty_output <- visualize_pathways_level2(
  input_test_df %>% # filter to be sure to produce empty bar plot
    filter(kingdom != "Protozoa" | pathway_level1 != "corridor"),
  chosen_pathway_level1 = "corridor",
  category = "Protozoa"
)

testthat::test_that("Argument: df", {
  expect_error(
    visualize_pathways_level2(
      df = 3,
      chosen_pathway_level1 = "escape"
    ),
    "`df` must be a data frame."
  )
})

testthat::test_that("Argument: chosen_pathway_level1", {
  expect_error(
    visualize_pathways_level2(invalid_pathways_df,
      chosen_pathway_level1 = TRUE
    ),
    "Argument `chosen_pathway_level1` must be a character."
  )
  expect_error(
    visualize_pathways_level2(
      invalid_pathways_df,
      chosen_pathway_level1 = "wehatebadpathways"
    ),
    paste(
      "chosen_pathway_level1 wehatebadpathways not present in column",
      "pathway_level1."
    )
  )
})

testthat::test_that("Argument: from", {
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      from = "1950"
    ),
    "`from` must be a number."
  )
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      from = 2900
    ),
    paste0(
      "`from` must be less than ",
      format(Sys.Date(), "%Y"),
      "."
    )
  )
})

testthat::test_that("Argument: category", {
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      category = 3
    ),
    paste0(
      "`category` must be a character. One of: ",
      paste0(categories, collapse = ", "),
      "."
    )
  )
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      category = "not nice"
    ),
    paste0(
      "`category` is not correct. Choose one of: ",
      paste0(categories, collapse = ", "),
      "."
    )
  )
})

testthat::test_that("Argument: facet_column", {
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      facet_column = 5
    ),
    "Argument facet_column has to be NULL or a character."
  )
  expect_error(visualize_pathways_level2(input_test_df,
    chosen_pathway_level1 = "escape",
    facet_column = "strange_col"
  ))
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      category = "Chordata",
      facet_column = "phylum"
    ),
    "You cannot use phylum as facet with category Chordata."
  )
})

testthat::test_that("Argument pathways", {
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      pathways = TRUE
    ),
    "`pathways` must be a vector of characters."
  )
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      pathways = no_cbd_values
    ),
    paste0(
      "Pathways in `pathways` not present in data.frame: ",
      paste(no_cbd_values, collapse = ","),
      "."
    )
  )
})
testthat::test_that("Argument: taxon_names", {
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      taxon_names = input_test_df
    ),
    "`taxon_names` must be a character."
  )
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      taxon_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: kingdom_names", {
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      kingdom_names = input_test_df
    ),
    "`kingdom_names` must be a character."
  )
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      kingdom_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: phylum_names", {
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      phylum_names = TRUE
    ),
    "`phylum_names` must be a character."
  )
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      category = "Chordata",
      phylum_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: first_observed", {
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      first_observed = 4
    ),
    "`first_observed` must be a character."
  )
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      first_observed = "strange_colname"
    )
  )
})
testthat::test_that("Argument: title labels", {
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      title = TRUE
    ),
    "`title` must be a character or NULL."
  )
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      x_lab = input_test_df
    ),
    "`x_lab` must be a character or NULL."
  )
  expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      y_lab = 4
    ),
    "`y_lab` must be a character or NULL."
  )
})

testthat::test_that("Test CBD standard compliance", {
  message_invalid_pathways <-
    paste0(
      "No CBD standard pathways level 2 value(s) in column `pathway_level2`: ",
      paste0(no_cbd_values, collapse = ", "),
      ". Valid pathways values: ",
      paste0(valid_pathways_escape, collapse = ", "),
      "."
    )
  expect_warning(visualize_pathways_level2(invalid_pathways_df,
    chosen_pathway_level1 = "escape",
    cbd_standard = FALSE
  ),
  message_invalid_pathways,
  fixed = TRUE
  )
  expect_error(visualize_pathways_level2(invalid_pathways_df,
    chosen_pathway_level1 = "escape",
    cbd_standard = TRUE
  ),
  message_invalid_pathways,
  fixed = TRUE
  )
})

testthat::test_that("Test empty pathway_level1 transformation to unknown", {
  expect_warning(visualize_pathways_level2(
    na_pathways,
    chosen_pathway_level1 = "escape"
  ),
  paste(
    nrow(taxa_na),
    "taxa have no information about pathway level 2.",
    "Set to 'unknown'."
  ),
  fixed = TRUE
  )
  expect_warning(visualize_pathways_level2(empty_pathways,
    chosen_pathway_level1 = "escape"
  ),
  paste(
    nrow(taxa_empty),
    "taxa have no information about pathway level 2.",
    "Set to 'unknown'."
  ),
  fixed = TRUE
  )
  expect_warning(visualize_pathways_level2(na_empty_pathways,
    chosen_pathway_level1 = "escape"
  ),
  paste(
    nrow(taxa_na_empty),
    "taxa have no information about pathway level 2.",
    "Set to 'unknown'."
  ),
  fixed = TRUE
  )
})

testthat::test_that("Test output class", {
  expect_type(output_general, type = "list")
  expect_type(output_with_facet, type = "list")
  expect_type(empty_output, type = "list")
  expect_s3_class(output_general, class = "gg")
  expect_s3_class(output_with_facet, class = "egg")
  expect_s3_class(empty_output, class = "gg")
})

testthat::test_that("test pathway factors and their order", {
  expect_true(is.factor(output_general$data$pathway_level2))
  expect_true(is.factor(output_less_pathways_escape$data$pathway_level2))
  expect_true(is.factor(output_less_pathways_inverted_escape$data$pathway_level2))
  expect_true(is.factor(empty_output$data$pathway_level2))
  expect_true(nrow(empty_output$data) == 0)
  expect_true(all(levels(output_general$data$pathway_level2) ==
    valid_pathways_escape))
  expect_true(all(levels(output_less_pathways_escape$data$pathway_level2) ==
    pathways_selection_escape))
  expect_true(all(levels(output_less_pathways_inverted_escape$data$pathway_level2) ==
    pathways_selection_inverted_escape))
})