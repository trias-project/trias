context("test_visualize_pathways_year_level2")

# input df
input_test_df_with_nas <- read.delim(
  paste0(
    "./data_test_pathways/",
    "input_data_pathways.tsv"
  ),
  sep = "\t",
  stringsAsFactors = FALSE
)

input_test_df_with_nas_escape <-
  input_test_df_with_nas %>%
  filter(pathway_level1 == "escape")

n_first_observed_na_escape <-
  nrow(input_test_df_with_nas_escape) -
  nrow(input_test_df_with_nas_escape %>%
    filter(!is.na(first_observed)))

input_test_df <-
  input_test_df_with_nas %>%
  filter(!is.na(first_observed))

input_test_df_other_colnames <-
  input_test_df %>%
  rename(
    date_introduction = first_observed,
    pathway_1 = pathway_level1,
    pathway_2 = pathway_level2,
    KINGDOM = kingdom,
    PHYLUM = phylum
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
large_bin <- 20
later_from <- 1970

bin_levels <- c(
  "before 1950",
  "1950 - 1959",
  "1960 - 1969",
  "1970 - 1979",
  "1980 - 1989",
  "1990 - 1999",
  "2000 - 2009",
  "2010 - 2019"
)

large_bin_levels <- c(
  "before 1950",
  "1950 - 1969",
  "1970 - 1989",
  "1990 - 2009",
  "2010 - 2029"
)

later_from_bin_levels <- c("before 1970", bin_levels[4:8])

# Generate outputs
output_general <- visualize_pathways_year_level2(
  input_test_df,
  chosen_pathway_level1 = "escape"
)

output_general_other_colnames <- visualize_pathways_year_level2(
  input_test_df_other_colnames,
  chosen_pathway_level1 = "escape",
  first_observed = "date_introduction",
  pathway_level1_names = "pathway_1",
  pathway_level2_names = "pathway_2",
  kingdom_names = "KINGDOM",
  phylum_names = "PHYLUM"
)

output_with_facet <- visualize_pathways_year_level2(
  input_test_df,
  chosen_pathway_level1 = "escape",
  facet_column = "habitat"
)
output_less_pathways <- visualize_pathways_year_level2(
  input_test_df,
  chosen_pathway_level1 = "escape",
  pathways = pathways_selection_escape
)
output_less_pathways_inverted <- visualize_pathways_year_level2(
  input_test_df,
  chosen_pathway_level1 = "escape",
  pathways = pathways_selection_inverted_escape
)
output_large_bin <- visualize_pathways_year_level2(
  input_test_df,
  chosen_pathway_level1 = "escape",
  bin = large_bin
)
output_later_from <- visualize_pathways_year_level2(
  input_test_df,
  chosen_pathway_level1 = "escape",
  from = later_from
)

testthat::test_that("Argument: df", {
  expect_error(
    visualize_pathways_year_level2(3,
      chosen_pathway_level1 = "escape"
    ),
    "`df` must be a data frame."
  )
})

testthat::test_that("Argument bin", {
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      bin = "20"
    ),
    "`bin` must be a number."
  )
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      bin = 20.5
    ),
    "`bin` must be an integer."
  )
})

testthat::test_that("Argument: from", {
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      from = "1950"
    ),
    "`from` must be a number."
  )
  expect_error(
    visualize_pathways_year_level2(input_test_df,
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
    visualize_pathways_year_level2(input_test_df,
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
    visualize_pathways_year_level2(input_test_df,
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
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      facet_column = 5
    ),
    "Argument facet_column has to be NULL or a character."
  )
  expect_error(visualize_pathways_year_level2(input_test_df,
    chosen_pathway_level1 = "escape",
    facet_column = "strange_col"
  ))
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      category = "Chordata",
      facet_column = "phylum"
    ),
    "You cannot use phylum as facet with category Chordata."
  )
})

testthat::test_that("Argument pathways", {
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      pathways = TRUE
    ),
    "`pathways` must be a vector of characters."
  )
  expect_error(
    visualize_pathways_year_level2(input_test_df,
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
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      taxon_names = input_test_df
    ),
    "`taxon_names` must be a character."
  )
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      taxon_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: kingdom_names", {
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      kingdom_names = input_test_df
    ),
    "`kingdom_names` must be a character."
  )
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      kingdom_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: phylum_names", {
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      phylum_names = TRUE
    ),
    "`phylum_names` must be a character."
  )
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      category = "Chordata",
      phylum_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: first_observed", {
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      first_observed = 4
    ),
    "`first_observed` must be a character."
  )
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      first_observed = "strange_colname"
    )
  )
})
testthat::test_that("Argument: title labels", {
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      title = TRUE
    ),
    "`title` must be a character or NULL."
  )
  expect_error(
    visualize_pathways_year_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      x_lab = input_test_df
    ),
    "`x_lab` must be a character or NULL."
  )
  expect_error(
    visualize_pathways_year_level2(input_test_df,
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
  expect_warning(visualize_pathways_year_level2(
    invalid_pathways_df,
    chosen_pathway_level1 = "escape",
    cbd_standard = FALSE
  ),
  message_invalid_pathways,
  fixed = TRUE
  )
  expect_error(visualize_pathways_year_level2(
    invalid_pathways_df,
    chosen_pathway_level1 = "escape",
    cbd_standard = TRUE
  ),
  message_invalid_pathways,
  fixed = TRUE
  )
})

testthat::test_that("Test empty pathway_level1 transformation to unknown", {
  expect_warning(visualize_pathways_year_level2(
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
  expect_warning(visualize_pathways_year_level2(
    empty_pathways,
    chosen_pathway_level1 = "escape"
  ),
  paste(
    nrow(taxa_empty),
    "taxa have no information about pathway level 2.",
    "Set to 'unknown'."
  ),
  fixed = TRUE
  )
  expect_warning(visualize_pathways_year_level2(
    na_empty_pathways,
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

testthat::test_that("Test warning no year of introduction", {
  expect_warning(
    visualize_pathways_year_level2(
      input_test_df_with_nas,
      chosen_pathway_level1 = "escape"
    ),
    paste0(
      n_first_observed_na_escape,
      " rows without year of introduction in column ",
      "`first_observed` removed."
    )
  )
})

testthat::test_that("Test output class", {
  expect_type(output_general, type = "list")
  expect_type(output_with_facet, type = "list")
  expect_s3_class(output_general, class = "gg")
  expect_s3_class(output_with_facet, class = "egg")
})

testthat::test_that("test pathway factors and their order", {
  expect_true(is.factor(output_general$data$pathway_level2))
  expect_true(is.factor(output_less_pathways$data$pathway_level2))
  expect_true(is.factor(output_less_pathways_inverted$data$pathway_level2))
  expect_true(all(levels(output_general$data$pathway_level2) ==
    valid_pathways_escape))
  expect_true(all(levels(output_less_pathways$data$pathway_level2) ==
    pathways_selection_escape))
  expect_true(all(levels(output_less_pathways_inverted$data$pathway_level2) ==
    pathways_selection_inverted_escape))
})

testthat::test_that("test bin", {
  bins_output <- output_general$data$bins_first_observed
  large_bin_output <-
    output_large_bin$data$bins_first_observed
  expect_true(is.factor(bins_output))
  expect_true(all(levels(bins_output) == bin_levels))
  expect_true(all(levels(large_bin_output) == large_bin_levels))
})

testthat::test_that("test from", {
  bins_output_later_from <- output_later_from$data$bins_first_observed
  expect_true(is.factor(bins_output_later_from))
  expect_true(all(levels(bins_output_later_from) == later_from_bin_levels))
})

testthat::test_that("test output with other column names", {
  expect_equivalent(
    output_general_other_colnames$data,
    output_general$data
  )
  expect_equivalent(
    output_general_other_colnames$scales,
    output_general$scales
  )
  expect_equivalent(
    output_general_other_colnames$mapping,
    output_general$mapping
  )
  expect_equivalent(
    output_general_other_colnames$theme,
    output_general$theme
  )
  expect_equivalent(
    output_general_other_colnames$coordinates,
    output_general$coordinates
  )
  expect_equivalent(
    output_general_other_colnames$facet,
    output_general$facet
  )
  expect_equivalent(
    output_general_other_colnames$labels,
    output_general$labels
  )
})
