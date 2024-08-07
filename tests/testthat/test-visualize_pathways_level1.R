context("test_visualize_pathways_level1")

# input df
input_test_df <- read.delim(
  test_path("data_test_pathways/input_data_pathways.tsv"),
  sep = "\t",
  stringsAsFactors = FALSE
)

valid_pathways <-
  pathways_cbd() %>%
  dplyr::distinct(pathway_level1) %>%
  dplyr::pull()

invalid_pathways_df <- input_test_df[1:6, ]

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

na_pathways <- input_test_df[1:6, ]
na_pathways$pathway_level1[3:4] <- c(NA_character_, NA_character_)
taxa_na <- unique(na_pathways$key[3:4])

empty_pathways <- input_test_df[1:6, ]
empty_pathways$pathway_level1[3] <- ""
taxa_empty <- unique(empty_pathways$key[3])

na_empty_pathways <- na_pathways
na_empty_pathways$pathway_level1[5:6] <- c("", "")
taxa_na_empty <- unique(na_empty_pathways$key[3])

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

pathways_selection <- c("corridor", "escape")
pathways_selection_inverted <- c("escape", "corridor")

output_general <- visualize_pathways_level1(input_test_df)
output_with_facet <- visualize_pathways_level1(input_test_df,
  facet_column = "habitat"
)
output_less_pathways <- visualize_pathways_level1(
  input_test_df,
  pathways = pathways_selection
)
output_less_pathways_inverted <- visualize_pathways_level1(
  input_test_df,
  pathways = pathways_selection_inverted
)

empty_output <- visualize_pathways_level1(
  input_test_df %>%
    filter(kingdom != "Protozoa" | pathway_level1 != "unknown"),
  category = "Protozoa"
)

testthat::test_that("Argument: df", {
  expect_error(
    visualize_pathways_level1(3),
    "`df` must be a data frame."
  )
})

testthat::test_that("Argument: from", {
  expect_error(
    visualize_pathways_level1(input_test_df,
      from = "1950"
    ),
    "`from` must be a number."
  )
  expect_error(
    visualize_pathways_level1(input_test_df, from = c(1920, 1930)),
    "length(from) not equal to 1",
    fixed = TRUE
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
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
    visualize_pathways_level1(input_test_df, category = 3),
    paste0(
      "`category` must be a character. One of: ",
      paste0(categories, collapse = ", "),
      "."
    )
  )
  expect_error(
    visualize_pathways_level1(input_test_df, 
                                   category = c("Animalia", "Protozoa")
    ),
    "length(category) not equal to 1",
    fixed = TRUE
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
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
    visualize_pathways_level1(input_test_df,
      facet_column = 5
    ),
    "Argument facet_column has to be NULL or a character."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                                   facet_column = c("habitat", "phylum")
    ),
    "length(facet_column) not equal to 1",
    fixed = TRUE
  )
  expect_error(visualize_pathways_level1(input_test_df,
    facet_column = "strange_col"
  ))
  expect_error(
    visualize_pathways_level1(input_test_df,
      category = "Chordata",
      facet_column = "phylum"
    ),
    "You cannot use phylum as facet with category Chordata."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                              category = "Not Chordata",
                              facet_column = "phylum"
    ),
    "You cannot use phylum as facet with category Not Chordata."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                              category = "Animalia",
                              facet_column = "kingdom"
    ),
    "You cannot use kingdom as facet if category is selected."
  )
})

testthat::test_that("Argument pathways", {
  expect_error(
    visualize_pathways_level1(input_test_df,
      pathways = TRUE
    ),
    "`pathways` must be a vector of characters."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
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
    visualize_pathways_level1(input_test_df, taxon_names = input_test_df),
    "`taxon_names` must be a character."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                                   taxon_names = c("taxon", "taxonKey")
    ),
    "length(taxon_names) not equal to 1",
    fixed = TRUE
  )
  expect_error(
    visualize_pathways_level1(input_test_df, taxon_names = "blablabla")
  )
})

testthat::test_that("Argument: kingdom_names", {
  expect_error(
    visualize_pathways_level1(input_test_df, kingdom_names = input_test_df),
    "`kingdom_names` must be a character."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                              kingdom_names = c("kingdom", "kingdom_col")
    ),
    "length(kingdom_names) not equal to 1",
    fixed = TRUE
  )
  expect_error(
    visualize_pathways_level1(input_test_df, kingdom_names = "blablabla")
  )
})

testthat::test_that("Argument: phylum_names", {
  expect_error(
    visualize_pathways_level1(input_test_df, phylum_names = TRUE),
    "`phylum_names` must be a character."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                              phylum_names = c("phylum", "phylum_col")
    ),
    "length(phylum_names) not equal to 1",
    fixed = TRUE
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
      category = "Chordata",
      phylum_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: first_observed", {
  expect_error(
    visualize_pathways_level1(input_test_df, first_observed = 4),
    "`first_observed` must be a character."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                                   first_observed = c("year", "year_intro")
    ),
    "length(first_observed) not equal to 1",
    fixed = TRUE
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
      first_observed = "strange_colname"
    )
  )
})
testthat::test_that("Argument: title labels", {
  expect_error(
    visualize_pathways_level1(input_test_df, title = TRUE),
    "`title` must be a character or NULL."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                              title = c("my title", "my 2nd title")
    ),
    "length(title) not equal to 1",
    fixed = TRUE
  )
  expect_error(
    visualize_pathways_level1(input_test_df, x_lab = input_test_df),
    "`x_lab` must be a character or NULL."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                              x_lab = c("x_label", "my x_label")
    ),
    "length(x_lab) not equal to 1",
    fixed = TRUE
  )
  expect_error(
    visualize_pathways_level1(input_test_df, y_lab = 4),
    "`y_lab` must be a character or NULL."
  )
  expect_error(
    visualize_pathways_level1(input_test_df,
                              y_lab = c("y_label", "my y_label")
    ),
    "length(y_lab) not equal to 1",
    fixed = TRUE
  )
})

testthat::test_that("Test CBD standard compliance", {
  message_invalid_pathways <-
    paste0(
      "No CBD standard pathways level 1 value(s) in column `pathway_level1`: ",
      paste0(no_cbd_values, collapse = ", "),
      ". Valid pathways values: ",
      paste0(valid_pathways, collapse = ", "),
      "."
    )
  expect_warning(visualize_pathways_level1(invalid_pathways_df,
    cbd_standard = FALSE
  ),
  message_invalid_pathways,
  fixed = TRUE
  )
  expect_error(visualize_pathways_level1(invalid_pathways_df,
    cbd_standard = TRUE
  ),
  message_invalid_pathways,
  fixed = TRUE
  )
})

testthat::test_that("Test empty pathway_level1 transformation to unknown", {
  expect_warning(visualize_pathways_level1(na_pathways),
    paste(
      nrow(taxa_na),
      "taxa have no information about pathway level 1.",
      "Set to 'unknown'."
    ),
    fixed = TRUE
  )
  expect_warning(visualize_pathways_level1(empty_pathways),
    paste(
      nrow(taxa_empty),
      "taxa have no information about pathway level 1.",
      "Set to 'unknown'."
    ),
    fixed = TRUE
  )
  expect_warning(visualize_pathways_level1(na_empty_pathways),
    paste(
      nrow(taxa_na_empty),
      "taxa have no information about pathway level 1.",
      "Set to 'unknown'."
    ),
    fixed = TRUE
  )
})

testthat::test_that("Test output class", {
  # output is a list
  expect_type(output_general, type = "list")
  expect_type(output_with_facet, type = "list")
  expect_type(empty_output, type = "list")
  
  # plot slot is a list with gg as class if not NULL
  testthat::expect_type(output_general$plot, type = "list")
  testthat::expect_type(output_with_facet$plot, type = "list")
  testthat::expect_s3_class(output_general$plot, class = "gg")
  testthat::expect_s3_class(output_with_facet$plot, class = "egg")
  testthat::expect_null(empty_output$plot)
  
  # data_top_graph is a data.frame (tibble) if not NULL
  testthat::expect_type(output_general$data_top_graph, type = "list")
  testthat::expect_s3_class(output_general$data_top_graph,
                            class = "data.frame")
  testthat::expect_s3_class(output_general$data_top_graph,
                            class = "tbl_df")
  testthat::expect_type(output_with_facet$data_top_graph,
                        type = "list")
  testthat::expect_s3_class(output_with_facet$data_top_graph,
                            class = "data.frame")
  testthat::expect_s3_class(output_with_facet$data_top_graph,
                            class = "tbl_df")
  testthat::expect_null(empty_output$data_top_graph)
  
  # data_top_graph contains only columns taxonKey, and pathway_level1 in this
  # order
  testthat::expect_equal(
    names(output_general$data_top_graph),
    c("taxonKey", "pathway_level1"))
  testthat::expect_equal(
    names(output_with_facet$data_top_graph),
    c("taxonKey", "pathway_level1"))
  
  # data_facet_graph is NULL if faceting is deactivated
  testthat::expect_null(output_general$date_facet_graph)
  
  # data_facet_graph is a data.frame (tibble) if faceting is activated
  testthat::expect_type(output_with_facet$data_facet_graph, type = "list")
  testthat::expect_s3_class(output_with_facet$data_facet_graph,
                            class = "data.frame")
  testthat::expect_s3_class(output_with_facet$data_facet_graph,
                            class = "tbl_df")
  
  # data_facet_graph contains only columns taxonKey, habitat (the facet) and
  # pathway_level1
  testthat::expect_true(
    all(names(output_with_facet$data_facet_graph) %in%
    c("taxonKey", "habitat", "pathway_level1"))
  )
})

testthat::test_that("test pathway factors and their order", {
  expect_true(is.factor(output_general$data_top_graph$pathway_level1))
  expect_true(is.factor(output_less_pathways$data_top_graph$pathway_level1))
  expect_true(is.factor(output_less_pathways_inverted$data_top_graph$pathway_level1))
  expect_true(all(levels(output_general$data_top_graph$pathway_level1) ==
    valid_pathways))
  expect_true(all(levels(output_less_pathways$data_top_graph$pathway_level1) ==
    pathways_selection))
  expect_true(all(
    levels(output_less_pathways_inverted$data_top_graph$pathway_level1) ==
    pathways_selection_inverted))
})
