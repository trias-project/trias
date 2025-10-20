context("test_visualize_pathways_level2")

# input df
input_test_df <- read.delim(
  test_path("data_test_pathways/input_data_pathways.tsv"),
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
  testthat::expect_error(
    visualize_pathways_level2(
      df = 3,
      chosen_pathway_level1 = "escape"
    ),
    "`df` must be a data frame."
  )
})

testthat::test_that("Argument: chosen_pathway_level1", {
  testthat::expect_error(
    visualize_pathways_level2(invalid_pathways_df,
                              chosen_pathway_level1 = TRUE
    ),
    "Argument `chosen_pathway_level1` must be a character."
  )
  testthat::expect_error(
    visualize_pathways_level2(
      input_test_df, 
      chosen_pathway_level1  = c("escape", "corridor")),
    "length(chosen_pathway_level1) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
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
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      from = "1950"
    ),
    "`from` must be a number."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df, 
                              from = c(1950, 1960),
                              chosen_pathway_level1 = "escape"),
    "length(from) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
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
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      category = 3
    ),
    paste0(
      "`category` must be NULL or a character. One of: ",
      paste0(categories, collapse = ", "),
      "."
    )
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              category = c("Animalia", "Plantae")),
    "length(category) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
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
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      facet_column = 5
    ),
    "Argument facet_column has to be NULL or a character."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              facet_column = c("habitat", "kingdom")),
    "length(facet_column) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              facet_column = "strange_col"
    )
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      category = "Chordata",
      facet_column = "phylum"
    ),
    "You cannot use phylum as facet with category Chordata."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              category = "Not Chordata",
                              facet_column = "phylum"
    ),
    "You cannot use phylum as facet with category Not Chordata."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              category = "Animalia",
                              facet_column = "kingdom"
    ),
    "You cannot use kingdom as facet if category is selected."
  )
})

testthat::test_that("Argument pathways", {
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      pathways = TRUE
    ),
    "`pathways` must be a vector of characters."
  )
})
testthat::test_that("Argument: taxon_names", {
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      taxon_names = input_test_df
    ),
    "`taxon_names` must be a character."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              taxon_names = c("taxonKey", "key")),
    "length(taxon_names) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      taxon_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: kingdom_names", {
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              kingdom_names = input_test_df
    ),
    "`kingdom_names` must be a character."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df, 
                              chosen_pathway_level1 = "escape",
                              kingdom_names = c("kingdomKey", "kingdom")),
    "length(kingdom_names) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              kingdom_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: phylum_names", {
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              phylum_names = TRUE
    ),
    "`phylum_names` must be a character."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df, 
                              chosen_pathway_level1 = "escape",
                              phylum_names = c("phylumKey", "phylum")
    ),
    "length(phylum_names) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              category = "Chordata",
                              phylum_names = "blablabla"
    )
  )
})

testthat::test_that("Argument: first_observed", {
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      first_observed = 4
    ),
    "`first_observed` must be a character."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              first_observed = c("year", "first_observed")),
    "length(first_observed) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              first_observed = "strange_colname"
    )
  )
})
testthat::test_that("Argument: title labels", {
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              title = TRUE
    ),
    "`title` must be a character or NULL."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df, 
                              chosen_pathway_level1 = "escape",
                              title = c("title", "my title")),
    "length(title) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
      chosen_pathway_level1 = "escape",
      x_lab = input_test_df
    ),
    "`x_lab` must be a character or NULL."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df, 
                              chosen_pathway_level1 = "escape",
                              x_lab = c("x", "x lab")
    ),
    "length(x_lab) not equal to 1",
    fixed = TRUE
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df,
                              chosen_pathway_level1 = "escape",
                              y_lab = 4
    ),
    "`y_lab` must be a character or NULL."
  )
  testthat::expect_error(
    visualize_pathways_level2(input_test_df, 
                              chosen_pathway_level1 = "escape",
                              y_lab = c("y", "y lab")
    ),
    "length(y_lab) not equal to 1",
    fixed = TRUE
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
  testthat::expect_warning(visualize_pathways_level2(invalid_pathways_df,
    chosen_pathway_level1 = "escape",
    cbd_standard = FALSE
    ),
    message_invalid_pathways,
    fixed = TRUE
  )
  testthat::expect_error(visualize_pathways_level2(invalid_pathways_df,
    chosen_pathway_level1 = "escape",
    cbd_standard = TRUE
    ),
    message_invalid_pathways,
    fixed = TRUE
  )
})

testthat::test_that("Test empty pathway_level1 transformation to unknown", {
  testthat::expect_warning(visualize_pathways_level2(na_pathways,
                                           chosen_pathway_level1 = "escape"
  ),
  paste(
    nrow(taxa_na),
    "taxa have no information about pathway level 2.",
    "Set to 'unknown'."
  ),
  fixed = TRUE
  )
  testthat::expect_warning(visualize_pathways_level2(empty_pathways,
                                           chosen_pathway_level1 = "escape"
  ),
  paste(
    nrow(taxa_empty),
    "taxa have no information about pathway level 2.",
    "Set to 'unknown'."
  ),
  fixed = TRUE
  )
  testthat::expect_warning(visualize_pathways_level2(na_empty_pathways,
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
  # output is a list
  testthat::expect_type(output_general, type = "list")
  testthat::expect_type(output_with_facet, type = "list")
  testthat::expect_type(empty_output, type = "list")
  
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
  
  # data_top_graph contains only columns taxonKey, pathway_level2 in this order
  testthat::expect_equal(
    names(output_general$data_top_graph),
    c("taxonKey", "pathway_level2"))
  testthat::expect_equal(
    names(output_with_facet$data_top_graph),
    c("taxonKey", "pathway_level2"))
  
  # data_facet_graph is NULL if faceting is deactivated
  testthat::expect_null(output_general$date_facet_graph)
  
  # data_facet_graph is a data.frame (tibble) if faceting is activated
  testthat::expect_type(output_with_facet$data_facet_graph, type = "list")
  testthat::expect_s3_class(output_with_facet$data_facet_graph,
                            class = "data.frame")
  testthat::expect_s3_class(output_with_facet$data_facet_graph,
                            class = "tbl_df")
  
  # data_facet_graph contains only columns taxonKey, habitat (the facet),
  # pathway_level2, in this order
  testthat::expect_true(
    all(names(output_with_facet$data_facet_graph) %in%
          c("taxonKey", "habitat", "pathway_level2"))
  )
})

testthat::test_that("test pathway factors and their order", {
  testthat::expect_true(is.factor(output_general$data_top_graph$pathway_level2))
  testthat::expect_true(
    is.factor(output_less_pathways_escape$data_top_graph$pathway_level2)
  )
  testthat::expect_true(
    is.factor(output_less_pathways_inverted_escape$data_top_graph$pathway_level2)
  )
  testthat::expect_true(
    all(levels(output_general$data_top_graph$pathway_level2) ==
    valid_pathways_escape)
  )
  testthat::expect_true(
    all(levels(output_less_pathways_escape$data_top_graph$pathway_level2) ==
    pathways_selection_escape)
  )
  testthat::expect_true(
    all(
      levels(output_less_pathways_inverted_escape$data_top_graph$pathway_level2) ==
    pathways_selection_inverted_escape
    )
  )
})
