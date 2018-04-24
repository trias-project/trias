context("input_gbif_verify_synonyms")

# correct inputs
taxa_in <- data.frame(
  backbone_taxonKey = c(2360181, 2427092, 2651108, 5228676),
  backbone_scientificName = c("Aspius aspius (Linnaeus, 1758)",
                              "Rana catesbeiana Shaw, 1802",
                              "Polystichum tsus-simense (Hook.) J.Sm.",
                              "Apus apus (Linnaeus, 1758)"),
  backbone_acceptedKey = c(5851603, 2427091, 4046493, 5228676),
  backbone_accepted = c("Leuciscus aspius (Linnaeus, 1758)",
                        "Lithobates catesbeianus (Shaw, 1802)",
                        "Polystichum luctuosum (Kunze) Moore.",
                        "Apus apus (Linnaeus, 1758)"),
  backbone_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM", "ACCEPTED"),
  stringsAsFactors = FALSE)

verified_synonyms_in <- data.frame(
  backbone_taxonKey = c(2427092,
                        2651108,
                        6723),
  backbone_scientificName = c("Rana catesbeiana Shaw, 1802",
                              "Polystichum tsus-tsus-tsus (Hook.) Captain",
                              "Lemnaceae"),
  backbone_accepted = c("Lithobates dummyus (Batman, 2018)",
                        "Polystichum luctuosum (Kunze) Moore.",
                        "Araceae"),
  backbone_acceptedKey = c(2427091,
                           4046493,
                           6979),
  backbone_taxonomicStatus = c("SYNONYM", 
                               "SYNONYM", 
                               "SYNONYM"),
  date_added = as.Date(c("2018-01-01",
                         "2018-01-01",
                         "2018-01-01")),
  verified_key = c(2427091,
                   4046493,
                   6979),
  remarks = c("dummy example 1: backbone_accepted should be updated",
              "dummy example 2: backbone_scientificName should be updated",
              "dummy example 3: nothing should be changed"),
  stringsAsFactors = FALSE)

testthat::test_that("taxa is a data.frames", {
  expect_error(gbif_verify_synonyms(taxa = 3, 
                                    verified_synonyms = data.frame(test = c(23))), 
               "taxa is not a data frame")
  expect_error(gbif_verify_synonyms(taxa = c("23"), 
                                    verified_synonyms = data.frame(test = c(23))), 
               "taxa is not a data frame")})


testthat::test_that("verified_synonyms is a data.frame", {
  expect_error(gbif_verify_synonyms(taxa = taxa_in, 
                                    verified_synonyms = 3),
               "verified_synonyms is not a data frame")
  expect_error(gbif_verify_synonyms(taxa = taxa_in, 
                                    verified_synonyms = c("3")),
               "verified_synonyms is not a data frame")})

# wrong inputs
taxa_test1 <- data.frame(
  i_am_not_a_good_column_name = c(2360181),
  backbone_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  backbone_acceptedKey = c(5851603),
  backbone_accepted = c("Leuciscus aspius (Linnaeus, 1758)"),
  backbone_taxonomicStatus = c("SYNONYM"),
  stringsAsFactors = FALSE)

taxa_test2 <- data.frame(
  i_am_not_a_good_column_name = c(2360181),
  i_am_bad_too = c("Aspius aspius (Linnaeus, 1758)"),
  i_am_the_worst_one = c(5851603),
  no_i_am_the_worst_one_please = c("Leuciscus aspius (Linnaeus, 1758)"),
  are_you_kidding_me = c("SYNONYM"),
  stringsAsFactors = FALSE)

taxa_test3 <- data.frame(
  backbone_taxonKey = c(2360181),
  backbone_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  # backbone_acceptedKey is missing
  backbone_accepted = c("Leuciscus aspius (Linnaeus, 1758)"),
  backbone_taxonomicStatus = c("SYNONYM"),
  stringsAsFactors = FALSE)

testthat::test_that("taxa column names are correct", {
expect_error(gbif_verify_synonyms(taxa = taxa_test1, 
                                  verified_synonyms = verified_synonyms_in),
             "Elements 1 of name_col_taxa %in% names(taxa) are not true", 
             fixed = TRUE)
expect_error(gbif_verify_synonyms(taxa = taxa_test2, 
                                  verified_synonyms = verified_synonyms_in),
             "Elements 1, 2, 3, 4, 5 of name_col_taxa %in% names(taxa) are not true",
             fixed = TRUE)
expect_error(gbif_verify_synonyms(taxa = taxa_test3, 
                                  verified_synonyms = verified_synonyms_in),
             "Elements 3 of name_col_taxa %in% names(taxa) are not true",
             fixed = TRUE)})

# wrong inputs
verified_syno_test1 <- data.frame(
  i_am_not_a_good_column_name = c(2360181),
  backbone_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  backbone_acceptedKey = c(5851603),
  backbone_accepted = c("Leuciscus aspius (Linnaeus, 1758)"),
  backbone_taxonomicStatus = c("SYNONYM"),
  date_added = c("2018-01-01"),
  verified_key = c(2427091),
  remarks = c("dummy example 1: backbone_accepted should be updated"),
  stringsAsFactors = FALSE)

verified_syno_test2 <- data.frame(
  i_am_not_a_good_column_name = c(2360181),
  i_am_bad_too = c("Aspius aspius (Linnaeus, 1758)"),
  i_am_the_worst_one = c(5851603),
  no_i_am_the_worst_one_please = c("Leuciscus aspius (Linnaeus, 1758)"),
  we_are_alomost_perfect_backbone_taxonomicStatus = c("SYNONYM"),
  are_you_sure_date_added = c("2018-01-01"),
  i_have_doubts_about_it_verified_key = c(2427091),
  i_remark_it_remarks = c("dummy example 1: backbone_accepted should be updated"),
  stringsAsFactors = FALSE)

verified_syno_test3 <- data.frame(
  backbone_taxonKey = c(2360181),
  backbone_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  backbone_acceptedKey = c(5851603),
  backbone_accepted = c("Leuciscus aspius (Linnaeus, 1758)"),
  backbone_taxonomicStatus = c("SYNONYM"),
  # date_added column missing
  verified_key = c(2427091),
  remarks = c("dummy example 1: backbone_accepted should be updated"),
  stringsAsFactors = FALSE)

testthat::test_that("verified_synonyms column names are correct", {
  expect_error(gbif_verify_synonyms(taxa = taxa_in, 
                                    verified_synonyms = verified_syno_test1),
               paste("Elements 1 of name_col_synonyms %in%", 
                     "names(verified_synonyms) are not true"), 
               fixed = TRUE)
  expect_error(gbif_verify_synonyms(taxa = taxa_in, 
                                    verified_synonyms = verified_syno_test2),
               paste("Elements 1, 2, 3, 4, 5, 6, 7 of name_col_synonyms %in%", 
                     "names(verified_synonyms) are not true"),
               fixed = TRUE)
  expect_error(gbif_verify_synonyms(taxa = taxa_in, 
                                    verified_synonyms = verified_syno_test3),
               paste("Elements 6 of name_col_synonyms %in%", 
                     "names(verified_synonyms) are not true"),
               fixed = TRUE)})
