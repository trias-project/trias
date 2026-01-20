context("test-get_nubkeys")

# Test input validation ----

test_that("datasetKey must be a scalar character", {
  expect_error(
    get_nubkeys(datasetKey = c("key1", "key2")),
    "`datasetKey` should be a single \\(scalar\\) non-NA character value."
  )
  
  expect_error(
    get_nubkeys(datasetKey = 123),
    "`datasetKey` should be a single \\(scalar\\) non-NA character value."
  )
  
  expect_error(
    get_nubkeys(datasetKey = NA_character_),
    "`datasetKey` should be a single \\(scalar\\) non-NA character value."
  )
  
  expect_error(
    get_nubkeys(datasetKey = NULL),
    "`datasetKey` should be a single \\(scalar\\) non-NA character value."
  )
})

test_that("`datasetKey` doesn't contain spaces", {
  expect_error(
    get_nubkeys(datasetKey = "a b"),
    "`datasetKey` should not contain spaces."
  )
})

test_that("allow_synonyms must be a scalar logical", {
  expect_error(
    get_nubkeys(
      datasetKey = "e082b10e-476f-43c1-aa61-f8d92f33029a",
      allow_synonyms = c(TRUE, FALSE)
    ),
    "`allow_synonyms` should be a single \\(scalar\\) non-NA logical value."
  )
  
  expect_error(
    get_nubkeys(
      datasetKey = "e082b10e-476f-43c1-aa61-f8d92f33029a",
      allow_synonyms = "yes"
    ),
    "`allow_synonyms` should be a single \\(scalar\\) non-NA logical value."
  )
  
  expect_error(
    get_nubkeys(
      datasetKey = "e082b10e-476f-43c1-aa61-f8d92f33029a",
      allow_synonyms = NA
    ),
    "`allow_synonyms` should be a single \\(scalar\\) non-NA logical value."
  )
  
  expect_error(
    get_nubkeys(
      datasetKey = "e082b10e-476f-43c1-aa61-f8d92f33029a",
      allow_synonyms = NULL
    ),
    "`allow_synonyms` should be a single \\(scalar\\) non-NA logical value."
  )
})

# Test output structure ----

test_that("get_nubkeys returns a vector", {
  skip_on_cran()
  
  dataset_key <- "e082b10e-476f-43c1-aa61-f8d92f33029a"
  
  # With synonyms
  result_with_synonyms <- get_nubkeys(
    datasetKey = dataset_key,
    allow_synonyms = TRUE
  )
  
  expect_true(is.vector(result_with_synonyms))
  expect_type(result_with_synonyms, "integer")
  
  # Without synonyms
  result_without_synonyms <- get_nubkeys(
    datasetKey = dataset_key,
    allow_synonyms = FALSE
  )
  
  expect_true(is.vector(result_without_synonyms))
  expect_type(result_without_synonyms, "integer")
})

test_that("get_nubkeys returns unique values", {
  skip_on_cran()
  
  dataset_key <- "e082b10e-476f-43c1-aa61-f8d92f33029a"
  
  # With synonyms
  result_with_synonyms <- get_nubkeys(
    datasetKey = dataset_key,
    allow_synonyms = TRUE
  )
  
  expect_equal(length(result_with_synonyms), length(unique(result_with_synonyms)))
  
  # Without synonyms
  result_without_synonyms <- get_nubkeys(
    datasetKey = dataset_key,
    allow_synonyms = FALSE
  )
  
  expect_equal(
    length(result_without_synonyms),
    length(unique(result_without_synonyms))
  )
})

# Test functional behavior ----

test_that(
  "allow_synonyms = FALSE returns <= keys than allow_synonyms = TRUE", {
  skip_on_cran()
  
  dataset_key <- "e082b10e-476f-43c1-aa61-f8d92f33029a"
  
  result_with_synonyms <- get_nubkeys(
    datasetKey = dataset_key,
    allow_synonyms = TRUE
  )
  
  result_without_synonyms <- get_nubkeys(
    datasetKey = dataset_key,
    allow_synonyms = FALSE
  )
  
  # When synonyms are resolved to accepted taxa, there should be fewer or equal keys
  expect_true(length(result_without_synonyms) <= length(result_with_synonyms))
})

test_that("get_nubkeys default parameter is allow_synonyms = TRUE", {
  skip_on_cran()
  
  dataset_key <- "e082b10e-476f-43c1-aa61-f8d92f33029a"
  
  result_default <- get_nubkeys(datasetKey = dataset_key)
  result_explicit <- get_nubkeys(
    datasetKey = dataset_key,
    allow_synonyms = TRUE
  )
  
  expect_equal(result_default, result_explicit)
})

# Warning is returned if some taxa are not matching to GBIF Backbone
test_that("Warning is given for taxa not matched to GBIF Backbone", {
  skip_on_cran()
  
  dataset_key <- "79d65658-526c-4c78-9d24-1870d67f8439"
  
  expect_warning(
    get_nubkeys(datasetKey = dataset_key),
    regexp = paste(
      "\\d+ taxa from the checklist with datasetKey",
       dataset_key,
      "are not matched to GBIF Backbone and are ignored."
    )
  )
})
