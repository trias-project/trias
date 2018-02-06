#' Get taxa and combine it with backbone information
#'
#' This function gets taxa and combine informations from GBIF backbone.
#'
#' @param taxon_keys a key (numeric or character), a vector of keys, or the output
#' of a call to name_usage()
#' @param checklist_keys a datasetKey (character), a vector of datasetkeys
#' @param limit With taxon_keys: limit to `limit` taxa. With  `checklist_keys`: 
#' limit to `limit` taxa PER DATASET
#' @return A dataframe with all returned attributes for any taxa
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rgbif name_usage
#' @importFrom tibble tibble
get_taxa <- function(
  taxon_keys = NULL,
  checklist_keys = NULL,
  limit = NULL) {
  API_terms = c("key", "nubKey", "nameKey", "taxonID", "sourceTaxonKey",
                "kingdom", "phylum", "order", "family", "genus", "species",
                "kingdomKey", "phylumKey", "classKey", "orderKey", "familyKey",
                "genusKey", "speciesKey", "datasetKey", "constituentKey",
                "parentKey", "parent", "basionymKey", "basionym", 
                "scientificName", "canonicalName", "authorship","nameType",
                "rank", "origin", "taxonomicStatus", "nomenclaturalStatus", 
                "remarks", "publishedIn", "numDescendants", "lastCrawled",
                "lastInterpreted","issues", "synonym", "class")
  
  # test incoming arguments
  assertthat::assert_that(!all(!is.null(taxon_keys), !is.null(checklist_keys)),
              msg = paste("Both taxon_keys and checklist_keys not NULL.", 
                          "You should choose one of the two!", sep = " "))
  
  # test parameter taxon_keys
  if (!is.null(taxon_keys)) {
    assertthat::assert_that(is.numeric(taxon_keys) | is.character(taxon_keys) |
                              inherits(taxon_keys, "list") | 
                              inherits(taxon_keys, "tbl_df"),
              msg = paste("taxon_keys should be a numeric, character, vector,",
                          "tibble data.frame or a list.", sep = " "))
  
    if (inherits(taxon_keys, "list")) {
      assertthat::assert_that(isTRUE(all.equal(names(taxon_keys), c("meta", "data"))),
                  msg = paste("List is corrupted: meta and data are both",
                              "expected and nothing else.", sep = " "))
      assertthat::assert_that(isTRUE(all.equal(class(taxon_keys$data), 
                                               c("tbl_df","tbl","data.frame"))),
                  msg = paste("List is corrupted: class(data) !=", 
                              "class(name_usage(key = ...)), tibble.", sep = " "))
      gbif_terms <- colnames(taxon_keys$data)
      assertthat::assert_that(isTRUE(all(gbif_terms %in% API_terms)),
                              msg = "One or more attributes don't match API terms.")
    }
    
    if (inherits(taxon_keys, "tbl_df")) {
      gbif_terms <- colnames(taxon_keys)
      assertthat::assert_that(isTRUE(all(gbif_terms %in% API_terms)),
                              msg = "One or more attributes don't match API terms.")
    }
  }
  
  # test parameter checklist_keys
  if (!is.null(checklist_keys)) {
    assertthat::assert_that(is.character(checklist_keys) |
                              inherits(checklist_keys, "list") | 
                              inherits(checklist_keys, "tbl_df"),
                          msg = paste("checklist_keys should be a character,",
                                      "vector, tibble data.frame or a list.", 
                                      sep = " "))
    if (inherits(checklist_keys, "list")) {
      assertthat::assert_that(isTRUE(all.equal(names(checklist_keys), 
                                               c("meta", "data"))),
                              msg = paste("List is corrupted: meta and data are",
                                          "both expected and nothing else.", 
                                          sep = " "))
      assertthat::assert_that(isTRUE(all.equal(class(checklist_keys$data), 
                                               c("tbl_df","tbl","data.frame"))),
                  msg = paste("List is corrupted: class(checklist_keys$data) !=", 
                              "class(name_usage(datasetKey = ...)), tibble.", 
                              sep = " "))
      gbif_terms <- colnames(checklist_keys$data)
      assertthat::assert_that(isTRUE(all(gbif_terms %in% API_terms)),
                              msg = "One or more attributes don't match API terms.")
    }
    
    if (inherits(checklist_keys, "tbl_df")) {
      gbif_terms <- colnames(checklist_keys)
      assertthat::assert_that(isTRUE(all(gbif_terms %in% API_terms)),
                              msg = "One or more attributes don't match API terms.")
    }
    
  }
  
  if (!is.null(limit)) {
    assertthat::assert_that(is.numeric(class(limit)),
                            msg = "Limit has to be numeric.")
  }
  }
}
