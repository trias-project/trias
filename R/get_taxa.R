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
#' @importFrom purrr map
#' @importFrom lazyeval interp
get_taxa <- function(
  taxon_keys = NULL,
  checklist_keys = NULL,
  limit = NULL) {
  
  # test incoming arguments
  assertthat::assert_that(!all(!is.null(taxon_keys), !is.null(checklist_keys)),
              msg = paste("Both taxon_keys and checklist_keys not NULL.", 
                          "You should choose one of the two!", sep = " "))
  
  # test parameter taxon_keys
  if (!is.null(taxon_keys)) {
    assertthat::assert_that(is.numeric(taxon_keys) | is.character(taxon_keys),
                msg = "taxon_keys should be a numeric, character or a vector.")
  }
  
  # test parameter checklist_keys
  if (!is.null(checklist_keys)) {
    assertthat::assert_that(is.character(checklist_keys),
                msg = "checklist_keys should be a character or a vector.")
  }
  
  if (!is.null(limit)) {
    assertthat::assert_that(is.numeric(limit),
                            msg = "Limit has to be numeric.")
    assertthat::assert_that(limit > 0,
                            msg = "Limit has to be a positive number.")
  }
  
  # working with taxon_keys
  if (!is.null(taxon_keys)) {
    return <- "taxon"
    if (is.null(limit)) {
      maxlimit <- length(taxon_keys)
    } else {
      if (limit > length(taxon_keys)) {
        warning("Limit is higher than number of taxon keys.")
        maxlimit = length(taxon_keys)
      } else maxlimit <- limit
    }
    taxon_taxa <- as.data.frame(as.integer(taxon_keys[1:maxlimit])) %>% 
      rowwise() %>%
      do_(interp(~ as.data.frame(rgbif::name_usage(key = .,
                                                     return = "data")))) 
      # GBIF Backbone matching
      number_no_nubkey <- nrow(taxon_taxa %>% filter(is.na(nubKey)))
  }
  
  # working with checklist_keys
  if (!is.null(checklist_keys) & is.character(checklist_keys)) {
    return <- "checklist"
    if (is.null(limit)) {
      maxlimit <- 1000 # 1000 is the hard maximum (up to now) for name_usage()
    } else maxlimit <- limit
    checklist_taxa <- as.data.frame(checklist_keys) %>% rowwise() %>%
      do_(interp(~ as.data.frame(rgbif::name_usage(datasetKey = .,
                                                     limit = maxlimit,
                                                     return = "data"))))
    if (!is.null(limit) & (nrow(checklist_taxa) < maxlimit)) {
        warning("Dataset contains less records than limit.")
    }
    
    # GBIF Backbone matching
    number_no_nubkey <- nrow(checklist_taxa %>% 
                               filter(is.na(nubKey)))
  }
  
  # print on screen GBIF Backbone matching
  if (number_no_nubkey == 0) {
    print("All taxon keys match GBIF Backbone.")
  } else {
    print(paste("No match with GBIF Backbone for", number_no_nubkey,
                "taxon keys.", sep = " "))
  }

  # select output
  return <- match.arg(return, c("taxon","checklist"))
  switch(return,
         taxon = taxon_taxa,
         checklist = checklist_taxa
  )
}
