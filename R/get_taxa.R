#' Get taxa and combine it with backbone information
#'
#' This function retrieves taxa information. It is a higher level function built 
#' on rgbif function `name_usage()`.
#' @param taxon_keys (single numeric or character or a vector) a single key or a 
#' vector of keys. Not to use together with `checklist_keys`.
#' @param checklist_keys (single character or a vector) a datasetKey (character) 
#' or a vector of datasetkeys. Not to use together with `checklist_keys`.
#' @param limit With taxon_keys: limit number of taxa. 
#' With checklist_keys: limit number of taxa per each dataset.
#' A warning is given if limit is higher than the length of taxon_keys or number of records
#' in the checklist_keys (if string) or any of the checklist_keys (if vector)
#' @return A data.frame with all returned attributes for any taxa
#' @examples
#' # A single numeric taxon_keys
#' get_taxa(taxon_keys = 1)
#' # A single character taxon_keys
#' get_taxa(taxon_keys = "1")
#' # Multiple numeric taxon_keys (vector)
#' get_taxa(taxon_keys = c(1,2,3,4,5,6))
#' # Multiple character taxon_keys (vector)
#' get_taxa(taxon_keys = c("1","2","3","4","5","6"))
#' # Limit number of taxa (coupled with taxon_keys)
#' get_taxa(taxon_keys = c(1,2,3,4,5,6), limit = 3)
#' # A single checklist_keys (character)
#' get_taxa(checklist_keys = "b3fa7329-a002-4243-a7a7-cd066092c9a6")
#' # Multiple checklist_keys (vector)
#' get_taxa(checklist_keys = c("e4746398-f7c4-47a1-a474-ae80a4f18e92", 
#'                             "b3fa7329-a002-4243-a7a7-cd066092c9a6"))
#' # Limit number of taxa (coupled with checklist_keys)
#' get_taxa(checklist_keys = c("e4746398-f7c4-47a1-a474-ae80a4f18e92", 
#'                             "b3fa7329-a002-4243-a7a7-cd066092c9a6"), 
#'          limit = 30)
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
