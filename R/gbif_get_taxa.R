#' Get taxa information from GBIF
#'
#' This function retrieves taxa information from GBIF. It is a higher level function built 
#' on rgbif functions `name_usage()` and `name_lookup()`.
#' @param taxon_keys (single numeric or character or a vector) a single key or a 
#' vector of keys. Not to use together with `checklist_keys`.
#' @param checklist_keys (single character or a vector) a datasetKey (character) 
#' or a vector of datasetkeys. Not to use together with `checklist_keys`.
#' @param origin (single character or a vector) filter by origin. 
#' It can take many inputs, and treated as OR (e.g., a or b or c)
#' To be used only in combination with `checklist_keys`. Ignored otherwise.
#' @param limit With taxon_keys: limit number of taxa. 
#' With checklist_keys: limit number of taxa per each dataset.
#' A warning is given if limit is higher than the length of taxon_keys or number of records
#' in the checklist_keys (if string) or any of the checklist_keys (if vector)
#' @return A data.frame with all returned attributes for any taxa
#' @examples
#' # A single numeric taxon_keys
#' gbif_get_taxa(taxon_keys = 1)
#' # A single character taxon_keys
#' gbif_get_taxa(taxon_keys = "1")
#' # Multiple numeric taxon_keys (vector)
#' gbif_get_taxa(taxon_keys = c(1,2,3,4,5,6))
#' # Multiple character taxon_keys (vector)
#' gbif_get_taxa(taxon_keys = c("1","2","3","4","5","6"))
#' # Limit number of taxa (coupled with taxon_keys)
#' gbif_get_taxa(taxon_keys = c(1,2,3,4,5,6), limit = 3)
#' # A single checklist_keys (character)
#' gbif_get_taxa(checklist_keys = "b3fa7329-a002-4243-a7a7-cd066092c9a6")
#' # Multiple checklist_keys (vector)
#' gbif_get_taxa(checklist_keys = c("e4746398-f7c4-47a1-a474-ae80a4f18e92", 
#'                             "b3fa7329-a002-4243-a7a7-cd066092c9a6"))
#' # Limit number of taxa (coupled with checklist_keys)
#' gbif_get_taxa(checklist_keys = c("e4746398-f7c4-47a1-a474-ae80a4f18e92", 
#'                             "b3fa7329-a002-4243-a7a7-cd066092c9a6"), 
#'          limit = 30)
#' # Filter by origin
#' gbif_get_taxa(checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42", 
#'          origin = "source", limit = 3000)
#' gbif_get_taxa(checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42", 
#'          origin = c("source" ,"denormed_classification"), limit = 3000)
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rgbif name_usage name_lookup
#' @importFrom dplyr filter mutate rowwise do_ ungroup %>%
#' @importFrom tibble tibble
#' @importFrom lazyeval interp
#' @importFrom stringr str_to_lower
#' @importFrom magrittr %<>%

gbif_get_taxa <- function(
  taxon_keys = NULL,
  checklist_keys = NULL,
  origin = NULL,
  limit = NULL) {
  
  # test incoming arguments
  assert_that(!all(!is.null(taxon_keys), !is.null(checklist_keys)),
              msg = paste("Both taxon_keys and checklist_keys not NULL.", 
                          "You should choose one of the two!"))
  
  # test parameter taxon_keys
  if (!is.null(taxon_keys)) {
    assert_that(is.numeric(taxon_keys) | is.character(taxon_keys),
                msg = "taxon_keys should be a numeric, character or a vector.")
  }
  
  # test parameter checklist_keys
  if (!is.null(checklist_keys)) {
    assert_that(is.character(checklist_keys),
                msg = "checklist_keys should be a character or a vector.")
  }
  
  # test limit
  if (!is.null(limit)) {
    assert_that(is.numeric(limit), msg = "Limit has to be numeric.")
    assert_that(limit > 0, msg = "Limit has to be a positive number.")
  }
  
  # test number of taxa
  if (!is.null(checklist_keys) & !is.null(limit)) {
    assert_that(limit < 100000, 
                msg = "Too many keys. API maximum is 99999.")
    if (limit * length(checklist_keys) > 100000)
      warning(paste("Attention: if all datasets contain at least as many taxa",
                    "as limit, you are querying too many taxa.",
                    "API maximum is 99999."))
  }
  
  # test origin and set to lower
  if (!is.null(origin)) {
    assert_that(is.character(origin),
                            msg = "origin should be a character or a vector.")
    origins <- stringr::str_to_lower(origin)
    if (!is.null(taxon_keys))
      warning("origin parameter ignored if used in combination with taxon_keys.")
  }
  
  # working with taxon_keys
  if (!is.null(taxon_keys)) {
    return <- "taxon"
    if (is.null(limit)) {
      maxlimit <- length(taxon_keys)
    } else {
      if (limit > length(taxon_keys)) {
        warning("Limit is higher than number of taxon keys.")
        maxlimit <- length(taxon_keys)
      } else maxlimit <- limit
    }
    taxon_keys = as.integer(taxon_keys[1:maxlimit])
    taxon_keys_df <- as.data.frame(taxon_keys)
    taxon_taxa <-  taxon_keys_df %>% 
      rowwise() %>%
      do_(interp(~ as.data.frame(name_usage(key = .$taxon_keys,
                                                     return = "data")))) 
    taxon_taxa %<>% 
      ungroup %>% 
      mutate(origin = str_to_lower(origin))
    if (!is.null(origin))
      taxon_taxa %<>% filter(origin %in% origins)
    
    # GBIF Backbone matching
    number_key <- nrow(taxon_taxa)
    number_no_nubkey <- taxon_taxa %>% filter(is.na(nubKey)) %>% nrow
  }
  
  # working with checklist_keys
  if (!is.null(checklist_keys) & is.character(checklist_keys)) {
    return <- "checklist"
    if (is.null(limit)) {
      maxlimit <- 99999 # after paging implmentation, set maxlimit <- 99999
    } else {
      maxlimit <- limit
    }
    
    if (!is.null(origin)) {
      checklist_keys = as.character(checklist_keys)
      checklist_keys_df <- as.data.frame(checklist_keys)
      checklist_taxa <- checklist_keys_df %>% 
        rowwise() %>%
        do_(interp(~ as.data.frame(name_lookup(datasetKey = .,
                                                      origin = origins,
                                                      limit = maxlimit,
                                                      return = "data"))))
    } else {
      checklist_taxa <- as.data.frame(checklist_keys) %>% rowwise() %>%
        do_(interp(~ as.data.frame(name_lookup(datasetKey = .,
                                                      limit = maxlimit,
                                                      return = "data"))))
    }
    
    checklist_taxa %<>% 
      ungroup %>% 
      mutate(origin = str_to_lower(origin))
    
    if (!is.null(limit) & 
        (nrow(checklist_taxa) < maxlimit*length(checklist_keys))) {
      if (length(checklist_keys) > 1) {
        warning("One of the datasets contains less records than limit.")
      } else {
        warning("Dataset contains less records than limit.")
      }
    }
    
    # GBIF Backbone matching
    number_key <- nrow(checklist_taxa)
    number_no_nubkey <- checklist_taxa %>% filter(is.na(nubKey)) %>% nrow
  }
  
  # print GBIF Backbone matching on screen
  if (number_no_nubkey == 0) {
    print(paste("All", number_key, "taxa match GBIF Backbone."))
  } else {
    print(paste(number_key, "taxa found of which", number_no_nubkey, 
                "could not be matched to GBIF Backbone."))
  }
  
  # select output
  return <- match.arg(return, c("taxon","checklist"))
  switch(return,
         taxon = taxon_taxa,
         checklist = checklist_taxa
  )
}
