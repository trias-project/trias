#' Verify and update taxa information.
#'
#' This function checks all input taxa by adding automatically a verification
#' key where possible (taxa with taxonomic status equal to \code{accepted} or
#' \code{doubful}). The function also update a dataframe related to taxa which
#' need expert's verification. Ancillary information are also given to explore
#' the changes.
#' @param taxa a dataframe with at least the following columns: \itemize{
#'   \item{taxonKey} {: taxon keys as mentioned in checklist.}
#'   \item{scientificName} {: scientific name as provided by GBIF.}
#'   \item{datasetKey} {: dataset key (a UUID) of the checklist the taxon comes
#'   from.} \item{bb_key} {: GBIF backbone key.} \item{bb_scientificName}
#'   {:scientific name as provided by GBIF backbone.} \item{bb_kingdom} {:
#'   kingdom as returned by GBIF Backbone.} \item{bb_rank} {: rank as provided
#'   by GBIF Backbone.} \item{bb_taxonomicStatus} {: taxonomic status as
#'   provided by GBIF Backbone.} \item{bb_acceptedKey} {: accepted key (of
#'   synonym) as provided by GBIF Backbone.} \item{bb_acceptedName} {: accepted
#'   name (ofsynonym) as provided by GBIF Backbone.} }
#' @param taxa_to_verify a dataframe with at least the following columns:
#'   \itemize{ \item{taxonKey} {: taxon keys as mentioned in checklist.}
#'   \item{scientificName: scientific name as provided by GBIF.}
#'   \item{datasetKey} {: dataset key (UUID) of the checklist the taxon comes
#'   from.} \item{bb_key:} {: a GBIF backbone key.} \item{bb_scientificName} {:
#'   scientific name as provided by GBIF backbone.} \item{bb_kingdom} {: kingdom
#'   as returned by GBIF Backbone.} \item{bb_rank} {: rank as provided by GBIF
#'   Backbone.} \item{bb_taxonomicStatus} {: taxonomic status as provided by
#'   GBIF Backbone.} \item{bb_acceptedKey} {: accepted key (of synonym) as
#'   provided by GBIF Backbone.} \item{bb_acceptedName} {: accepted name (of
#'   synonym) as provided by GBIF Backbone.} \item{bb_acceptedKingdom} {:
#'   kingdom of \code{bb_acceptedName} as provided by GBIF backbone. Expected to
#'   be equal to \code{bb_kingdom}.} \item{bb_acceptedRank} {: rank of
#'   \code{bb_acceptedName} as provided by GBIF Backbone.}
#'   \item{bb_acceptedTaxonomicStatus} {: taxonomic status of
#'   \code{bb_acceptedName} as provided by GBIF Backbone.}
#'   \item{verificationKey}{: to be populated manually by expert.}
#'   \item{remarks} {: remarks as provided by the expert (optional).}
#'   \item{dateAdded}{: (date) for new taxa will be populated by function.}
#'   \item{outdated}{: (logical) taxon is not in use (not in \code{taxa}).} }
#' @return a list of three objects: \itemize{ \item{taxa}{: same dataframe as
#'   input taxa with column \code{verificationKey} added. This key is equal to
#'   \code{bb_key} for taxa with \code{bb_taxonomicStatus} one of 'ACCEPTED' or
#'   'DOUBTFUL'. In all other cases (synonym or unmatched taxa) the key provided
#'   by experts in taxa_to_verify, if present, is used.} \item{taxa_to_verify}{:
#'   same dataframe as input taxa_to_verify, but now with updated info. New
#'   synonyms and new unmatched taxa in input taxa are added. Backbone names
#'   (\code{bb_scientificName} and \code{bb_acceptedName}) are updated if
#'   needed.} \item{info}{: a list of dataframes with ancillary informations
#'   related to taxa_to_verify.} } Dataframes contained in info: \itemize{
#'   \item{new_synonyms}{: a subset of taxa_to_verify (same columns) with added
#'   synonym relations (found in taxa, but not in taxa_to_verify).}
#'   \item{new_unmatched_taxa}{: a subset of taxa_to_verify (same columns). New
#'   taxa without match to GBIF backbone.} \item{outdated_taxa}{: a subset of
#'   taxa_to_verify (same columns) with unused taxa (found in taxa_to_verify, but
#'   not in taxa).} \item{updated_bb_scientificName}{: a dataframe with
#'   bb_scientificName + updated_bb_scientificName.}
#'   \item{updated_bb_acceptedName}{: a dataframe with bb_acceptedName +
#'   updated_bb_acceptedName.} \item{duplicates}{: a dataframe with all
#'   taxa present in more than one checklist.} \item{check_verificationKey} {: a
#'   dataframe with results of checking all \code{verificationKey} against GBIF
#'   Backbone.} }
#' @examples
#'
#' # output
#' verify_taxa(taxa = taxa_in, taxa_to_verify = taxa_to_verify_in)
#' @export
#' @importFrom assertthat assert_that is.date
#' @importFrom dplyr filter select distinct mutate rename rename_at
#'   arrange bind_rows inner_join anti_join left_join right_join %>% pull
#'   group_by count
#' @importFrom stringr str_remove
#' @importFrom tidyselect one_of everything ends_with
#' @importFrom purrr pmap_dfr
#' @importFrom rgbif name_usage
verify_taxa <- function(taxa, taxa_to_verify) {
  # start checks input
  message("Check input dataframes...", appendLF = FALSE)
  # test taxa
  name_col_taxa <- c("taxonKey", "scientificName", "datasetKey",
                     "bb_key", "bb_scientificName", "bb_kingdom",
                     "bb_rank", "bb_taxonomicStatus", 
                     "bb_acceptedKey", "bb_acceptedName"
                     )
  assertthat::assert_that(is.data.frame(taxa))
  assertthat::assert_that(all(name_col_taxa %in% names(taxa)))
  is.character(c(taxa$scientificName, 
                 taxa$datasetKey,
                 taxa$bb_scientificName,
                 taxa$bb_kingdom,
                 taxa$bb_rank,
                 taxa$bb_taxonomicStatus,
                 taxa$bb_acceptedName
  ))
  is.numeric(c(taxa$taxonKey, 
               taxa$bb_key, 
               taxa$bb_acceptedKey
  ))
  
  # check that accepted or doubtful taxa have a backbone key
  assert_that(
    taxa %>%
      filter(bb_taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL") & 
               is.na(bb_key)) %>% 
      nrow() == 0,
    msg = "Taxa which don't need verification must have a backbone key.")
  
  # test taxa_to_verify
  name_col_verified <- c("taxonKey", "scientificName", "datasetKey", 
                         "bb_key", "bb_scientificName",
                         "bb_kingdom", "bb_rank", "bb_taxonomicStatus", 
                         "bb_acceptedKey", "bb_acceptedName",
                         "bb_acceptedKingdom", "bb_acceptedRank",
                         "bb_acceptedTaxonomicStatus",
                         "verificationKey", "remarks",
                         "dateAdded", "outdated"
  )
  assert_that(is.data.frame(taxa_to_verify))
  assert_that(all(name_col_verified %in% names(taxa_to_verify)))
  is.character(c(taxa_to_verify$scientificName,
                 taxa_to_verify$datasetKey,
                 taxa_to_verify$bb_scientificName,
                 taxa_to_verify$bb_kingdom,
                 taxa_to_verify$bb_rank,
                 taxa_to_verify$bb_taxonomicStatus,
                 taxa_to_verify$bb_acceptedName,
                 taxa_to_verify$bb_acceptedKingdom,
                 taxa_to_verify$bb_acceptedRank,
                 taxa_to_verify$bb_acceptedTaxonomicStatus
  ))
  is.numeric(c(taxa_to_verify$taxonKey, 
               taxa_to_verify$bb_key, 
               taxa_to_verify$bb_acceptedKey
  ))
  is.date(taxa_to_verify$dateAdded)
  is.logical(taxa_to_verify$outdated)
  # allowe multiple comma separated verification keys (character)
  class(taxa_to_verify$verificationKey) <- "character"
  # alow remarks (remarks col empty means for R a column logicals)
  class(taxa_to_verify$remarks) <- "character"
  
  # check for integrity synonym relations
  assert_that(
    taxa_to_verify %>% 
      filter((is.na(bb_acceptedName) & 
                !is.na(bb_acceptedKey)
      ) | 
        (!is.na(bb_acceptedName) & 
           is.na(bb_acceptedKey)
        )
      ) %>% 
      nrow() == 0, 
    msg = paste("bb_acceptedName and bb_acceptedKey",
                "should be both NA or both present."))
  
  # check that only synonyms and unmatched taxa are present in taxa_to_verify
  taxonomic_status <- 
    taxa_to_verify %>%
    distinct(bb_taxonomicStatus) %>%
    filter(!is.na(bb_taxonomicStatus)) %>%
    pull()
  not_allowed_taxonomicStatus <- c("ACCEPTED", "DOUBTFUL")
  assert_that(all(!taxonomic_status %in% not_allowed_taxonomicStatus),
              msg = "Only synonyms and unmatched taxa allowed in taxa_to_verify.")
  message("DONE.", appendLF = TRUE)
  
  ordered_taxon_keys <- 
    taxa %>%
    select(taxonKey)
  
  # find taxa which don't need any verification and assign verificationKey
  message("Assign verificationKey to taxa which don't need verification...", 
          appendLF = FALSE)
  not_to_verify_taxa <- 
    taxa %>%
    filter(bb_taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL")) %>%
    mutate(
      verificationKey = as.character(bb_key))
  message("DONE.", appendLF = TRUE)
  
  # go further with taxa which need verification
  taxa <- 
    taxa %>%
    anti_join(not_to_verify_taxa,
              by = colnames(taxa))
  
  message("Find new synonyms...", appendLF = FALSE) 
  # find new synonyms (= new triplets (taxonKey, bb_key, bb_acceptedKey))
  new_synonyms <- 
    taxa %>%
    # remove not synonyms 
    filter(!is.na(bb_taxonomicStatus) & 
             !bb_taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL")
           ) %>%
    anti_join(taxa_to_verify,
              by = c("taxonKey", "bb_key", "bb_acceptedKey")) %>%
    mutate(
      dateAdded = Sys.Date(),
      verificationKey = NA_character_,
      remarks = NA_character_,
      bb_acceptedKingdom = NA_character_,
      bb_acceptedRank = NA_character_,
      bb_acceptedTaxonomicStatus = NA_character_,
      outdated = FALSE
      ) %>%
    select(one_of(name_col_verified), everything())
  message("DONE.", appendLF = TRUE)
  
  # find new taxa not matched to GBIF backbone 
  message("Find new unmatched taxa...", appendLF = FALSE)
  unmatched_taxa <- 
    taxa_to_verify %>%
    filter(is.na(bb_key)) %>%
    distinct(taxonKey) %>%
    pull()
  new_unmatched_taxa <- 
    taxa %>%
    filter(is.na(bb_key)) %>%
    filter(!taxonKey %in% unmatched_taxa) %>% 
    mutate(dateAdded = Sys.Date(),
           verificationKey = NA_character_,
           remarks = NA_character_,
           bb_acceptedKingdom = NA_character_,
           bb_acceptedRank = NA_character_,
           bb_acceptedTaxonomicStatus = NA_character_,
           outdated = FALSE) %>%
    select(one_of(name_col_verified), everything())
  message("DONE.", appendLF = TRUE)
  
  # create df of updated bb_scientificName
  message("Update backbone scientific names...", appendLF = FALSE)
  updated_bb_scientificName <- 
    taxa_to_verify %>%
    filter(!is.na(bb_scientificName)) %>%
    left_join(taxa, 
              by = c("taxonKey", "bb_key", "bb_acceptedKey")) %>%
    rename("bb_scientificName" = "bb_scientificName.x",
           "updated_bb_scientificName" = "bb_scientificName.y") %>% 
    filter(bb_scientificName != updated_bb_scientificName) %>% 
    select(which(colnames(taxa_to_verify)%in% colnames(.)), 
           updated_bb_scientificName,
           ends_with(".x")) %>%
    rename_at(vars(ends_with(".x")), funs(str_remove(., "\\.x")))
  
  # version for info
  updated_bb_scientificName_short <- 
    updated_bb_scientificName %>%
    select(taxonKey, bb_key, bb_acceptedKey, 
           bb_scientificName, updated_bb_scientificName)
  
  # update bb_scientificName of taxa_to_verify
  taxa_to_verify <- 
    taxa_to_verify %>% 
    anti_join(updated_bb_scientificName,
              by = colnames(taxa_to_verify)) %>%
    bind_rows(updated_bb_scientificName %>%
                mutate(bb_scientificName = updated_bb_scientificName) %>%
                select(-updated_bb_scientificName))
  message("DONE.", appendLF = TRUE)
  
  # create df of updated acceptedName
  message("Update backbone accpeted names...", appendLF = FALSE)
  updated_bb_acceptedName <- 
    taxa_to_verify %>%
    filter(!is.na(bb_acceptedName)) %>%
    left_join(taxa, 
              by = c("taxonKey", "bb_key", "bb_acceptedKey")) %>%
    rename("bb_acceptedName" = "bb_acceptedName.x",
           "updated_bb_acceptedName" = "bb_acceptedName.y") %>% 
    filter(bb_acceptedName != updated_bb_acceptedName) %>% 
    select(which(colnames(taxa_to_verify_in)%in% colnames(.)), 
           updated_bb_acceptedName,
           ends_with(".x")) %>%
    rename_at(vars(ends_with(".x")), funs(str_remove(., "\\.x")))
  
  # version for info
  updated_bb_acceptedName_short <- 
    updated_bb_acceptedName %>%
    select(taxonKey, bb_key, bb_acceptedKey, 
           bb_acceptedName, updated_bb_acceptedName) 

  # update bb_acceptedName of taxa_to_verify
  taxa_to_verify <- 
    taxa_to_verify %>% 
    anti_join(updated_bb_acceptedName,
              by = colnames(taxa_to_verify)) %>%
    bind_rows(updated_bb_acceptedName %>%
                mutate(bb_acceptedName = updated_bb_acceptedName) %>%
                select(-updated_bb_acceptedName))
  message("DONE.", appendLF = TRUE)
  
  # add new synonyms to taxa_to_verify
  taxa_to_verify <- 
    taxa_to_verify %>% 
    bind_rows(new_synonyms)
  
  # add new unmatches to taxa_to_verify
  taxa_to_verify <- 
    taxa_to_verify %>% 
    bind_rows(new_unmatched_taxa)
  
  # retrieve backbone information about taxa the synonyms point to
  message("Retrieve backbone info about taxa the synonyms point to...",
          appendLF = FALSE)
  accepted_keys <- 
    taxa_to_verify %>%
    distinct(bb_acceptedKey) %>%
    filter(!is.na(bb_acceptedKey))
  accepted_info <- pmap_dfr(accepted_keys, 
                            function(bb_acceptedKey) {
                              name_usage(key = bb_acceptedKey, 
                                         return = "data")
                            }) %>%
    select(key, kingdom, rank, taxonomicStatus) %>%
    rename(bb_acceptedKey = key,
           bb_acceptedKingdom = kingdom, 
           bb_acceptedRank = rank,
           bb_acceptedTaxonomicStatus = taxonomicStatus)
  
  # Update backbone info about accepted taxa in taxa_to_verify
  taxa_to_verify <- 
    taxa_to_verify %>%
    select(-c(bb_acceptedKingdom, 
              bb_acceptedRank, 
              bb_acceptedTaxonomicStatus)) %>%
    left_join(accepted_info, by = "bb_acceptedKey") %>%
    select(name_col_verified)
  message("DONE.", appendLF = TRUE)
  
  # handle outdated taxa
  message("Detect outdated data...", appendLF = FALSE)
  # set outdated = FALSE for taxa which are in use: 
  # some outdated taxa could be back in use
  not_outdated_taxa <-
    taxa_to_verify %>%
    inner_join(taxa %>%
                select(taxonKey, bb_key, bb_acceptedKey),
              by = c("taxonKey", "bb_key", "bb_acceptedKey")) %>%
    mutate(outdated = FALSE) %>%
    mutate(remarks = str_remove(remarks, "Outdated taxa."))
  # define the outdated taxa subset
  outdated_taxa <- 
    taxa_to_verify %>% 
    anti_join(taxa, by = c("taxonKey", "bb_key", "bb_acceptedKey"))

  # not add 'Outdated taxa' in remarks to already outdated taxa
  old_outdated_taxa <-
    outdated_taxa %>%
    filter(outdated == TRUE)
  # set outdated = TRUE, add 'Outdated taxa.' to remarks for new outdated taxa
  new_outdated_taxa <- 
    outdated_taxa %>%
    filter(outdated == FALSE) %>%
    mutate(remarks = paste(remarks, "Outdated taxa."),
           outdated = TRUE)
  outdated_taxa <- bind_rows(old_outdated_taxa, new_outdated_taxa)
  # compose vreified_taxa back together
  taxa_to_verify <- 
    not_outdated_taxa %>%
    bind_rows(outdated_taxa)
  message("DONE.", appendLF = TRUE)
  
  # check verificationKey values against GBIF and GBIF Backbone
  message("Check verification keys...", appendLF = FALSE)
  verification_keys <- taxa_to_verify %>% 
    filter(!is.na(verificationKey))  %>%
    pull(verificationKey)
  verification_keys <- paste(verification_keys, collapse = ",")
  verification_keys <- unlist(stringr::str_split(verification_keys, ","))
  check_verificationKey <- gbif_verify_keys(verification_keys)
  message("DONE.", appendLF = TRUE)
  
  # find taxa duplicates 
  message("Find scientific names used in multiple taxa...", appendLF = FALSE)
  duplicates <-
    taxa_to_verify %>%
    filter(!is.na(bb_key) & !is.na(bb_acceptedKey)) %>%
    group_by(bb_key, bb_acceptedKey) %>%
    count() %>%
    filter(n > 1) %>%
    left_join((taxa_to_verify %>%
                select(bb_key, bb_acceptedKey, bb_scientificName)),
              by = c("bb_key", "bb_acceptedKey"))
  message("DONE.", appendLF = TRUE)
  
  # order taxa_to_verify by outdated and dateAdded
  taxa_to_verify <-
    taxa_to_verify %>%
    arrange(outdated, dateAdded)
  
  # add not outdated taxa from taxa_to_verify to not_to_verify_taxa
  taxa <- 
    taxa_to_verify %>%
    filter(outdated == FALSE) %>%
    select(names(taxa), verificationKey) %>%
    bind_rows(not_to_verify_taxa) %>%
    right_join(ordered_taxon_keys, by = "taxonKey")

  
  return(list(taxa = taxa,
              taxa_to_verify = taxa_to_verify,
              info = list(
                new_synonyms = new_synonyms,
                new_unmatched_taxa = new_unmatched_taxa,
                outdated_taxa = outdated_taxa,
                updated_bb_scientificName = updated_bb_scientificName_short,
                updated_bb_acceptedName = updated_bb_acceptedName_short,
                duplicates = duplicates,
                check_verificationKey = check_verificationKey
              )
  ))
}
