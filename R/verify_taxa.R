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
#'   from.} 
#'   \item{bb_key} {: GBIF backbone key.} 
#'   \item{bb_scientificName} {:scientific name as provided by GBIF backbone.} 
#'   \item{bb_kingdom} {: kingdom
#'   as returned by GBIF Backbone.} 
#'   \item{bb_rank} {: rank as provided by GBIF Backbone.} 
#'   \item{bb_taxonomicStatus} {: taxonomic status as provided by
#'   GBIF Backbone.} 
#'   \item{bb_acceptedKey} {: accepted key (of synonym) as
#'   provided by GBIF Backbone.} 
#'   \item{bb_acceptedName} {: accepted name (ofsynonym) as provided by GBIF
#'   Backbone.} 
#'   }
#' @param verified_taxa a dataframe with at least the following columns:
#'   \itemize{ 
#'     \item{taxonKey} {: taxon keys as mentioned in checklist.}
#'     \item{scientificName: scientific name as provided by GBIF.} 
#'     \item{datasetKey} {: dataset key (UUID) of the checklist the taxon comes
#'     from.}
#'     \item{bb_key:} {: a GBIF backbone key.} 
#'     \item{bb_scientificName} {: scientific name as provided by GBIF
#'     backbone.}
#'     \item{bb_kingdom} {: kingdom as returned by GBIF Backbone.} 
#'     \item{bb_rank} {: rank as provided by GBIF Backbone.} 
#'     \item{bb_taxonomicStatus} {: taxonomic status as provided by GBIF
#'     Backbone.}
#'     \item{bb_acceptedKey} {: accepted key (of synonym) as provided by GBIF
#'     Backbone.}
#'     \item{bb_acceptedName} {: accepted name (of synonym) as provided by GBIF
#'     Backbone.}
#'     \item{bb_acceptedKingdom} {: kingdom of \code{bb_acceptedName} as
#'     provided by GBIF backbone. Expected to be equal to \code{bb_kingdom}.}
#'     \item{bb_acceptedRank} {: rank of \code{bb_acceptedName} as provided by
#'     GBIF Backbone.}
#'     \item{bb_acceptedTaxonomicStatus} {: taxonomic status of
#'     \code{bb_acceptedName} as provided by GBIF Backbone.}
#'     \item{verificationKey}{: to be populated manually by expert.}
#'     \item{remarks} {: remarks as provided by the expert (optional).}
#'     \item{dateAdded}{: (date) for new taxa will be populated by function.}
#'     \item{outdated}{: (logical) taxon is not in \code{taxa}.}
#'     }
#' @return a list of three objects: \itemize{ 
#'   \item{taxa}{: same dataframe as input taxa with column
#'   \code{verificationKey} added. This key is equal to \code{bb_key} for taxa
#'   with \code{bb_taxonomicStatus} one of 'ACCEPTED' or 'DOUBTFUL'. In all
#'   other cases (synonym or unmatched taxa) the key provided by experts in
#'   verified_taxa, if present, is used.}
#'   \item{verified_taxa}{: same dataframe as input verified_taxa, but now with
#'   updated info. New synonyms and new unmatched taxa in input taxa are added.
#'   Backbone names (\code{bb_scientificName} and \code{bb_acceptedName}) are
#'   updated if needed.}
#'   \item{info}{: a list of dataframes with ancillary informations related to
#'   verified_taxa.}
#'   }
#'   Dataframes contained in info: \itemize{
#'   \item{new_synonyms}{: a subset of verified_taxa (same columns) with added
#'   synonym relations (found in taxa, but not in verified_taxa).}
#'   \item{new_unmatched_taxa}{: a subset of verified_taxa (same columns). New
#'   taxa without match to GBIF backbone.}
#'   \item{outdated_taxa}{: a subset of verified_taxa (same columns) with unused
#'   taxa (found in verified_taxa, but not in taxa).}
#'   \item{updated_bb_scientificName}{: a dataframe with bb_scientificName +
#'   updated_bb_scientificName.}
#'   \item{updated_bb_acceptedName}{: a dataframe with bb_acceptedName +
#'   updated_bb_acceptedName.}
#'   \item{duplicates_taxa}{: a dataframe with all taxa present in more than
#'   one checklist.}
#'   \item{check_verificationKey} {: a dataframe with results of checking all
#'   \code{verificationKey} against GBIF Backbone.}
#'   }
#' @examples
#' taxa_in <- data.frame(
#'   taxonKey = c(
#'     141117238,
#'     113794952,
#'     141264857,
#'     100480872,
#'     141264614,
#'     100220432,
#'     141264835,
#'     140563014,
#'     140562956,
#'     145953989,
#'     148437916,
#'     114445583
#'   ),
#'   scientificName = c(
#'     "Aspius aspius",
#'     "Rana catesbeiana",
#'     "Polystichum tsus-simense J.Smith",
#'     "Apus apus (Linnaeus, 1758)",
#'     "Begonia x semperflorens hort.",
#'     "Rana catesbeiana",
#'     "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley",
#'     "Atyaephyra desmaresti",
#'     "Ferrissia fragilis",
#'     "Ferrissia fragilis",
#'     "Ferrissia fragilis",
#'     "Rana blanfordii Boulenger"
#'   ),
#'   datasetKey = c(
#'     "98940a79-2bf1-46e6-afd6-ba2e85a26f9f",
#'     "e4746398-f7c4-47a1-a474-ae80a4f18e92",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "39653f3e-8d6b-4a94-a202-859359c164c5",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "b351a324-77c4-41c9-a909-f30f77268bc4",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "289244ee-e1c1-49aa-b2d7-d379391ce265",
#'     "289244ee-e1c1-49aa-b2d7-d379391ce265",
#'     "3f5e930b-52a5-461d-87ec-26ecd66f14a3",
#'     "1f3505cd-5d98-4e23-bd3b-ffe59d05d7c2",
#'     "3772da2f-daa1-4f07-a438-15a881a2142c"
#'   ),
#'   bb_scientificName = c(
#'     "Aspius aspius (Linnaeus, 1758)",
#'     "Rana catesbeiana Shaw, 1802",
#'     "Polystichum tsus-simense (Hook.) J.Sm.",
#'     "Apus apus (Linnaeus, 1758)",
#'     NA,
#'     "Rana catesbeiana Shaw, 1802",
#'     NA,
#'     "Atyaephyra desmarestii (Millet, 1831)",
#'     "Ferrissia fragilis (Tryon, 1863)",
#'     "Ferrissia fragilis (Tryon, 1863)",
#'     "Ferrissia fragilis (Tryon, 1863)",
#'     "Rana blanfordii Boulenger, 1882"
#'   ),
#'   bb_key = c(
#'     2360181,
#'     2427092,
#'     2651108,
#'     5228676,
#'     NA,
#'     2427092,
#'     NA,
#'     4309705,
#'     2291152,
#'     2291152,
#'     2291152,
#'     2430304
#'   ),
#'   bb_kingdom = c(
#'     "Animalia",
#'     "Animalia",
#'     "Plantae",
#'     "Animalia",
#'     NA,
#'     "Animalia",
#'     NA,
#'     "Animalia",
#'     "Animalia",
#'     "Animalia",
#'     "Animalia",
#'     "Animalia"
#'   ),
#'   bb_rank = c("SPECIES",
#'               "SPECIES",
#'               "SPECIES",
#'               "SPECIES",
#'               NA,
#'               "SPECIES",
#'               NA,
#'               "SPECIES",
#'               "SPECIES",
#'               "SPECIES",
#'               "SPECIES",
#'               "SPECIES"
#'   ),
#'   bb_taxonomicStatus = c(
#'     "SYNONYM",
#'     "SYNONYM",
#'     "SYNONYM",
#'     "ACCEPTED",
#'     NA,
#'     "SYNONYM",
#'     NA,
#'     "HOMOTYPIC_SYNONYM",
#'     "SYNONYM",
#'     "SYNONYM",
#'     "SYNONYM",
#'     "SYNONYM"
#'   ),
#'   bb_acceptedName = c(
#'     "Leuciscus aspius (Linnaeus, 1758)",
#'     "Lithobates catesbeianus (Shaw, 1802)",
#'     "Polystichum luctuosum (Kunze) Moore.",
#'     NA,
#'     NA,
#'     "Lithobates catesbeianus (Shaw, 1802)",
#'     NA,
#'     "Hippolyte desmarestii Millet, 1831",
#'     "Ferrissia californica (Rowell, 1863)",
#'     "Ferrissia californica (Rowell, 1863)",
#'     "Ferrissia californica (Rowell, 1863)",
#'     "Nanorana blanfordii (Boulenger, 1882)"
#'   ),
#'   bb_acceptedKey = c(
#'     5851603,
#'     2427091,
#'     4046493,
#'     NA,
#'     NA,
#'     2427091,
#'     NA,
#'     6454754,
#'     9520065,
#'     9520065,
#'     9520065,
#'     2430301
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' 
#' verified_taxa_in <- data.frame(
#'   taxonKey = c(
#'     113794952,
#'     141264857,
#'     143920280,
#'     141264835,
#'     141264614,
#'     140562956,
#'     145953989,
#'     114445583
#'   ),
#'   scientificName = c(
#'     "Rana catesbeiana",
#'     "Polystichum tsus-simense J.Smith",
#'     "Lemnaceae",
#'     "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley",
#'     "Begonia x semperflorens hort.",
#'     "Ferrissia fragilis",
#'     "Ferrissia fragilis",
#'     "Rana blanfordii Boulenger"
#'   ),
#'   datasetKey = c(
#'     "e4746398-f7c4-47a1-a474-ae80a4f18e92",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "e4746398-f7c4-47a1-a474-ae80a4f18e92",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "289244ee-e1c1-49aa-b2d7-d379391ce265",
#'     "3f5e930b-52a5-461d-87ec-26ecd66f14a3",
#'     "3772da2f-daa1-4f07-a438-15a881a2142c"
#'   ),
#'   bb_key = c(2427092,
#'              2651108,
#'              6723,
#'              NA,
#'              NA,
#'              2291152,
#'              2291152,
#'              2430304
#'   ),
#'   bb_scientificName = c(
#'     "Rana catesbeiana Shaw, 1802",
#'     "Polystichum tsus-tsus-tsus (Hook.) Captain",
#'     "Lemnaceae",
#'     NA,
#'     NA,
#'     "Ferrissia fragilis (Tryon, 1863)",
#'     "Ferrissia fragilis (Tryon, 1863)",
#'     "Rana blanfordii Boulenger, 1882"
#'   ),
#'   bb_kingdom = c("Animalia",
#'                  "Plantae",
#'                  "Plantae",
#'                  NA,
#'                  NA,
#'                  "Animalia",
#'                  "Animalia",
#'                  "Animalia"
#'   ),
#'   bb_rank = c("SPECIES",
#'               "SPECIES",
#'               "FAMILY",
#'               NA,
#'               NA,
#'               "SPECIES",
#'               "SPECIES",
#'               "SPECIES"
#'   ),
#'   bb_taxonomicStatus = c("SYNONYM",
#'                          "SYNONYM",
#'                          "SYNONYM",
#'                          NA,
#'                          NA,
#'                          "SYNONYM",
#'                          "SYNONYM",
#'                          "SYNONYM"
#'   ),
#'   bb_acceptedName = c(
#'     "Lithobates dummyus (Batman, 2018)",
#'     "Polystichum luctuosum (Kunze) Moore.",
#'     "Araceae",
#'     NA,
#'     NA,
#'     "Ferrissia californica (Rowell, 1863)",
#'     "Ferrissia californica (Rowell, 1863)",
#'     "Hylarana chalconota (Schlegel, 1837)"
#'   ),
#'   bb_acceptedKey = c(2427091,
#'                      4046493,
#'                      6979,
#'                      NA,
#'                      NA,
#'                      9520065,
#'                      9520065,
#'                      2427008
#'   ),
#'   bb_acceptedKingdom = c("Animalia",
#'                          "Plantae",
#'                          "Plantae",
#'                          NA,
#'                          NA,
#'                          "Animalia",
#'                          "Animalia",
#'                          "Animalia"
#'   ),
#'   bb_acceptedRank = c("SPECIES",
#'                       "SPECIES",
#'                       "FAMILY",
#'                       NA,
#'                       NA,
#'                       "SPECIES",
#'                       "SPECIES",
#'                       "SPECIES"
#'   ),
#'   bb_acceptedTaxonomicStatus = c("ACCEPTED",
#'                                  "ACCEPTED",
#'                                  "ACCEPTED",
#'                                  NA,
#'                                  NA,
#'                                  "ACCEPTED",
#'                                  "ACCEPTED",
#'                                  "ACCEPTED"
#'   ),
#'   verificationKey = c(2427091,
#'                       4046493,
#'                       6979,
#'                       "2805420,2805363",
#'                       NA,
#'                       NA,
#'                       NA,
#'                       NA
#'   ),
#'   remarks = c(
#'     "dummy example 1: bb_acceptedName should be updated.",
#'     "dummy example 2: bb_scientificName should be updated.",
#'     "dummy example 3: not used anymore. Set outdated = TRUE. Add 'Outdated taxa.' to remarks.",
#'     "dummy example 4: multiple keys in verificationKey are allowed.",
#'     "dummy example 5: nothing should happen.",
#'     "dummy example 6: datasetKey should not be modified. If new taxa come in
#'     with same name from other checklsits, they should be added as new rows.
#'     Report them as duplicates in duplicates_taxa",
#'     "dummy example 7: datasetKey should not be modified. If new taxa come in
#'     with same name from other checklsits, they should be added as new rows.
#'     Report them as duplicates in duplicates_taxa",
#'     "dummy example 8: outdated synonym. Set outdated = TRUE. Add 'Outdated taxa.' to remarks. Add new synonym relation in a new row."
#'   ),
#'   dateAdded = as.Date(
#'     c(
#'       "2018-07-01",
#'       "2018-07-01",
#'       "2018-07-01",
#'       "2018-07-16",
#'       "2018-07-16",
#'       "2018-07-01",
#'       "2018-11-20",
#'       "2018-11-29"
#'     )
#'   ),
#'   outdated = c(FALSE,
#'                FALSE,
#'                FALSE,
#'                FALSE,
#'                FALSE,
#'                FALSE,
#'                FALSE,
#'                FALSE
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # output
#' verify_taxa(taxa = taxa_in, verified_taxa = verified_taxa_in)
#' @export
#' @importFrom assertthat assert_that is.date
#' @importFrom dplyr filter select select_ distinct mutate rename bind_rows anti_join
#'    left_join %>% pull group_by count
#' @importFrom tibble as.tibble
#' @importFrom stringr str_remove
#' @importFrom tidyselect one_of everything
#' @importFrom purrr pmap_dfr
#' @importFrom rgbif name_usage
verify_taxa <- function(taxa, verified_taxa) {
  # start checks input
  message("Reading and checking inputs...", appendLF = FALSE)
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
  
  # test verified_taxa
  name_col_verified <- c("taxonKey", "scientificName", "datasetKey", 
                         "bb_key", "bb_scientificName",
                         "bb_kingdom", "bb_rank", "bb_taxonomicStatus", 
                         "bb_acceptedKey", "bb_acceptedName",
                         "bb_acceptedKingdom", "bb_acceptedRank",
                         "bb_acceptedTaxonomicStatus",
                         "verificationKey", "remarks",
                         "dateAdded", "outdated"
  )
  assert_that(is.data.frame(verified_taxa))
  assert_that(all(name_col_verified %in% names(verified_taxa)))
  is.character(c(verified_taxa$scientificName,
                 verified_taxa$datasetKey,
                 verified_taxa$bb_scientificName,
                 verified_taxa$bb_kingdom,
                 verified_taxa$bb_rank,
                 verified_taxa$bb_taxonomicStatus,
                 verified_taxa$bb_acceptedName,
                 verified_taxa$bb_acceptedKingdom,
                 verified_taxa$bb_acceptedRank,
                 verified_taxa$bb_acceptedTaxonomicStatus
  ))
  is.numeric(c(verified_taxa$taxonKey, 
               verified_taxa$bb_key, 
               verified_taxa$bb_acceptedKey
  ))
  is.date(verified_taxa$dateAdded)
  is.logical(verified_taxa$outdated)
  # allowe multiple comma separated verification keys (character)
  class(verified_taxa$verificationKey) <- "character"
  # alow remarks (remarks col empty means for R a column logicals)
  class(verified_taxa$remarks) <- "character"
  
  # check for integrity synonym relations
  assert_that(
    verified_taxa %>% 
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
  
  # check that only synonyms and unmatched taxa are present in verified_taxa
  taxonomic_status <- 
    verified_taxa %>%
    distinct(bb_taxonomicStatus) %>%
    filter(!is.na(bb_taxonomicStatus)) %>%
    pull()
  not_allowed_taxonomicStatus <- c("ACCEPTED", "DOUBTFUL")
  assert_that(all(!taxonomic_status %in% not_allowed_taxonomicStatus),
              msg = "Only synonyms and unmatched taxa allowed in verified_taxa.")
  # End of checks
  message("DONE.", appendLF = TRUE)
  
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
    anti_join(verified_taxa,
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
    verified_taxa %>%
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
    verified_taxa %>%
    filter(!is.na(bb_scientificName)) %>%
    left_join(taxa, 
              by = c("taxonKey", "bb_key", "bb_acceptedKey")) %>%
    rename("bb_scientificName" = "bb_scientificName.x",
           "updated_bb_scientificName" = "bb_scientificName.y") %>% 
    filter(bb_scientificName != updated_bb_scientificName) %>% 
    select(which(colnames(verified_taxa)%in% colnames(.)), 
           updated_bb_scientificName,
           ends_with(".x")) %>%
    rename_at(vars(ends_with(".x")), funs(str_remove(., "\\.x"))) %>%
    as.tibble()
  
  # version for info
  updated_bb_scientificName_short <- 
    updated_bb_scientificName %>%
    select(taxonKey, bb_key, bb_acceptedKey, 
           bb_scientificName, updated_bb_scientificName)
  
  # update bb_scientificName of verified_taxa
  verified_taxa <- 
    verified_taxa %>% 
    anti_join(updated_bb_scientificName,
              by = colnames(verified_taxa)) %>%
    bind_rows(updated_bb_scientificName %>%
                mutate(bb_scientificName = updated_bb_scientificName) %>%
                select(-updated_bb_scientificName))
  message("DONE.", appendLF = TRUE)
  
  # create df of updated acceptedName
  message("Update backbone accpeted names...", appendLF = FALSE)
  updated_bb_acceptedName <- 
    verified_taxa %>%
    filter(!is.na(bb_acceptedName)) %>%
    left_join(taxa, 
              by = c("taxonKey", "bb_key", "bb_acceptedKey")) %>%
    rename("bb_acceptedName" = "bb_acceptedName.x",
           "updated_bb_acceptedName" = "bb_acceptedName.y") %>% 
    filter(bb_acceptedName != updated_bb_acceptedName) %>% 
    select(which(colnames(verified_taxa_in)%in% colnames(.)), 
           updated_bb_acceptedName,
           ends_with(".x")) %>%
    rename_at(vars(ends_with(".x")), funs(str_remove(., "\\.x"))) %>%
    as.tibble()
  
  # version for info
  updated_bb_acceptedName_short <- 
    updated_bb_acceptedName %>%
    select(taxonKey, bb_key, bb_acceptedKey, 
           bb_acceptedName, updated_bb_acceptedName) 

  # update bb_acceptedName of verified_taxa
  verified_taxa <- 
    verified_taxa %>% 
    anti_join(updated_bb_acceptedName,
              by = colnames(verified_taxa)) %>%
    bind_rows(updated_bb_acceptedName %>%
                mutate(bb_acceptedName = updated_bb_acceptedName) %>%
                select(-updated_bb_acceptedName))
  message("DONE.", appendLF = TRUE)
  
  # add new synonyms to verified_taxa
  verified_taxa <- 
    verified_taxa %>% 
    bind_rows(new_synonyms)
  
  # add new unmatches to verified_taxa
  verified_taxa <- 
    verified_taxa %>% 
    bind_rows(new_unmatched_taxa)
  
  # retrieve backbone information about taxa the synonyms point to
  message("Retrieve backbone info about taxa the synonyms point to...",
          appendLF = FALSE)
  accepted_keys <- 
    verified_taxa %>%
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
  
  # Update backbone info about accepted taxa in verified_taxa
  verified_taxa <- 
    verified_taxa %>%
    select(-c(bb_acceptedKingdom, 
              bb_acceptedRank, 
              bb_acceptedTaxonomicStatus)) %>%
    left_join(accepted_info, by = "bb_acceptedKey") %>%
    select(name_col_verified)
  message("DONE.", appendLF = TRUE)
  
  # outdated taxa: set outdated = TRUE and add 'Outdated taxa.' to remarks.
  message("Detect outdated data...", appendLF = FALSE)
  outdated_taxa <- 
    verified_taxa %>% 
    anti_join(taxa, by = c("taxonKey", "bb_key", "bb_acceptedKey")) %>%
    mutate(outdated = TRUE) %>%
    mutate(remarks = paste(remarks, "Outdated taxa."))
  
  verified_taxa <- verified_taxa %>%
    anti_join(outdated_taxa, by = c("taxonKey", "bb_key", "bb_acceptedKey")) %>%
    bind_rows(outdated_taxa)
  message("DONE.", appendLF = TRUE)
  
  # check verificationKey values against GBIF and GBIF Backbone
  message("Check verification keys...", appendLF = FALSE)
  verification_keys <- verified_taxa %>% 
    filter(!is.na(verificationKey))  %>%
    pull(verificationKey)
  verification_keys <- paste(verification_keys, collapse = ",")
  verification_keys <- unlist(stringr::str_split(verification_keys, ","))
  check_verificationKey <- gbif_verify_keys(verification_keys)
  message("DONE.", appendLF = TRUE)
  
  # find duplicates of checklist scientific names (scientificName)
  message("Find scientific names used in multiple taxa...", appendLF = FALSE)
  duplicates_taxa <- 
    verified_taxa %>%
    select(scientificName, datasetKey) %>%
    group_by(scientificName) %>%
    count() %>%
    filter(n > 0)
  message("DONE.", appendLF = TRUE)
  
  # add not outdated taxa from verified_taxa to not_to_verify_taxa
  taxa <- 
    verified_taxa %>%
    filter(isFALSE(outdated)) %>%
    select_(name_col_taxa, "verificationKey") %>%
    bind_rows(not_to_verify_taxa)
  
  return(list(taxa = taxa,
              verified_taxa = verified_taxa,
              info = list(
                new_synonyms = new_synonyms,
                new_unmatched_taxa = new_unmatched_taxa,
                outdated_taxa = outdated_taxa,
                updated_bb_scientificName = updated_bb_scientificName_short,
                updated_bb_acceptedName = updated_bb_acceptedName_short,
                duplicates_taxa = duplicates_taxa,
                check_verificationKey = check_verificationKey
              )
  ))
}
