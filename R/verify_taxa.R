#' Check and update verified taxa
#'
#' This function checks all verified taxa, add new synonyms or taxa not matched
#' to GBIF backbone in order to be evaluated by an expert, update taxa names in
#' case they have been changed and report the changes.
#' @param taxa a dataframe with at least the following columns: \itemize{
#'   \item{scientificName} {: scientific name as provided by GBIF}
#'   \item{datasetKey} {: dataset key (a UUID) of the checklist the
#'   taxon comes from.} \item{bb_key} {: a GBIF backbone key.}
#'   \item{bb_scientificName} {: scientific name as provided by GBIF
#'   backbone.} \item{bb_taxonomicStatus} {taxonomic status as provided by
#'   GBIF backbone.} \item{bb_acceptedName} {: accepted name (in case of
#'   synonyms) as provided by GBIF backbone.}  \item{bb_acceptedKey} {:
#'   accepted key as provided by GBIF backbone.} \item{bb_kingdom}
#'   \item{issues} {: issues as provided by GBIF backbone.} }
#' @param verified_taxa a dataframe with at least the following columns:
#'   \itemize{ \item{scientificName} \item{bb_scientificName}
#'   \item{bb_acceptedName} \item{bb_key}
#'   \item{bb_acceptedKey} \item{verification_key}{: to be populated
#'   manually by expert (not required by this function, but any other
#'   functionality will use this key so it is good to check its existence)}
#'   \item{bb_kingdom} \item{date_added}{: to be populated by function}
#'   \item{issues} \item{checklistst} {: checklists name where this taxa shows
#'   up}, \item{remarks} {: (optional) remarks as provided by the expert}}
#' @return a list of five dataframes: \itemize{ \item{verified_taxa}{: same
#'   dataframe as input verified_taxa, but now with updated info.}
#'   \item{new_synonyms}{: a subset of verified_taxa (same columns) with added
#'   synonym relations (found in taxa, but not in verified_taxa)}
#'   \item{unused_taxa}{: a subset of verified_taxa (same columns) with unused
#'   taxa (found in verified_taxa, but not in taxa)}
#'   \item{updated_scientificName}{: a dataframe with bb_scientificName +
#'   updated_bb_scientificName} \item{updated_acceptedName}{: a dataframe
#'   with bb_acceptedName + updated_bb_acceptedName}
#'   \item{duplicates_taxa}{: a dataframe with all taxa present in more than one
#'   checklist.} }
#' @examples
#' taxa_in <- data.frame(
#'   scientificName = c("Aspius aspius",
#'                                "Rana catesbeiana",
#'                                "Polystichum tsus-simense J.Smith",
#'                                "Apus apus (Linnaeus, 1758)",
#'                                "Begonia x semperflorens hort.",
#'                                "Rana catesbeiana",
#'                                "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley", "Atyaephyra desmaresti"),
#'   datasetKey = c("98940a79-2bf1-46e6-afd6-ba2e85a26f9f",
#'                            "e4746398-f7c4-47a1-a474-ae80a4f18e92",
#'                            "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'                            "39653f3e-8d6b-4a94-a202-859359c164c5",
#'                            "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'                            "b351a324-77c4-41c9-a909-f30f77268bc4",
#'                            "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'                            "289244ee-e1c1-49aa-b2d7-d379391ce265"),
#'   bb_scientificName = c("Aspius aspius (Linnaeus, 1758)",
#'                               "Rana catesbeiana Shaw, 1802",
#'                               "Polystichum tsus-simense (Hook.) J.Sm.",
#'                               "Apus apus (Linnaeus, 1758)",
#'                               NA,
#'                               "Rana catesbeiana Shaw, 1802",
#'                               NA,
#'                               "Atyaephyra desmarestii (Millet, 1831)"),
#'   bb_key = c(2360181, 2427092, 2651108, 5228676, NA, 2427092, NA,
#'                         4309705),
#'   bb_kingdom = c("Animalia", "Animalia", "Plantae",
#'                        "Plantae", NA, "Animalia", NA, "Animalia"),
#'   bb_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM", "ACCEPTED",
#'                                NA, "SYNONYM", NA, "HOMOTYPIC_SYNONYM"),
#'   bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)",
#'                             "Lithobates catesbeianus (Shaw, 1802)",
#'                             "Polystichum luctuosum (Kunze) Moore.",
#'                             NA, NA,
#'                             "Lithobates catesbeianus (Shaw, 1802)",
#'                             NA,
#'                             "Hippolyte desmarestii Millet, 1831"),
#'   bb_acceptedKey = c(5851603, 2427091, 4046493, NA, NA, 2427091, NA,
#'                            6454754),
#'   issues = c("ORIGINAL_NAME_DERIVED", NA, "ORIGINAL_NAME_DERIVED",
#'                       NA, NA, NA, NA, "CONFLICTING_BASIONYM_COMBINATION"),
#'   stringsAsFactors = FALSE)
#'
#' verified_taxa_in <- data.frame(
#'   scientificName = c("Rana catesbeiana",
#'                                "Polystichum tsus-simense J.Smith",
#'                                "Lemnaceae",
#'                                "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley"),
#'   bb_scientificName = c("Rana catesbeiana Shaw, 1802",
#'                               "Polystichum tsus-tsus-tsus (Hook.) Captain",
#'                               "Lemnaceae",
#'                               NA),
#'   bb_key = c(2427092, 2651108, 6723,NA),
#'   bb_kingdom = c("Animalia", "Plantae", "Plantae", NA),
#'   bb_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM", NA),
#'   bb_acceptedName = c("Lithobates dummyus (Batman, 2018)",
#'                             "Polystichum luctuosum (Kunze) Moore.",
#'                             "Araceae",
#'                             NA),
#'   bb_acceptedKey = c(2427091, 4046493, 6979, NA),
#'   issues = c(NA_character_, NA_character_, NA_character_,
#'                       NA_character_),
#'   verification_key = c(2427091,
#'                    4046493,
#'                    6979,
#'                    "2805420,2805363"),
#'   date_added = as.Date(c("2018-07-01",
#'                          "2018-07-01",
#'                          "2018-07-01",
#'                          "2018-07-16")),
#'   checklists = c("e4746398-f7c4-47a1-a474-ae80a4f18e92",
#'                  "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'                  "e4746398-f7c4-47a1-a474-ae80a4f18e92,39653f3e-8d6b-4a94-a202-859359c164c5",
#'                  "9ff7d317-609b-4c08-bd86-3bc404b77c42"),
#'   remarks = c("dummy example 1: bb_acceptedName and checklists should be updated",
#'               "dummy example 2: bb_scientificName and issues should be updated",
#'               "dummy example 3: add 'Unused taxa.' at the end of remarks.",
#'               "dummy example 4: multiple keys in verification_key are allowed."),
#'   stringsAsFactors = FALSE)
#' verify_taxa(taxa = taxa_in, verified_taxa = verified_taxa_in)
#' @export
#' @importFrom assertthat assert_that
#' @importFrom stringr str_detect
#' @importFrom tidyr separate_rows
#' @importFrom dplyr filter distinct rowwise mutate rename bind_rows anti_join
#'   select left_join full_join case_when
#' @importFrom tibble as.tibble
verify_taxa <- function(taxa, verified_taxa) {
  # test incoming arguments
  name_col_taxa <- c("scientificName", "bb_scientificName", 
                     "bb_taxonomicStatus", "bb_acceptedName",
                     "bb_key", "bb_acceptedKey",
                     "bb_kingdom", "issues",
                     "datasetKey")
  assert_that(is.data.frame(taxa))
  assert_that(all(name_col_taxa %in% names(taxa)))
  is.character(c(taxa$scientificName, 
                 taxa$bb_scientificName,
                 taxa$bb_taxonomicStatus,
                 taxa$bb_acceptedName,
                 taxa$bb_kingdom,
                 taxa$datasetKey))
  is.numeric(c(taxa$bb_key, taxa$bb_acceptedKey))
  # in case issues contains only logical NA
  class(taxa$issues) <- "character"
  # select columns needed for verifying synonyms and unmatched taxa
  taxa <- taxa %>% select(name_col_taxa)
  
  name_col_verified <- c("scientificName", "bb_scientificName",
                         "bb_taxonomicStatus", "bb_acceptedName",
                         "bb_key", "bb_acceptedKey", 
                         "verification_key", "bb_kingdom", "date_added", 
                         "issues", "remarks", "checklists")
  assert_that(is.data.frame(verified_taxa))
  assert_that(all(name_col_verified %in% names(verified_taxa)))
  is.character(c(verified_taxa$scientificName, 
                 verified_taxa$bb_scientificName,
                 verified_taxa$bb_taxonomicStatus,
                 verified_taxa$bb_acceptedName,
                 verified_taxa$bb_kingdom,
                 verified_taxa$checklists))
  assert_that(verified_taxa %>% 
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
  is.numeric(c(taxa$bb_key, taxa$bb_acceptedKey))
  # multiple comma separated keys could be added, if not already present
  class(verified_taxa$verification_key) <- "character"
  # in case issues contains only logical NA
  class(verified_taxa$issues) <- "character"

  # find new synonyms
  new_synonyms <- taxa %>%
    filter(!is.na(bb_taxonomicStatus)) %>%
    filter(! bb_taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL")) %>%
    filter(!bb_key %in% verified_taxa$bb_key) %>% 
    rowwise() %>%
    mutate(date_added = Sys.Date(),
           checklists = datasetKey,
           verification_key = NA_character_,
           remarks = NA_character_) %>% 
    ungroup() %>%
    select(one_of(name_col_verified))
  
  # find new taxa not matched to GBIF backbone 
  new_unmatched_taxa <- taxa %>%
    filter(is.na(bb_key)) %>%
    filter(!scientificName %in% 
             verified_taxa$scientificName) %>% 
    rowwise() %>%
    mutate(date_added = Sys.Date(),
           checklists = datasetKey,
           verification_key = NA_character_,
           remarks = NA_character_) %>%
    ungroup() %>%
    select(one_of(name_col_verified))
  
  # create df of updated scientificName 
  updated_scientificName <- verified_taxa %>%
    filter(bb_key %in% taxa$bb_key) %>%
    anti_join(taxa, by = "bb_scientificName") %>%
    select(bb_scientificName, bb_key) %>%
    left_join(taxa, by = "bb_key") %>%
    rename("bb_scientificName" = "bb_scientificName.x",
           "updated_bb_scientificName" = "bb_scientificName.y") %>%
    select(bb_scientificName, updated_bb_scientificName) %>% 
    distinct() %>% 
    as.tibble()
  
  # create df of updated acceptedName
  updated_acceptedName <- verified_taxa %>%
    filter(bb_key %in% taxa$bb_key) %>%
    filter(!is.na(bb_acceptedName)) %>%
    anti_join(taxa, by = "bb_acceptedName") %>%
    select(bb_acceptedName, bb_key) %>%
    left_join(taxa, by = "bb_key") %>%
    rename("bb_acceptedName" = "bb_acceptedName.x",
           "updated_bb_acceptedName" = "bb_acceptedName.y") %>%
    select(bb_acceptedName, updated_bb_acceptedName) %>% 
    distinct() %>% 
    as.tibble()
  
  # create df of updated issues
  updated_issues <- verified_taxa %>%
    select(scientificName, issues) %>%
    anti_join(taxa %>%
                select(scientificName, issues), 
              by = c("scientificName", "issues")) %>%
    left_join(taxa %>%
                distinct(scientificName, issues), 
              by = c("scientificName")) %>%
    rename("issues" = "issues.x",
           "updated_issues" = "issues.y") %>%
    filter(updated_issues != issues | 
             (is.na(updated_issues) & !is.na(issues)) |
             (!is.na(updated_issues) & is.na(issues))) %>%
    select(scientificName, issues, updated_issues) %>% 
    distinct() %>% 
    as.tibble()
  
  #update scientificName of verified taxa
  verified_taxa <- verified_taxa %>% 
    rowwise() %>%
    mutate(bb_scientificName = ifelse(
      bb_scientificName %in% updated_scientificName$bb_scientificName,
      updated_scientificName$updated_bb_scientificName[
        which(bb_scientificName == 
                updated_scientificName$bb_scientificName)],
      bb_scientificName)) %>% 
    ungroup()
  
  # update acceptedName of verified taxa
  verified_taxa <- verified_taxa %>% 
    rowwise() %>%
    mutate(bb_acceptedName = ifelse(
      bb_acceptedName %in% updated_acceptedName$bb_acceptedName,
      updated_acceptedName$updated_bb_acceptedName[
        which(bb_acceptedName == 
                updated_acceptedName$bb_acceptedName)],
      bb_acceptedName)) %>% 
    ungroup()
  
  # update issues of verified taxa
  verified_taxa <- verified_taxa %>% 
    rowwise() %>%
    mutate(issues = ifelse(
      scientificName %in% updated_issues$scientificName,
      updated_issues$updated_issues[
        which(scientificName == 
                updated_issues$scientificName)],
      issues)) %>% 
    ungroup()
  
  # update checklists
  if (nrow(verified_taxa) > 0) {
    verified_taxa <- taxa %>% 
      anti_join(verified_taxa %>% 
                  separate_rows(checklists, sep = ","), 
                by = c(intersect(colnames(taxa), colnames(verified_taxa)),
                       "datasetKey" = "checklists")) %>%
      filter(scientificName %in% 
               verified_taxa$scientificName) %>%
      full_join(verified_taxa, 
                by = intersect(colnames(taxa), colnames(verified_taxa))) %>%
      rowwise() %>%
      mutate(checklists = case_when(
        !is.na(datasetKey) ~ paste(checklists, 
                                             datasetKey, sep = ","),
        is.na(datasetKey) ~ checklists)) %>%
      ungroup() %>%
      select(-datasetKey)
  }
  
  # add bb information for better understanding
  updated_issues <- updated_issues %>%
    left_join(verified_taxa %>% 
                select(scientificName, bb_key, bb_scientificName), 
              by = "scientificName") %>%
    distinct()
  
  # add new synonyms to verified taxa
  verified_taxa <- verified_taxa %>% bind_rows(new_synonyms)
  
  # add new unmatches to verified taxa
  verified_taxa <- verified_taxa %>% bind_rows(new_unmatched_taxa)
  
  # unused taxa
  unused_taxa <- verified_taxa %>% 
    filter(!scientificName %in% taxa$scientificName)
  unused_taxa <- unused_taxa %>%
    mutate(remarks = ifelse(! "remarks" %in% colnames(unused_taxa) | 
                              remarks == "" | is.na(remarks) | is.null(remarks),
                            "Unused taxa.",
                            str_c(remarks, " Unused taxa.")))
  
  # Update remarks of unused taxa in verified_taxa
  verified_taxa <- bind_rows(verified_taxa %>% 
                               filter(scientificName %in% 
                                        taxa$scientificName),
                             unused_taxa)
  
  # taxa in several checklists (duplicates)
  duplicates_taxa <- verified_taxa %>%
    filter(str_detect(checklists, pattern = ","))
  
  return(list(verified_taxa = verified_taxa,
              new_synonyms = new_synonyms,
              new_unmatched_taxa = new_unmatched_taxa,
              unused_taxa = unused_taxa,
              updated_scientificName = updated_scientificName,
              updated_acceptedName = updated_acceptedName,
              updated_issues = updated_issues,
              duplicates_taxa = duplicates_taxa
  ))
}
