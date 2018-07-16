#' Check and update verified taxa
#'
#' This function checks all verified taxa, add new synonyms or taxa not matched
#' to GBIF backbone in order to be evaluated by an expert, update taxa names in
#' case they have been changed and report the changes.
#' @param taxa: a dataframe with at least the following columns: \itemize{
#'   \item{checklist_scientificName} {: scientific name as provided by GBIF}
#'   \item{checklist_datasetKey} {: dataset key (a UUID) of the checklist the
#'   taxon comes from.} \item{backbone_taxonKey} {: a GBIF backbone key.}
#'   \item{backbone_scientificName} {: scientific name as provided by GBIF
#'   backbone.} \item{backbone_taxonomicStatus} {taxonomic status as provided by
#'   GBIF backbone.} \item{backbone_acceptedName} {: accepted name (in case of
#'   synonyms) as provided by GBIF backbone.}  \item{backbone_acceptedKey} {:
#'   accepted key as provided by GBIF backbone.} \item{backbone_kingdom}
#'   \item{backbone_issues} {: issues as provided by GBIF backbone.} }
#' @param verified_taxa: a dataframe with at least the following columns:
#'   \itemize{ \item{checklist_scientificName} \item{backbone_scientificName}
#'   \item{backbone_acceptedName} \item{backbone_taxonKey}
#'   \item{backbone_acceptedKey} \item{verified_key}{: to be populated manually
#'   by expert (not required by this function, but any other functionality will
#'   use this key so it is good to check its existence)} \item{backbone_kingdom}
#'   \item{date_added}{: to be populated by function} \item{issues}
#'   \item{checklistst} {: checklists name where this taxa shows up},
#'   \item{remarks} {: (optional) remarks as provided by the expert}}
#' @return a list of five dataframes: \itemize{ \item{verified_taxa}{: same
#'   dataframe as input verified_taxa, but now with updated info.}
#'   \item{new_synonyms}{: a subset of verified_taxa (same columns) with added
#'   synonym relations (found in taxa, but not in verified_taxa)}
#'   \item{unused_synonyms}{: a subset of verified_taxa (same columns) with
#'   unused synonym relations (found in verified_taxa, but not in taxa)}
#'   \item{updated_scientificName}{: a dataframe with backbone_scientificName +
#'   updated_backbone_scientificName} \item{updated_acceptedName}{: a dataframe
#'   with backbone_acceptedName + updated_backbone_acceptedName}
#'   \item{duplicates_taxa}{: a dataframe with all taxa present in more than one
#'   checklist.} }
#' @examples
#' taxa_in <- data.frame(
#'   checklist_scientificName = c("Aspius aspius",
#'                                "Rana catesbeiana",
#'                                "Polystichum tsus-simense J.Smith",
#'                                "Apus apus (Linnaeus, 1758)",
#'                                "Begonia x semperflorens hort.",
#'                                "Rana catesbeiana"),
#'   checklist_datasetKey = c("98940a79-2bf1-46e6-afd6-ba2e85a26f9f",
#'                            "e4746398-f7c4-47a1-a474-ae80a4f18e92",
#'                            "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'                            "39653f3e-8d6b-4a94-a202-859359c164c5",
#'                            "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'                            "b351a324-77c4-41c9-a909-f30f77268bc4"),
#'   backbone_taxonKey = c(2360181, 2427092, 2651108, 5228676, NA, 2427092),
#'   backbone_scientificName = c("Aspius aspius (Linnaeus, 1758)",
#'                               "Rana catesbeiana Shaw, 1802",
#'                               "Polystichum tsus-simense (Hook.) J.Sm.",
#'                               "Apus apus (Linnaeus, 1758)",
#'                               NA,
#'                               "Rana catesbeiana Shaw, 1802"),
#'   backbone_kingdom = c("Animalia", "Animalia", "Plantae", 
#'                        "Plantae", NA, "Animalia"),
#'   backbone_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM",
#'                                "ACCEPTED", NA, "SYNONYM"),
#'   backbone_acceptedKey = c(5851603, 2427091, 4046493, NA, NA, 2427091),
#'   backbone_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)",
#'                             "Lithobates catesbeianus (Shaw, 1802)",
#'                             "Polystichum luctuosum (Kunze) Moore.",
#'                             NA, NA,
#'                             "Lithobates catesbeianus (Shaw, 1802)"),
#'   backbone_issues = c("ORIGINAL_NAME_DERIVED", NA,
#'                       "ORIGINAL_NAME_DERIVED", NA, NA, NA),
#'   stringsAsFactors = FALSE)
#' 
#' verified_taxa_in <- data.frame(
#'   checklist_scientificName = c("Rana catesbeiana",
#'                                "Polystichum tsus-simense J.Smith",
#'                                "Lemnaceae"),
#'   backbone_scientificName = c("Rana catesbeiana Shaw, 1802",
#'                               "Polystichum tsus-tsus-tsus (Hook.) Captain",
#'                               "Lemnaceae"),
#'   backbone_taxonomicStatus = c("SYNONYM", "SYNONYM", "SYNONYM"),
#'   backbone_acceptedName = c("Lithobates dummyus (Batman, 2018)",
#'                             "Polystichum luctuosum (Kunze) Moore.",
#'                             "Araceae"),
#'   backbone_taxonKey = c(2427092,
#'                         2651108,
#'                         6723),
#'   backbone_acceptedKey = c(2427091,
#'                           4046493,
#'                           6979),
#'   verified_key = c(2427091,
#'                    4046493,
#'                    6979),
#'   backbone_kingdom = c("Animalia", "Plantae", "Plantae"),
#'   date_added = as.Date(c("2018-07-01",
#'                          "2018-07-01",
#'                          "2018-07-01")),
#'   backbone_issues = c(NA, NA, NA),
#'   checklists = c("e4746398-f7c4-47a1-a474-ae80a4f18e92",
#'                  "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'                  "e4746398-f7c4-47a1-a474-ae80a4f18e92,39653f3e-8d6b-4a94-a202-859359c164c5"),
#'   remarks = c("dummy example 1: backbone_acceptedName and checklists should be updated",
#'               "dummy example 2: backbone_scientificName and backbone_issues should be updated",
#'               "dummy example 3: nothing should be changed"),
#'   stringsAsFactors = FALSE)
#' verify_taxa(taxa = taxa_in, verified_taxa = verified_taxa_in)
#' @export
#' @importFrom assertthat assert_that
#' @importFrom stringr str_detect
#' @importFrom tidyr separate_rows
#' @importFrom dplyr filter distinct rowwise mutate rename bind_rows
#' @importFrom dplyr anti_join select left_join full_join case_when
#' @importFrom tibble as.tibble
verify_taxa <- function(taxa, verified_taxa) {
  # test incoming arguments
  name_col_taxa <- c("checklist_scientificName", "backbone_scientificName", 
                     "backbone_taxonomicStatus", "backbone_acceptedName",
                     "backbone_taxonKey", "backbone_acceptedKey",
                     "backbone_kingdom", "backbone_issues",
                     "checklist_datasetKey")
  assert_that(is.data.frame(taxa))
  assert_that(all(name_col_taxa %in% names(taxa)))
  
  name_col_verified <- c("checklist_scientificName", "backbone_scientificName",
                         "backbone_taxonomicStatus", "backbone_acceptedName",
                         "backbone_taxonKey", "backbone_acceptedKey", 
                         "verified_key", "backbone_kingdom", "date_added", 
                         "backbone_issues", "remarks", "checklists")
  assert_that(is.data.frame(verified_taxa))
  assert_that(all(name_col_verified %in% names(verified_taxa)))
  
  # convert backbone_issues from logical to character (in case only NA occur)
  class(taxa$backbone_issues) <- "character"
  class(verified_taxa$backbone_issues) <- "character"
  
  # find new synonyms
  new_synonyms <- taxa %>%
    filter(backbone_taxonomicStatus == "SYNONYM") %>%
    filter(!backbone_taxonKey %in% verified_taxa$backbone_taxonKey) %>% 
    rowwise() %>%
    mutate(date_added = Sys.Date(),
           checklists = checklist_datasetKey,
           verified_key = NA_integer_,
           remarks = NA_character_) %>% 
    ungroup() %>%
    select(one_of(name_col_verified))
  
  # find new taxa not matched to GBIF backbone 
  new_unmatched_taxa <- taxa %>%
    filter(is.na(backbone_taxonKey)) %>%
    filter(!checklist_scientificName %in% 
             verified_taxa$checklist_scientificName) %>% 
    rowwise() %>%
    mutate(date_added = Sys.Date(),
           checklists = checklist_datasetKey,
           verified_key = NA_integer_,
           remarks = NA_character_) %>%
    ungroup() %>%
    select(one_of(name_col_verified))
  
  # create df of updated scientificNames 
  updated_scientificName <- verified_taxa %>%
    filter(backbone_taxonKey %in% taxa$backbone_taxonKey) %>%
    anti_join(taxa, by = "backbone_scientificName") %>%
    select(backbone_scientificName, backbone_taxonKey) %>%
    left_join(taxa, by = "backbone_taxonKey") %>%
    rename("backbone_scientificName" = "backbone_scientificName.x",
           "updated_backbone_scientificName" = "backbone_scientificName.y") %>%
    select(backbone_scientificName, updated_backbone_scientificName) %>% 
    distinct() %>% 
    as.tibble()
  
  # create df of updated acceptedName
  updated_acceptedName <- verified_taxa %>%
    filter(backbone_taxonKey %in% taxa$backbone_taxonKey) %>%
    filter(!is.na(backbone_acceptedName)) %>%
    anti_join(taxa, by = "backbone_acceptedName") %>%
    select(backbone_acceptedName, backbone_taxonKey) %>%
    left_join(taxa, by = "backbone_taxonKey") %>%
    rename("backbone_acceptedName" = "backbone_acceptedName.x",
           "updated_backbone_acceptedName" = "backbone_acceptedName.y") %>%
    select(backbone_acceptedName, updated_backbone_acceptedName) %>% 
    distinct() %>% 
    as.tibble()
  
  # create df of updated backbone_issues
  updated_backbone_issues <- verified_taxa %>%
    filter(backbone_taxonKey %in% taxa$backbone_taxonKey) %>%
    select(backbone_taxonKey, backbone_issues) %>%
    anti_join(taxa, by = c("backbone_issues", "backbone_taxonKey")) %>%
    left_join(taxa %>%
                distinct(backbone_taxonKey, backbone_issues), 
              by = c("backbone_taxonKey")) %>%
    rename("backbone_issues" = "backbone_issues.x",
           "updated_backbone_issues" = "backbone_issues.y") %>%
    select(backbone_taxonKey, backbone_issues, updated_backbone_issues) %>% 
    distinct() %>% 
    as.tibble()
  
  #update scientificName of verified taxa
  verified_taxa <- verified_taxa %>% 
    rowwise() %>%
    mutate(backbone_scientificName = ifelse(
      backbone_scientificName %in% updated_scientificName$backbone_scientificName,
      updated_scientificName$updated_backbone_scientificName[
        which(backbone_scientificName == 
                updated_scientificName$backbone_scientificName)],
      backbone_scientificName)) %>% 
    ungroup()
  
  # update acceptedName of verified taxa
  verified_taxa <- verified_taxa %>% 
    rowwise() %>%
    mutate(backbone_acceptedName = ifelse(
      backbone_acceptedName %in% updated_acceptedName$backbone_acceptedName,
      updated_acceptedName$updated_backbone_acceptedName[
        which(backbone_acceptedName == 
                updated_acceptedName$backbone_acceptedName)],
      backbone_acceptedName)) %>% 
    ungroup()
  
  # update backbone_issues of verified taxa
  verified_taxa <- verified_taxa %>% 
    rowwise() %>%
    mutate(backbone_issues = ifelse(
      backbone_taxonKey %in% updated_backbone_issues$backbone_taxonKey,
      updated_backbone_issues$updated_backbone_issues[
        which(backbone_taxonKey == 
                updated_backbone_issues$backbone_taxonKey)],
      backbone_issues)) %>% 
    ungroup()
  
  # update checklists
  verified_taxa <- taxa %>% 
    anti_join(verified_taxa %>% 
                separate_rows(checklists, sep = ","), 
              by = c(intersect(colnames(taxa), colnames(verified_taxa)),
                     "checklist_datasetKey" = "checklists")) %>%
    filter(checklist_scientificName %in% 
             verified_taxa$checklist_scientificName) %>%
    full_join(verified_taxa, 
              by = intersect(colnames(taxa), colnames(verified_taxa))) %>%
    rowwise() %>%
    mutate(checklists = case_when(
      !is.na(checklist_datasetKey) ~ paste(checklists, 
                                           checklist_datasetKey, sep = ","),
      is.na(checklist_datasetKey) ~ checklists)) %>%
    ungroup() %>%
    select(-checklist_datasetKey)
  
  # add (eventually updated) backbone_scientificName to updated_backbone_issues
  # for readibility reasons: better to have a scientificName than just a key!
  updated_backbone_issues <- updated_backbone_issues %>%
    left_join(verified_taxa %>% 
                select(backbone_taxonKey, backbone_scientificName), 
              by = "backbone_taxonKey")
  
  # add new synonyms to verified taxa
  verified_taxa <- verified_taxa %>% bind_rows(new_synonyms)
  
  # add new unmatches to verified taxa
  verified_taxa <- verified_taxa %>% bind_rows(new_unmatched_taxa)
  
  # taxa in several checklists
  duplicates_taxa <- verified_taxa %>%
    filter(str_detect(checklists, pattern = ","))
  
  # unused synonyms
  unused_taxa <- verified_taxa %>% 
    filter(!backbone_taxonKey %in% taxa$backbone_taxonKey)
  
  return(list(verified_taxa = verified_taxa,
              new_synonyms = new_synonyms,
              new_unmatched_taxa = new_unmatched_taxa,
              unused_taxa = unused_taxa,
              updated_scientificName = updated_scientificName,
              updated_acceptedName = updated_acceptedName,
              updated_backbone_issues = updated_backbone_issues,
              duplicates_taxa = duplicates_taxa
  ))
}
