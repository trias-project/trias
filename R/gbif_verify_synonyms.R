#' Check and update verified synonyms
#'
#' This function checks all verified synonyms, add new sysnonyms in order to be 
#' evaluated by an expert, update taxa names in case they have been changed and 
#' report the changes.
#' @param taxa: a dataframe with at least the following columns:
#' \itemize{
#'  \item{backbone_taxonKey}
#'  \item{backbone_scientificName}
#'  \item{backbone_acceptedKey}
#'  \item{backbone_accepted}
#' }
#' @param verified_synonyms: a dataframe with at least the following columns:
#' \itemize{
#'  \item{backbone_taxonKey}
#'  \item{backbone_scientificName}
#'  \item{backbone_acceptedKey}
#'  \item{backbone_accepted}
#'  \item{backbone_kingdom}{: to be populated from GBIF (is not in taxa)}
#'  \item{date_added}{: to be populated by function}
#'  \item{verified_key}{: to be populated manually by expert 
#' (not required by this function, but any other functionality will use this 
#' key so it is good to check its existence)
#' }
#' }
#' @return a list of five dataframes:
#' \itemize{
#'  \item{verified_synonyms}{: same dataframe as input verified_synonyms, but now with 
#' updated info.}
#'  \item{new_synonyms}{: a subset of verified_synonyms (same columns) with added synonym 
#' relations (found in taxa, but not in verified_synonyms)}
#'  \item{unused_synonyms}{: a subset of verified_synonyms (same columns) with 
#' unused synonym relations (found in verified_synonyms, but not in taxa)}
#'  \item{updated_scientificName}{: a dataframe with 
#' backbone_scientificName + updated_backbone_scientificName}
#'  \item{updated_accepted}{: a dataframe with 
#' backbone_accepted + updated_backbone_accepted}
#' }
#' @examples
#' taxa_in <- data.frame(
#'   backbone_taxonKey = c(2360181, 2427092, 2651108),
#'   backbone_scientificName = c("Aspius aspius (Linnaeus, 1758)",
#'                               "Rana catesbeiana Shaw, 1802",
#'                               "Polystichum tsus-simense (Hook.) J.Sm."),
#'   backbone_acceptedKey = c(5851603, 2427091, 4046493),
#'   backbone_accepted = c("Leuciscus aspius (Linnaeus, 1758)",
#'                        "Lithobates catesbeianus (Shaw, 1802)",
#'                        "Polystichum luctuosum (Kunze) Moore."),
#'   stringsAsFactors = FALSE)
#' verified_synonyms_in <- data.frame(
#'   backbone_taxonKey = c(2427092,
#'                         2651108,
#'                         6723),
#'   backbone_scientificName = c("Rana catesbeiana Shaw, 1802",
#'                               "Polystichum tsus-tsus-tsus (Hook.) Captain",
#'                               "Lemnaceae"),
#'   backbone_accepted = c("Lithobates dummyus (Batman, 2018)",
#'                         "Polystichum luctuosum (Kunze) Moore.",
#'                         "Araceae"),
#'   backbone_acceptedKey = c(2427091,
#'                            4046493,
#'                            6979),
#'   backbone_kingdom = c("Animalia",
#'                        "Plantae",
#'                        "Plantae"),
#'   date_added = c("2018-01-01",
#'                  "2018-01-01",
#'                  "2018-01-01"),
#'   verified_key = c(2427091,
#'                    4046493,
#'                    6979),
#'   remarks = c("dummy example 1: backbone_accepted should be updated",
#'               "dummy example 2: backbone_scientificName should be updated",
#'               "dummy example 3: nothing should be changed");
#'   stringsAsFactors = FALSE)
#' gbif_verify_synonyms(taxa = taxa_in, verified_synonyms = verified_synonyms_in)
gbif_verify_synonyms <- function(taxa, verified_synonyms) {
  
  # test incoming arguments
  name_col_taxa <- c("backbone_taxonKey","backbone_scientificName",
                "backbone_acceptedKey","backbone_accepted")
  assert_that(is.data.frame(taxa))
  assert_that(all(name_col_taxa %in% names(taxa)))
  
  name_col_synonyms <- c("backbone_taxonKey","backbone_scientificName",
                     "backbone_acceptedKey","backbone_accepted",
                     "backbone_kingdom", "date_added", "verified_key")
  assert_that(is.data.frame(verified_synonyms))
  assert_that(all(name_col_synonyms %in% names(verified_synonyms)))
  
  # find new synonyms
  new_synonyms <- taxa %>% 
    filter(!backbone_taxonKey %in% verified_synonyms$backbone_taxonKey) %>% 
    rowwise() %>%
    mutate(backbone_kingdom = name_usage(key = backbone_taxonKey,
                                         return = "data") %>% pull(kingdom),
           date_added = Sys.Date())

  # create df of updated scientificNames 
  updated_scientificName <- verified_synonyms %>%
    filter(backbone_taxonKey %in% taxa$backbone_taxonKey) %>%
    anti_join(taxa, by = "backbone_scientificName") %>%
    select(backbone_scientificName, backbone_taxonKey) %>%
    left_join(taxa, by = "backbone_taxonKey") %>%
    rename("backbone_scientificName" = "backbone_scientificName.x",
           "updated_backbone_scientificName" = "backbone_scientificName.y") %>%
    select(backbone_scientificName, updated_backbone_scientificName)
  
  # create df of updated accepted
  updated_accepted <- verified_synonyms %>%
    filter(backbone_taxonKey %in% taxa$backbone_taxonKey) %>%
    anti_join(taxa, by = "backbone_accepted") %>%
    select(backbone_accepted, backbone_taxonKey) %>%
    left_join(taxa, by = "backbone_taxonKey") %>%
    rename("backbone_accepted" = "backbone_accepted.x",
           "updated_backbone_accepted" = "backbone_accepted.y") %>%
    select(backbone_accepted, updated_backbone_accepted)

  #update scientificName of verified synonyms
  verified_synonyms %<>% rowwise() %>%
    mutate(backbone_scientificName = ifelse(
      backbone_scientificName %in% updated_scientificName$backbone_scientificName,
      updated_scientificName$updated_backbone_scientificName[which(backbone_scientificName == updated_scientificName$backbone_scientificName)],
      backbone_scientificName))
  
  # update accepted of verified synonyms
  verified_synonyms %<>% rowwise() %>%
    mutate(backbone_accepted = ifelse(
      backbone_accepted %in% updated_accepted$backbone_accepted,
      updated_accepted$updated_backbone_accepted[which(backbone_accepted == updated_accepted$backbone_accepted)],
      backbone_accepted))

  # add new synonyms to verified synonyms
  verified_synonyms %<>% bind_rows(new_synonyms)
  
  # unused synonyms
  unused_synonyms <- verified_synonyms %>% 
    filter(!backbone_taxonKey %in% taxa$backbone_taxonKey)
  
  return(list(verified_synonyms = verified_synonyms,
              new_synonyms = new_synonyms,
              unused_synonyms = unused_synonyms,
              updated_scientificName = updated_scientificName,
              updated_accepted = updated_accepted))
}
