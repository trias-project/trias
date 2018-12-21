#' Verify taxa that the GBIF Backbone Taxonomy does not recognize or will lump
#'
#' Verify taxa that the \href{https://doi.org/10.15468/39omei}{GBIF Backbone
#' Taxonomy} does not recognize (no backbone match) or will lump under another
#' name (synonyms). This is done by adding a \code{verificationKey} to the input
#' dataframe, populated with: \itemize{ \item{For \code{ACCEPTED} and
#' \code{DOUBTFUL} taxa: the backbone taxon key for that taxon (taxon is its own
#' unit and won't be lumped).} \item{For other taxa: a manually chosen and thus
#' verified backbone taxon key. This could either be the taxon key of: \itemize{
#' \item{accepted taxon suggested by GBIF: backbone synonymy is accepted and
#' taxon will be lumped.} \item{another accepted taxon: backbone synonymy is
#' rejected, but taxon will be lumped under another name.} \item{taxon itself:
#' backbone synonymy is rejected, taxon will be considered as separate taxon.}
#' \item{other taxon/taxa: automatic backbone match failed, but taxon can be
#' considered/lumped with manually found taxon/taxa (e.g. hybrid formula
#' considered equal to its hybrid parents).} }} } The manually chosen
#' \code{verificationKey} should be provided in \code{verification}: a dataframe
#' (probably read from a file) listing all checklist taxon/backbone
#' taxon/accepted taxon combinations that require verification. The function
#' will update a provided verification based on the input taxa or create a new
#' one if none is provided. Any changes to the verification are also provided as
#' ancillary information.
#'
#' @param taxa df. Dataframe with at least the following columns for each taxon:
#'   \itemize{ \item{\code{taxonKey}: numeric. Non-backbone checklist taxon key
#'   assigned by GBIF.} \item{\code{scientificName}: character. Scientific name
#'   as interpreted by GBIF.} \item{\code{datasetKey}: character. Dataset key
#'   (UUID) assigned by GBIF of originating checklist.} \item{\code{bb_key}:
#'   numeric. Taxon key of matching backbone taxon (if any).}
#'   \item{\code{bb_scientificName}: character. Scientific name of matching
#'   backbone taxon.} \item{\code{bb_kingdom}: character. Kingdom of matching
#'   backbone taxon.} \item{\code{bb_rank}: character. Rank of matching backbone
#'   taxon.} \item{\code{bb_taxonomicStatus}: character. Taxonomic status of
#'   matching backbone taxon.} \item{\code{bb_acceptedKey}: numeric. Accepted
#'   key of taxon for which matching backbone taxon is considered a synonym.}
#'   \item{\code{bb_acceptedName}: character. Accepted name of taxon for which
#'   matching backbone taxon is considered a synonym.} }
#' @param verification df. Dataframe with at least the following columns for
#'   each checklist taxon/backbone taxon/accepted taxon combination: \itemize{
#'   \item{\code{taxonKey}: numeric. Non-backbone checklist taxon key assigned
#'   by GBIF.} \item{\code{scientificName}: character. Scientific name as
#'   interpreted by GBIF.} \item{\code{datasetKey}: character. Dataset key
#'   (UUID) assigned by GBIF of originating checklist.} \item{\code{bb_key}:
#'   numeric. Taxon key of matching backbone taxon (if any).}
#'   \item{\code{bb_scientificName}: character. Scientific name of matching
#'   backbone taxon.} \item{\code{bb_kingdom}: character. Kingdom of matching
#'   backbone taxon.} \item{\code{bb_rank}: character. Rank of matching backbone
#'   taxon.} \item{\code{bb_taxonomicStatus}: character. Taxonomic status of
#'   matching backbone taxon.} \item{\code{bb_acceptedKey}: numeric. Taxon key
#'   of accepted backbone taxon in case matching backbone taxon is considered a
#'   synonym.} \item{\code{bb_acceptedName}: character. Scientific name of
#'   accepted backbone taxon in case matching backbone taxon is considered a
#'   synonym.} \item{\code{bb_acceptedKingdom}: character. Kingdom of accepted
#'   taxon. Expected to be equal to \code{bb_kingdom}.}
#'   \item{\code{bb_acceptedRank}: character. Rank of accepted taxon.}
#'   \item{\code{bb_acceptedTaxonomicStatus}: character. Taxonomic status of
#'   accepted taxon. Expected to be \code{ACCEPTED}.}
#'   \item{\code{verificationKey}: character. Taxon key(s) of backbone taxon
#'   manually set by expert.} \item{\code{remarks}: character. Remarks provided
#'   by the expert.} \item{\code{verifiedBy}: character. Name of the person who
#'   assigned \code{verificationKey}.} \item{\code{dateAdded}: date. Date on
#'   which new
#'   combinations were added.} \item{\code{outdated}: logical. \code{TRUE} when
#'   combination was not used for input taxa.} }
#'
#' @return list. List with three objects: \itemize{ \item{\code{taxa}: df.
#'   Provided dataframe with additional column \code{verificationKey}.}
#'   \item{\code{verification}: df. New or updated dataframe with verification
#'   information.} \item{\code{info}: list. Dataframes with ancillary
#'   information regarding changes to the verification. \itemize{
#'   \item{\code{new_synonyms}: df. Subset of \code{verification} with synonym
#'   taxa found in \code{taxa} but not in provided \code{verification}).}
#'   \item{\code{new_unmatched_taxa}: df. Subset of \code{verification} with
#'   unmatched taxa found in \code{taxa} but not in provided
#'   \code{verification}).} \item{\code{outdated_synonyms}: df. Subset of
#'   \code{verification} with synonyms found in provided \code{verification} but not
#'   in \code{taxa}.} \item{\code{outdated_unmatched_taxa}: df. Subset of
#'   \code{verification} with unmatched taxa found in provided \code{verification} but not
#'   in \code{taxa}.} \item{\code{updated_bb_scientificName}: df.
#'   \code{bb_scientificName}s in provided \code{verification} that were updated
#'   \code{updated_bb_scientificName} in the backbone since.}
#'   \item{\code{updated_bb_acceptedName}: df. \code{bb_acceptedName}s in
#'   provided \code{verification} that were updated
#'   \code{updated_bb_acceptedName} in the backbone since.}
#'   \item{\code{duplicates}: df. Taxa present in more than one checklist.}
#'   \item{\code{check_verificationKey}: df. Check if provided
#'   \code{verificationKey}s can be found in backbone.} }} }
#'
#' @export
#' @importFrom assertthat assert_that is.date
#' @importFrom dplyr filter filter_at select distinct mutate rename rename_at
#'   arrange bind_rows inner_join anti_join left_join right_join %>% pull
#'   as_tibble group_by count starts_with all_vars any_vars
#' @importFrom stringr str_remove str_split
#' @importFrom tidyselect one_of everything ends_with
#' @importFrom tibble tibble
#' @importFrom purrr pmap_dfr map2_chr
#' @importFrom rgbif name_usage
#'
#' @examples
#' my_taxa <- data.frame(
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
#'     114445583,
#'     141264849,
#'     101790530
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
#'     "Rana blanfordii Boulenger",
#'     "Pterocarya x rhederiana C.K. Schneider",
#'     "Stenelmis williami Schmude"
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
#'     "3772da2f-daa1-4f07-a438-15a881a2142c",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "9ca92552-f23a-41a8-a140-01abaa31c931"
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
#'     2430304,
#'     NA,
#'     1033588
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
#'     "Rana blanfordii Boulenger, 1882",
#'     NA,
#'     "Stenelmis williami Schmude"
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
#'     "Animalia",
#'     NA,
#'     "Animalia"
#'   ),
#'   bb_rank = c(
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     NA,
#'     "SPECIES",
#'     NA,
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     NA,
#'     "SPECIES"
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
#'     "SYNONYM",
#'     NA,
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
#'     "Nanorana blanfordii (Boulenger, 1882)",
#'     NA,
#'     "Stenelmis Dufour, 1835"
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
#'     2430301,
#'     NA,
#'     1033553
#'   ),
#'   taxonID = c(
#'     "alien-fishes-checklist:taxon:c937610f85ea8a74f105724c8f198049",
#'     "88",
#'     "alien-plants-belgium:taxon:57c1d111f14fd5f3271b0da53c05c745",
#'     "4512",
#'     "alien-plants-belgium:taxon:9a6c5ed8907ff169433fe44fcbff0705",
#'     "80-syn",
#'     "alien-plants-belgium:taxon:29409d1e1adc88d6357dd0be13350d6c",
#'     "alien-macroinvertebrates-checklist:taxon:54cca150e1e0b7c0b3f5b152ae64d62b",
#'     "alien-macroinvertebrates-checklist:taxon:73f271d93128a4e566e841ea6e3abff0",
#'     "rinse-checklist:taxon:7afe7b1fbdd06cbdfe97272567825c09",
#'     "ad-hoc-checklist:taxon:32dc2e18733fffa92ba4e1b35d03c4e2",
#'     "a80caa33-da9d-48ed-80e3-f76b0b3810f9",
#'     "alien-plants-belgium:taxon:56d6564f59d9092401c454849213366f",
#'     "193729"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' my_verification <- data.frame(
#'   taxonKey = c(
#'     113794952,
#'     141264857,
#'     143920280,
#'     141264835,
#'     141264614,
#'     140562956,
#'     145953989,
#'     114445583,
#'     128897752,
#'     101790530,
#'     141265523
#'   ),
#'   scientificName = c(
#'     "Rana catesbeiana",
#'     "Polystichum tsus-simense J.Smith",
#'     "Lemnaceae",
#'     "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley",
#'     "Begonia x semperflorens hort.",
#'     "Ferrissia fragilis",
#'     "Ferrissia fragilis",
#'     "Rana blanfordii Boulenger",
#'     "Python reticulatus Fitzinger, 1826",
#'     "Stenelmis williami Schmude",
#'     "Veronica austriaca Jacq."
#'   ),
#'   datasetKey = c(
#'     "e4746398-f7c4-47a1-a474-ae80a4f18e92",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "e4746398-f7c4-47a1-a474-ae80a4f18e92",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42",
#'     "289244ee-e1c1-49aa-b2d7-d379391ce265",
#'     "3f5e930b-52a5-461d-87ec-26ecd66f14a3",
#'     "3772da2f-daa1-4f07-a438-15a881a2142c",
#'     "7ddf754f-d193-4cc9-b351-99906754a03b",
#'     "9ca92552-f23a-41a8-a140-01abaa31c931",
#'     "9ff7d317-609b-4c08-bd86-3bc404b77c42"
#'   ),
#'   bb_key = c(
#'     2427092,
#'     2651108,
#'     6723,
#'     NA,
#'     NA,
#'     2291152,
#'     2291152,
#'     2430304,
#'     7587934,
#'     1033588,
#'     NA
#'   ),
#'   bb_scientificName = c(
#'     "Rana catesbeiana Shaw, 1802",
#'     "Polystichum tsus-tsus-tsus (Hook.) Captain",
#'     "Lemnaceae",
#'     NA,
#'     NA,
#'     "Ferrissia fragilis (Tryon, 1863)",
#'     "Ferrissia fragilis (Tryon, 1863)",
#'     "Rana blanfordii Boulenger, 1882",
#'     "Python reticulatus Fitzinger, 1826",
#'     "Stenelmis williami Schmude",
#'     NA
#'   ),
#'   bb_kingdom = c(
#'     "Animalia",
#'     "Plantae",
#'     "Plantae",
#'     NA,
#'     NA,
#'     "Animalia",
#'     "Animalia",
#'     "Animalia",
#'     "Animalia",
#'     "Animalia",
#'     NA
#'   ),
#'   bb_rank = c(
#'     "SPECIES",
#'     "SPECIES",
#'     "FAMILY",
#'     NA,
#'     NA,
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     NA
#'   ),
#'   bb_taxonomicStatus = c(
#'     "SYNONYM",
#'     "SYNONYM",
#'     "SYNONYM",
#'     NA,
#'     NA,
#'     "SYNONYM",
#'     "SYNONYM",
#'     "SYNONYM",
#'     "SYNONYM",
#'     "SYNONYM",
#'     NA
#'   ),
#'   bb_acceptedName = c(
#'     "Lithobates dummyus (Batman, 2018)",
#'     "Polystichum luctuosum (Kunze) Moore.",
#'     "Araceae",
#'     NA,
#'     NA,
#'     "Ferrissia californica (Rowell, 1863)",
#'     "Ferrissia californica (Rowell, 1863)",
#'     "Hylarana chalconota (Schlegel, 1837)",
#'     "Malayopython reticulatus (Schneider, 1801)",
#'     "Stenelmis Dufour, 1835",
#'     NA
#'   ),
#'   bb_acceptedKey = c(
#'     2427091,
#'     4046493,
#'     6979,
#'     NA,
#'     NA,
#'     9520065,
#'     9520065,
#'     2427008,
#'     9260388,
#'     1033553,
#'     NA
#'   ),
#'   bb_acceptedKingdom = c(
#'     "Animalia",
#'     "Plantae",
#'     "Plantae",
#'     NA,
#'     NA,
#'     "Animalia",
#'     "Animalia",
#'     "Animalia",
#'     "Animalia",
#'     "Animalia",
#'     NA
#'   ),
#'   bb_acceptedRank = c(
#'     "SPECIES",
#'     "SPECIES",
#'     "FAMILY",
#'     NA,
#'     NA,
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     "SPECIES",
#'     "GENUS",
#'     NA
#'   ),
#'   bb_acceptedTaxonomicStatus = c(
#'     "ACCEPTED",
#'     "ACCEPTED",
#'     "ACCEPTED",
#'     NA,
#'     NA,
#'     "ACCEPTED",
#'     "ACCEPTED",
#'     "ACCEPTED",
#'     "ACCEPTED",
#'     "ACCEPTED",
#'     NA
#'   ),
#'   verificationKey = c(
#'     2427091,
#'     4046493,
#'     6979,
#'     "2805420,2805363",
#'     NA,
#'     NA,
#'     NA,
#'     NA,
#'     9260388,
#'     NA,
#'     3172099
#'   ),
#'   remarks = c(
#'     "dummy example 1: bb_acceptedName should be updated.",
#'     "dummy example 2: bb_scientificName should be updated.",
#'     "dummy example 3: not used anymore. Set outdated = TRUE. Add 'Outdated
#'     taxa.' to remarks.",
#'     "dummy example 4: multiple keys in verificationKey are allowed.",
#'     "dummy example 5: nothing should happen.",
#'     "dummy example 6: datasetKey should not be modified. If new taxa come in
#'     with same name from other checklsits, they should be added as new rows.
#'     Report them as duplicates in duplicates_taxa",
#'     "dummy example 7: datasetKey should not be modified. If new taxa come in
#'     with same name from other checklsits, they should be added as new rows.
#'     Report them as duplicates in duplicates_taxa",
#'     "dummy example 8: outdated synonym. Set outdated = TRUE. Add 'Outdated
#'     taxa.' to remarks.",
#'     "dummy example 9: 'Outdated taxa'. outdated is already TRUE. Label
#'     'Outdated taxa' already in remarks. No actions.",
#'     "dummy example 10: 'Outdated taxa'. Not outdated anymore. Change outdated
#'     back to FALSE. Remove label from remarks.",
#'     "dummy example 11: outdated unmatched taxa. Set outdated = TRUE. Add
#'     'Outdated taxa' to remarks."
#'   ),
#'   verifiedBy = c(
#'     "Damiano Oldoni",
#'     "Peter Desmet",
#'     "Stijn Van Hoey",
#'     "Tanja Milotic",
#'     NA,
#'     NA,
#'     NA,
#'     NA,
#'     "Lien Reyserhove",
#'     NA,
#'     "Dimitri Brosens"
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
#'       "2018-11-29",
#'       "2018-12-01",
#'       "2018-12-02",
#'       "2018-12-03"
#'     )
#'   ),
#'   outdated = c(
#'     FALSE,
#'     FALSE,
#'     FALSE,
#'     FALSE,
#'     FALSE,
#'     FALSE,
#'     FALSE,
#'     FALSE,
#'     TRUE,
#'     TRUE,
#'     FALSE
#'   ),
#'   stringsAsFactors = FALSE
#'   )
#'
#' # output
#' verify_taxa(taxa = my_taxa, verification = my_verification)
#' verify_taxa(taxa = my_taxa)
verify_taxa <- function(taxa, verification = NULL) {
  # start checks input
  message("Check input dataframes...", appendLF = FALSE)
  # test taxa
  name_col_taxa <- c(
    "taxonKey", "scientificName", "datasetKey",
    "bb_key", "bb_scientificName", "bb_kingdom",
    "bb_rank", "bb_taxonomicStatus",
    "bb_acceptedKey", "bb_acceptedName"
  )
  assertthat::assert_that(is.data.frame(taxa))
  assertthat::assert_that(all(name_col_taxa %in% names(taxa)))
  is.character(c(
    taxa$scientificName,
    taxa$datasetKey,
    taxa$bb_scientificName,
    taxa$bb_kingdom,
    taxa$bb_rank,
    taxa$bb_taxonomicStatus,
    taxa$bb_acceptedName
  ))
  is.numeric(c(
    taxa$taxonKey,
    taxa$bb_key,
    taxa$bb_acceptedKey
  ))

  # check that accepted or doubtful taxa have a backbone key
  assert_that(
    taxa %>%
      filter(bb_taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL") &
        is.na(bb_key)) %>%
      nrow() == 0,
    msg = "Taxa which don't need verification must have a backbone key."
  )

  # unmatched taxa should have no GBIF Backbone information at all
  assert_that(
    taxa %>%
      filter(is.na(bb_key)) %>%
      filter_at(vars(starts_with("bb_")), all_vars(is.na(.))) %>%
      nrow() ==
      taxa %>%
        filter(is.na(bb_key)) %>%
        filter_at(vars(starts_with("bb_")), any_vars(is.na(.))) %>%
        nrow(),
    msg = "Columns with GBIF Backbone info should be empty for unmatched taxa."
  )

  # throw a message if a column called verificationKey already exists
  if ("verificationKey" %in% names(taxa)) {
    message(paste(
      "\nColumn verificationKey already exists. It will be overwritten."
    ),
    appendLF = TRUE
    )
    taxa <-
      taxa %>%
      select(-verificationKey)
  }

  # test verification
  name_col_verification <- c(
    "taxonKey", "scientificName", "datasetKey",
    "bb_key", "bb_scientificName",
    "bb_kingdom", "bb_rank", "bb_taxonomicStatus",
    "bb_acceptedKey", "bb_acceptedName",
    "bb_acceptedKingdom", "bb_acceptedRank",
    "bb_acceptedTaxonomicStatus",
    "verificationKey", "remarks",
    "verifiedBy","dateAdded", "outdated"
  )
  # make empty tibble df if not exist
  if (is.null(verification)) {
    verification <- tibble(
      taxonKey = double(),
      scientificName = character(),
      datasetKey = character(),
      bb_key = double(),
      bb_scientificName = character(),
      bb_kingdom = character(),
      bb_rank = character(),
      bb_taxonomicStatus = character(),
      bb_acceptedKey = double(),
      bb_acceptedName = character(),
      bb_acceptedKingdom = character(),
      bb_acceptedRank = character(),
      bb_acceptedTaxonomicStatus = character(),
      verificationKey = character(),
      remarks = character(),
      verifiedBy = character(),
      dateAdded = numeric(),
      outdated = logical()
    )
    class(verification$dateAdded) <- "Date"
  }
  assert_that(is.data.frame(verification))
  assert_that(all(name_col_verification %in% names(verification)))
  is.character(c(
    verification$scientificName,
    verification$datasetKey,
    verification$bb_scientificName,
    verification$bb_kingdom,
    verification$bb_rank,
    verification$bb_taxonomicStatus,
    verification$bb_acceptedName,
    verification$bb_acceptedKingdom,
    verification$bb_acceptedRank,
    verification$bb_acceptedTaxonomicStatus,
    verification$verificationKey,
    verification$remarks,
    verification$verifiedBy
  ))
  is.numeric(c(
    verification$taxonKey,
    verification$bb_key,
    verification$bb_acceptedKey
  ))
  is.date(verification$dateAdded)
  is.logical(verification$outdated)
  assert_that(
    all(nchar(verification$datasetKey) == 36) &
      isFALSE(any(grepl(pattern = ",", x = verification$datasetKey))),
    msg = paste(
      "Incorrect datesetKey:", verification$datasetKey,
      "Is expected to be 36-character UUID."
    )
  )
  assert_that(verification %>%
    filter(is.na(outdated)) %>%
    nrow() == 0,
  msg = "Only logicals (TRUE/FALSE) allowed in 'outdated' of verification."
  )
  # allow multiple comma separated verification keys (character)
  class(verification$verificationKey) <- "character"
  # allow remarks (remarks col empty means for R a column logicals)
  class(verification$remarks) <- "character"

  # check for integrity synonym relations
  assert_that(
    verification %>%
      filter((is.na(bb_acceptedName) &
        !is.na(bb_acceptedKey)
      ) |
        (!is.na(bb_acceptedName) &
          is.na(bb_acceptedKey)
        )) %>%
      nrow() == 0,
    msg = paste(
      "bb_acceptedName and bb_acceptedKey",
      "should be both NA or both present."
    )
  )

  # check that only synonyms and unmatched taxa are present in verification
  taxonomic_status <-
    verification %>%
    distinct(bb_taxonomicStatus) %>%
    filter(!is.na(bb_taxonomicStatus)) %>%
    pull()
  not_allowed_taxonomicStatus <- c("ACCEPTED", "DOUBTFUL")
  assert_that(all(!taxonomic_status %in% not_allowed_taxonomicStatus),
    msg = "Only synonyms and unmatched taxa allowed in verification."
  )
  
  verifiedBy_anomalies <- 
    verification %>%
    filter(is.na(verificationKey) & !is.na(verifiedBy))
  if (nrow(verifiedBy_anomalies) > 0) {
    warning(
      paste("verifiedBy must be empty if no verificationKey is present.",
            "Taxa with suspect verifiedBy values will be removed.",
            paste(map2_chr(verifiedBy_anomalies$taxonKey, 
                     verifiedBy_anomalies$verifiedBy, 
                     ~paste(.x,.y, sep = ": ")), collapse = "\n"),
            sep = "\n"
      )
    )
    # get order taxa in verification
    ordered_taxon_keys_verification <-
      verification %>%
      select(taxonKey)
    
    verifiedBy_anomalies <- 
      verifiedBy_anomalies %>%
      mutate(verifiedBy = NA_character_)
    verification <-
      verification %>%
      anti_join(verifiedBy_anomalies, by = names(verification)) %>%
      bind_rows(verifiedBy_anomalies) %>%
      right_join(ordered_taxon_keys_verification,
                 by = "taxonKey")
  }
  message("DONE.", appendLF = TRUE)
  
  # get order taxon keys
  ordered_taxon_keys <-
    taxa %>%
    select(taxonKey)

  # find taxa which don't need any verification and assign verificationKey
  message("Assign verificationKey to taxa which don't need verification...",
    appendLF = FALSE
  )
  not_to_verify_taxa <-
    taxa %>%
    filter(!is.na(bb_key) &
      bb_taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL")) %>%
    mutate(
      verificationKey = as.character(bb_key)
    )
  message("DONE.", appendLF = TRUE)

  # go further with taxa which need verification
  taxa_input <- taxa
  taxa <-
    taxa %>%
    anti_join(not_to_verify_taxa,
      by = colnames(taxa)
    )

  message("Find new synonyms...", appendLF = FALSE)
  # find new synonyms (= new triplets (taxonKey, bb_key, bb_acceptedKey))
  new_synonyms <-
    taxa %>%
    # remove not synonyms
    filter(!is.na(bb_taxonomicStatus) &
      !bb_taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL")) %>%
    anti_join(verification,
      by = c("taxonKey", "bb_key", "bb_acceptedKey")
    ) %>%
    mutate(
      bb_acceptedKingdom = NA_character_,
      bb_acceptedRank = NA_character_,
      bb_acceptedTaxonomicStatus = NA_character_,
      verificationKey = NA_character_,
      remarks = NA_character_,
      verifiedBy = NA_character_,
      dateAdded = Sys.Date(),
      outdated = FALSE
    ) %>%
    select(one_of(name_col_verification), everything())
  message("DONE.", appendLF = TRUE)

  # find new taxa not matched to GBIF backbone
  message("Find new unmatched taxa...", appendLF = FALSE)
  unmatched_taxa <-
    verification %>%
    filter(is.na(bb_key)) %>%
    distinct(taxonKey) %>%
    pull()
  new_unmatched_taxa <-
    taxa %>%
    filter(is.na(bb_key)) %>%
    filter(!taxonKey %in% unmatched_taxa) %>%
    mutate(
      bb_acceptedKingdom = NA_character_,
      bb_acceptedRank = NA_character_,
      bb_acceptedTaxonomicStatus = NA_character_,
      verificationKey = NA_character_,
      remarks = NA_character_,
      verifiedBy = NA_character_,
      dateAdded = Sys.Date(),
      outdated = FALSE
    ) %>%
    select(one_of(name_col_verification), everything())
  message("DONE.", appendLF = TRUE)

  # create df of updated bb_scientificName
  message("Update backbone scientific names...", appendLF = FALSE)
  if (nrow(verification) > 0) {
    updated_bb_scientificName <-
      verification %>%
      filter(!is.na(bb_scientificName)) %>%
      left_join(taxa,
        by = c("taxonKey", "bb_key", "bb_acceptedKey")
      ) %>%
      rename(
        "bb_scientificName" = "bb_scientificName.x",
        "updated_bb_scientificName" = "bb_scientificName.y"
      ) %>%
      filter(bb_scientificName != updated_bb_scientificName) %>%
      select(
        which(colnames(verification) %in% colnames(.)),
        updated_bb_scientificName,
        ends_with(".x")
      ) %>%
      rename_at(vars(ends_with(".x")), funs(str_remove(., "\\.x")))
    # update bb_scientificName of verification
    verification <-
      verification %>%
      anti_join(updated_bb_scientificName,
        by = colnames(verification)
      ) %>%
      bind_rows(updated_bb_scientificName %>%
        mutate(bb_scientificName = updated_bb_scientificName) %>%
        select(-updated_bb_scientificName))
    # version for info
    updated_bb_scientificName_short <-
      updated_bb_scientificName %>%
      select(
        taxonKey, bb_key, bb_acceptedKey,
        bb_scientificName, updated_bb_scientificName
      )
  }
  else {
    updated_bb_scientificName_short <- tibble(
      taxonKey = double(),
      bb_key = double(),
      bb_acceptedKey = double(),
      bb_scientificName = character(),
      updated_bb_scientificName = character()
    )
  }
  message("DONE.", appendLF = TRUE)

  # create df of updated acceptedName
  message("Update backbone accepted names...", appendLF = FALSE)
  if (nrow(verification) > 0) {
    updated_bb_acceptedName <-
      verification %>%
      filter(!is.na(bb_acceptedName)) %>%
      left_join(taxa,
        by = c("taxonKey", "bb_key", "bb_acceptedKey")
      ) %>%
      rename(
        "bb_acceptedName" = "bb_acceptedName.x",
        "updated_bb_acceptedName" = "bb_acceptedName.y"
      ) %>%
      filter(bb_acceptedName != updated_bb_acceptedName) %>%
      select(
        which(colnames(verification) %in% colnames(.)),
        updated_bb_acceptedName,
        ends_with(".x")
      ) %>%
      rename_at(vars(ends_with(".x")), funs(str_remove(., "\\.x")))
    # update bb_acceptedName of verification
    verification <-
      verification %>%
      anti_join(updated_bb_acceptedName,
        by = colnames(verification)
      ) %>%
      bind_rows(updated_bb_acceptedName %>%
        mutate(bb_acceptedName = updated_bb_acceptedName) %>%
        select(-updated_bb_acceptedName))
    # version for info
    updated_bb_acceptedName_short <-
      updated_bb_acceptedName %>%
      select(
        taxonKey, bb_key, bb_acceptedKey,
        bb_acceptedName, updated_bb_acceptedName
      )
  }
  else {
    updated_bb_acceptedName_short <- tibble(
      taxonKey = double(),
      bb_key = double(),
      bb_acceptedKey = double(),
      bb_acceptedName = character(),
      updated_bb_acceptedName = character()
    )
  }
  message("DONE.", appendLF = TRUE)

  # add new synonyms to verification
  verification <-
    verification %>%
    bind_rows(new_synonyms)

  # add new unmatches to verification
  verification <-
    verification %>%
    bind_rows(new_unmatched_taxa)

  # retrieve backbone information about taxa the synonyms point to
  message("Retrieve backbone info about accepted taxa for synonyms...",
    appendLF = FALSE
  )
  if (nrow(verification) > 0) {
    accepted_keys <-
      verification %>%
      distinct(bb_acceptedKey) %>%
      filter(!is.na(bb_acceptedKey))
    accepted_info <- pmap_dfr(
      accepted_keys,
      function(bb_acceptedKey) {
        name_usage(
          key = bb_acceptedKey,
          return = "data"
        )
      }
    ) %>%
      select(key, kingdom, rank, taxonomicStatus) %>%
      rename(
        bb_acceptedKey = key,
        bb_acceptedKingdom = kingdom,
        bb_acceptedRank = rank,
        bb_acceptedTaxonomicStatus = taxonomicStatus
      )
    # Update backbone info about accepted taxa in verification
    verification <-
      verification %>%
      select(-c(
        bb_acceptedKingdom,
        bb_acceptedRank,
        bb_acceptedTaxonomicStatus
      )) %>%
      left_join(accepted_info, by = "bb_acceptedKey") %>%
      select(name_col_verification)
    # add backbone info to new_synonys too
    new_synonyms <-
      new_synonyms %>%
      select(-c(
        bb_acceptedKingdom,
        bb_acceptedRank,
        bb_acceptedTaxonomicStatus
      )) %>%
      left_join(verification %>%
        select(
          taxonKey, bb_key, bb_acceptedKey,
          bb_acceptedKingdom, bb_acceptedRank,
          bb_acceptedTaxonomicStatus
        ),
      by = c("taxonKey", "bb_key", "bb_acceptedKey")
      )
  } else {
    verification <-
      verification %>%
      mutate(
        bb_acceptedKey = double(),
        bb_acceptedKingdom = character(),
        bb_acceptedRank = character(),
        bb_acceptedTaxonomicStatus = character()
      )
  }
  message("DONE.", appendLF = TRUE)

  # handle outdated taxa
  message("Detect outdated data...", appendLF = FALSE)
  # set outdated = FALSE for taxa which are in use:
  # some outdated taxa could be back in use
  if (nrow(verification) > 0) {
    not_outdated_taxa <-
      verification %>%
      inner_join(taxa %>%
        select(taxonKey, bb_key, bb_acceptedKey),
      by = c("taxonKey", "bb_key", "bb_acceptedKey")
      ) %>%
      mutate(outdated = FALSE) %>%
      mutate(remarks = str_remove(remarks, "Outdated taxa."))
    # define the outdated taxa subset
    outdated_taxa <-
      verification %>%
      anti_join(taxa, by = c("taxonKey", "bb_key", "bb_acceptedKey"))

    # not add 'Outdated taxa' in remarks to already outdated taxa
    old_outdated_taxa <-
      outdated_taxa %>%
      filter(outdated == TRUE)
    # set outdated = TRUE, add 'Outdated taxa.' to remarks for new outdated taxa
    new_outdated_taxa <-
      outdated_taxa %>%
      filter(outdated == FALSE) %>%
      mutate(
        remarks = paste(remarks, "Outdated taxa."),
        outdated = TRUE
      )
    outdated_taxa <- bind_rows(old_outdated_taxa, new_outdated_taxa)
    # compose verification back together
    verification <-
      not_outdated_taxa %>%
      bind_rows(outdated_taxa)
  }
  message("DONE.", appendLF = TRUE)

  # check verificationKey values against GBIF and GBIF Backbone
  message("Check verification keys...", appendLF = FALSE)

  verification_keys <- verification %>%
    filter(!is.na(verificationKey)) %>%
    filter(nchar(verificationKey) > 0) %>%
    pull(verificationKey)
  verification_keys <- paste(verification_keys, collapse = ",")
  verification_keys <- unlist(str_split(verification_keys, ","))
  check_verificationKey <- gbif_verify_keys(verification_keys)
  if (is.null(check_verificationKey)) {
    check_verificationKey <- tibble(
      key = double(),
      is_taxonKey = logical(),
      is_from_gbif_backbone = logical(),
      is_synonym = logical()
    )
  }
  message("DONE.", appendLF = TRUE)

  # find taxa duplicates
  message("Find scientific names used in multiple taxa...", appendLF = FALSE)
  if (nrow(verification > 0)) {
    duplicates <-
      verification %>%
      filter(!is.na(bb_key) & !is.na(bb_acceptedKey)) %>%
      group_by(bb_key, bb_acceptedKey) %>%
      count() %>%
      filter(n > 1) %>%
      left_join((verification %>%
        select(bb_key, bb_acceptedKey, bb_scientificName)),
      by = c("bb_key", "bb_acceptedKey")
      ) %>%
      select(bb_key, bb_acceptedKey, bb_scientificName, n) %>%
      arrange(desc(n)) %>%
      ungroup()
  } else {
    duplicates <- tibble(
      bb_key = double(),
      bb_acceptedKey = double(),
      bb_scientificName = character(),
      n = double()
    )
  }
  message("DONE.", appendLF = TRUE)

  # order verification by outdated and dateAdded
  verification <-
    verification %>%
    arrange(outdated, desc(dateAdded))

  # add not outdated taxa from verification to not_to_verify_taxa
  taxa <-
    verification %>%
    filter(outdated == FALSE) %>%
    select(name_col_taxa, verificationKey) %>%
    left_join(taxa_input,
      by = name_col_taxa
    ) %>%
    bind_rows(not_to_verify_taxa) %>%
    right_join(ordered_taxon_keys, by = "taxonKey")

  # split outdated_taxa in outdated_unmatched_taxa and outdated_synonyms
  outdated_unmatched_taxa <-
    outdated_taxa %>%
    filter(is.na(bb_key))
  outdated_synonyms <-
    outdated_taxa %>%
    filter(!is.na(bb_acceptedKey))

  return(list(
    taxa = taxa,
    verification = verification,
    info = list(
      new_synonyms = as_tibble(new_synonyms),
      new_unmatched_taxa = as_tibble(new_unmatched_taxa),
      outdated_unmatched_taxa = as_tibble(outdated_unmatched_taxa),
      outdated_synonyms = as_tibble(outdated_synonyms),
      updated_bb_scientificName = as_tibble(updated_bb_scientificName_short),
      updated_bb_acceptedName = as_tibble(updated_bb_acceptedName_short),
      duplicates = duplicates,
      check_verificationKey = check_verificationKey
    )
  ))
}
