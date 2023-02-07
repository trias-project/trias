# Define inputs

my_taxa <- tibble::tibble(
  taxonKey = c(
    141117238,
    113794952,
    141264857,
    100480872,
    141264614,
    100220432,
    141264835,
    140562956,
    145953989,
    148437916,
    141264849,
    101790530
  ),
  scientificName = c(
    "Aspius aspius",
    "Rana catesbeiana",
    "Polystichum tsus-simense J.Smith",
    "Apus apus (Linnaeus, 1758)",
    "Begonia x semperflorens hort.",
    "Rana catesbeiana",
    "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley",
    "Ferrissia fragilis",
    "Ferrissia fragilis",
    "Ferrissia fragilis",
    "Pterocarya x rhederiana C.K. Schneider",
    "Stenelmis williami Schmude"
  ),
  datasetKey = c(
    "98940a79-2bf1-46e6-afd6-ba2e85a26f9f",
    "e4746398-f7c4-47a1-a474-ae80a4f18e92",
    "9ff7d317-609b-4c08-bd86-3bc404b77c42",
    "39653f3e-8d6b-4a94-a202-859359c164c5",
    "9ff7d317-609b-4c08-bd86-3bc404b77c42",
    "b351a324-77c4-41c9-a909-f30f77268bc4",
    "9ff7d317-609b-4c08-bd86-3bc404b77c42",
    "289244ee-e1c1-49aa-b2d7-d379391ce265",
    "3f5e930b-52a5-461d-87ec-26ecd66f14a3",
    "1f3505cd-5d98-4e23-bd3b-ffe59d05d7c2",
    "9ff7d317-609b-4c08-bd86-3bc404b77c42",
    "9ca92552-f23a-41a8-a140-01abaa31c931"
  ),
  bb_key = c(
    2360181,
    2427092,
    2651108,
    5228676,
    NA,
    2427092,
    NA,
    2291152,
    2291152,
    2291152,
    NA,
    1033588
  ),
  bb_scientificName = c(
    "Aspius aspius (Linnaeus, 1758)",
    "Rana catesbeiana Shaw, 1802",
    "Polystichum tsus-simense (Hook.) J.Sm.",
    "Apus apus (Linnaeus, 1758)",
    NA,
    "Rana catesbeiana Shaw, 1802",
    NA,
    "Ferrissia fragilis (Tryon, 1863)",
    "Ferrissia fragilis (Tryon, 1863)",
    "Ferrissia fragilis (Tryon, 1863)",
    NA,
    "Stenelmis williami Schmude"
  ),
  bb_kingdom = c(
    "Animalia",
    "Animalia",
    "Plantae",
    "Animalia",
    NA,
    "Animalia",
    NA,
    "Animalia",
    "Animalia",
    "Animalia",
    NA,
    "Animalia"
  ),
  bb_rank = c(
    "SPECIES",
    "SPECIES",
    "SPECIES",
    "SPECIES",
    NA,
    "SPECIES",
    NA,
    "SPECIES",
    "SPECIES",
    "SPECIES",
    NA,
    "SPECIES"
  ),
  bb_taxonomicStatus = c(
    "SYNONYM",
    "SYNONYM",
    "SYNONYM",
    "ACCEPTED",
    NA,
    "SYNONYM",
    NA,
    "SYNONYM",
    "SYNONYM",
    "SYNONYM",
    NA,
    "SYNONYM"
  ),
  bb_acceptedKey = c(
    5851603,
    2427091,
    4046493,
    NA,
    NA,
    2427091,
    NA,
    9520065,
    9520065,
    9520065,
    NA,
    1033553
  ),
  bb_acceptedName = c(
    "Leuciscus aspius (Linnaeus, 1758)",
    "Lithobates catesbeianus (Shaw, 1802)",
    "Polystichum luctuosum (Kunze) Moore.",
    NA,
    NA,
    "Lithobates catesbeianus (Shaw, 1802)",
    NA,
    "Ferrissia californica (Rowell, 1863)",
    "Ferrissia californica (Rowell, 1863)",
    "Ferrissia californica (Rowell, 1863)",
    NA,
    "Stenelmis Dufour, 1835"
  ),
  taxonID = c(
    "alien-fishes-checklist:taxon:c937610f85ea8a74f105724c8f198049",
    "88",
    "alien-plants-belgium:taxon:57c1d111f14fd5f3271b0da53c05c745",
    "4512",
    "alien-plants-belgium:taxon:9a6c5ed8907ff169433fe44fcbff0705",
    "80-syn",
    "alien-plants-belgium:taxon:29409d1e1adc88d6357dd0be13350d6c",
    "alien-macroinvertebrates-checklist:taxon:73f271d93128a4e566e841ea6e3abff0",
    "rinse-checklist:taxon:7afe7b1fbdd06cbdfe97272567825c09",
    "ad-hoc-checklist:taxon:32dc2e18733fffa92ba4e1b35d03c4e2",
    "alien-plants-belgium:taxon:56d6564f59d9092401c454849213366f",
    "193729"
  )
)

# Add column verificationKey which will be overwritten by verify_taxa
my_taxa_vk <- dplyr::mutate(my_taxa, verificationKey = 1)

my_verification <- tibble::tibble(
  taxonKey = c(
    113794952,
    141264857,
    143920280,
    141264835,
    141264614,
    140562956,
    145953989,
    128897752,
    101790530,
    141265523
  ),
  scientificName = c(
    "Rana catesbeiana",
    "Polystichum tsus-simense J.Smith",
    "Lemnaceae",
    "Spiranthes cernua (L.) Richard x S. odorata (Nuttall) Lindley",
    "Begonia x semperflorens hort.",
    "Ferrissia fragilis",
    "Ferrissia fragilis",
    "Python reticulatus Fitzinger, 1826",
    "Stenelmis williami Schmude",
    "Veronica austriaca Jacq."
  ),
  datasetKey = c(
    "e4746398-f7c4-47a1-a474-ae80a4f18e92",
    "9ff7d317-609b-4c08-bd86-3bc404b77c42",
    "e4746398-f7c4-47a1-a474-ae80a4f18e92",
    "9ff7d317-609b-4c08-bd86-3bc404b77c42",
    "9ff7d317-609b-4c08-bd86-3bc404b77c42",
    "289244ee-e1c1-49aa-b2d7-d379391ce265",
    "3f5e930b-52a5-461d-87ec-26ecd66f14a3",
    "7ddf754f-d193-4cc9-b351-99906754a03b",
    "9ca92552-f23a-41a8-a140-01abaa31c931",
    "9ff7d317-609b-4c08-bd86-3bc404b77c42"
  ),
  bb_key = c(
    2427092,
    2651108,
    6723,
    NA,
    NA,
    2291152,
    2291152,
    7587934,
    1033588,
    NA
  ),
  bb_scientificName = c(
    "Rana catesbeiana Shaw, 1802",
    "Polystichum tsus-tsus-tsus (Hook.) Captain",
    "Lemnaceae",
    NA,
    NA,
    "Ferrissia fragilis (Tryon, 1863)",
    "Ferrissia fragilis (Tryon, 1863)",
    "Python reticulatus Fitzinger, 1826",
    "Stenelmis williami Schmude",
    NA
  ),
  bb_kingdom = c(
    "Animalia",
    "Plantae",
    "Plantae",
    NA,
    NA,
    "Animalia",
    "Animalia",
    "Animalia",
    "Animalia",
    NA
  ),
  bb_rank = c(
    "SPECIES",
    "SPECIES",
    "FAMILY",
    NA,
    NA,
    "SPECIES",
    "SPECIES",
    "SPECIES",
    "SPECIES",
    NA
  ),
  bb_taxonomicStatus = c(
    "SYNONYM",
    "SYNONYM",
    "SYNONYM",
    NA,
    NA,
    "SYNONYM",
    "SYNONYM",
    "SYNONYM",
    "SYNONYM",
    NA
  ),
  bb_acceptedKey = c(
    2427091,
    4046493,
    6979,
    NA,
    NA,
    9520065,
    9520065,
    9260388,
    1033553,
    NA
  ),
  bb_acceptedName = c(
    "Lithobates dummyus (Batman, 2018)",
    "Polystichum luctuosum (Kunze) Moore.",
    "Araceae",
    NA,
    NA,
    "Ferrissia californica (Rowell, 1863)",
    "Ferrissia californica (Rowell, 1863)",
    "Malayopython reticulatus (Schneider, 1801)",
    "Stenelmis Dufour, 1835",
    NA
  ),
  bb_acceptedKingdom = c(
    "Animalia",
    "Plantae",
    "Plantae",
    NA,
    NA,
    "Animalia",
    "Animalia",
    "Animalia",
    "Animalia",
    NA
  ),
  bb_acceptedRank = c(
    "SPECIES",
    "SPECIES",
    "FAMILY",
    NA,
    NA,
    "SPECIES",
    "SPECIES",
    "SPECIES",
    "GENUS",
    NA
  ),
  bb_acceptedTaxonomicStatus = c(
    "ACCEPTED",
    "ACCEPTED",
    "ACCEPTED",
    NA,
    NA,
    "ACCEPTED",
    "ACCEPTED",
    "ACCEPTED",
    "ACCEPTED",
    NA
  ),
  verificationKey = c(
    2427091,
    4046493,
    6979,
    "2805420,2805363",
    NA,
    NA,
    NA,
    9260388,
    NA,
    3172099
  ),
  remarks = c(
    "dummy example 1: bb_acceptedName should be updated.",
    "dummy example 2: bb_scientificName should be updated.",
    "dummy example 3: not used anymore. Set outdated = TRUE.",
    "dummy example 4: multiple keys in verificationKey are allowed.",
    "dummy example 5: nothing should happen.",
    "dummy example 6: datasetKey should not be modified. If new taxa come in
   with same name from other checklsits, they should be added as new rows.
   Report them as duplicates in duplicates_taxa",
    "dummy example 7: datasetKey should not be modified. If new taxa come in
   with same name from other checklsits, they should be added as new rows.
   Report them as duplicates in duplicates_taxa",
    "dummy example 9: outdated synonym. outdated is already TRUE. No actions.",
    "dummy example 10: outdated synonym. Not outdated anymore. Change outdated
   back to FALSE.",
    "dummy example 11: outdated unmatched taxa. Set outdated = TRUE."
  ),
  verifiedBy = c(
    "Damiano Oldoni",
    "Peter Desmet",
    "Stijn Van Hoey",
    "Tanja Milotic",
    NA,
    NA,
    NA,
    "Lien Reyserhove",
    NA,
    "Dimitri Brosens"
  ),
  dateAdded = as.Date(
    c(
      "2018-07-01",
      "2018-07-01",
      "2018-07-01",
      "2018-07-16",
      "2018-07-16",
      "2018-07-01",
      "2018-11-20",
      "2018-12-01",
      "2018-12-02",
      "2018-12-03"
    )
  ),
  outdated = c(
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    FALSE
  )
)

my_taxa_other_colnames <-
  dplyr::rename(
    my_taxa,
    checklist = datasetKey,
    scientific_names = scientificName
  )

my_verification_other_colnames <-
  dplyr::rename(
    my_verification,
    backbone_scientific_names = bb_scientificName,
    backbone_accepted_names = bb_acceptedName,
    is_outdated = outdated,
    author_verification = verifiedBy
  )

my_taxa_duplicates <-
  my_taxa[1:2, ]
my_taxa_duplicates$taxonKey[2] <- my_taxa_duplicates$taxonKey[1]

my_verification_duplicates <-
  my_verification[1:2, ]
my_verification_duplicates$taxonKey[2] <- my_verification_duplicates$taxonKey[1]

my_taxa_nas <-
  my_taxa_duplicates
my_taxa_nas$taxonKey[2] <- NA_real_

my_verification_nas <-
  my_verification_duplicates
my_verification_nas$taxonKey[2] <- NA_real_


context("input_verify_taxa")

test_that("taxa is a data frame", {
  expect_error(
    verify_taxa(
      taxa = 3,
      verification = my_verification
    ),
    "taxa is not a data frame"
  )
  expect_error(
    verify_taxa(
      taxa = c("23"),
      verification = my_verification
    ),
    "taxa is not a data frame"
  )
})


test_that("verification is a data frame", {
  expect_error(
    verify_taxa(
      taxa = my_taxa,
      verification = 3
    ),
    "verification is not a data frame"
  )
  expect_error(
    verify_taxa(
      taxa = my_taxa,
      verification = c("3")
    ),
    "verification is not a data frame"
  )
})


# no missing taxon keys in both input taxa and verification df (if not NULL)
test_that("No missing taxon keys in input taxa and verification dfs", {
  expect_error(
    verify_taxa(taxa = my_taxa_nas, verification = my_verification),
    paste(
      "Missing values found in taxon keys of input taxa.",
      "Check values in column taxonKey."
    )
  )
  expect_error(
    verify_taxa(taxa = my_taxa, verification = my_verification_nas),
    paste(
      "Missing values found in taxon keys of input taxa.",
      "Check values in column taxonKey."
    )
  )
})

# taxon keys are unique
test_that("Taxon keys are unique in input taxa and verification dfs", {
  expect_error(
    verify_taxa(taxa = my_taxa_duplicates, verification = my_verification),
    paste(
      "Taxon keys of input taxa must be unique.",
      "Check values in column taxonKey."
    )
  )
  expect_error(
    verify_taxa(taxa = my_taxa, verification = my_verification_duplicates),
    paste(
      "Taxon keys of input taxa must be unique.",
      "Check values in column taxonKey."
    )
  )
})


# different taxa column names
taxa_test1 <- tibble::tibble(
  bad_checklist_taxonKey_colname = c(123452),
  bad_checklist_scientificName_colname = c("Aspius aspius"),
  bad_checklist_datasetKey_colname = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  bad_backbone_taxonKey_colname = c(2360181),
  bad_backbone_scientificName_colname = c("Aspius aspius (Linnaeus, 1758)"),
  bad_backbone_kingdom_colname = c("Animalia"),
  bad_backbone_rank_colname = c("SPECIES"),
  bad_backbone_taxonomicStatus_colname = c("SYNONYM"),
  bad_backbone_acceptedKey_colname = c(5851603),
  bad_backbone_acceptedName_colname = c("Leuciscus aspius (Linnaeus, 1758)")
)

# missing column
taxa_test2 <- tibble::tibble(
  taxonKey = c(123452),
  scientificName = c("Aspius aspius"),
  datasetKey = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  # bb_acceptedKey is missing
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)")
)

test_that("taxa column names are correct", {
  expect_error(verify_taxa(
    taxa = taxa_test1,
    verification = my_verification
  ),
  paste(
    "The following columns of taxa are not present:",
    "taxonKey, scientificName, datasetKey, bb_key, bb_scientificName,",
    "bb_kingdom, bb_rank, bb_taxonomicStatus, bb_acceptedKey, bb_acceptedName.",
    "Did you maybe forget to provide the mapping of columns named differently",
    "than the default names?"
  ),
  fixed = TRUE
  )
  expect_error(verify_taxa(
    taxa = taxa_test2,
    verification = my_verification
  ),
  paste(
    "The following columns of taxa are not present:",
    "bb_acceptedKey. Did you maybe forget to provide the mapping of columns",
    "named differently than the default names?"
  ),
  fixed = TRUE
  )
})

# inconsistency about unmatched taxa
taxa_test3 <- tibble::tibble(
  taxonKey = c(123452),
  scientificName = c("Aspius aspius"),
  datasetKey = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  bb_key = c(NA_integer_),
  bb_scientificName = c(NA_character_),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(3483948),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)")
)

test_that("consistency of 'taxa' about GBIF backbone info columns", {
  expect_error(
    verify_taxa(
      taxa = taxa_test3,
      verification = my_verification
    ),
    "Columns with GBIF Backbone info should be empty for unmatched taxa.",
    fixed = TRUE
  )
})

# different verification column names
verification_test1 <- tibble::tibble(
  bad_checklist_taxonKey = c(12341),
  bad_checklist_scientificName_colname = c("Aspius aspius"),
  bad_datasetKey_colname = "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  bad_backbone_taxonKey_colname = c(2360181),
  bad_backbone_scientificName_colname = c("Aspius aspius (Linnaeus, 1758)"),
  bad_backbone_kingdom_colname = c("Animalia"),
  bad_backbone_rank_colname = c("SPECIES"),
  bad_backbone_taxonomicStatus_colname = c("SYNONYM"),
  bad_backbone_acceptedKey_colname = c(5851603),
  bad_backbone_acceptedName_colname = c("Leuciscus aspius (Linnaeus, 1758)"),
  bad_backbone_acceptedKingdom_colname = c("Animalia"),
  bad_backbone_acceptedrank_colname = c("SPECIES"),
  bad_backbone_acceptedTaxonomicStatus_colname = c("ACCEPTED"),
  bad_verificationKey_colname = c(2427091),
  bad_remarks_colname = c("dummy example 1: backbone_accepted should be updated"),
  bad_verifiedBy_colname = c("Damiano Oldoni"),
  bad_dateAdded_colname = c(as.Date("2018-01-01")),
  bad_outdated = c(FALSE)
)

# missing columns
verification_test2 <- tibble::tibble(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  # datasetKey column missing
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  # bb_kingdom column missing
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(5851603),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  # bb_acceptedKingdom
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = c("dummy example 1: backbone_accepted should be updated"),
  verifiedBy = c("Dami Oldi"),
  # dateAdded column missing
  outdated = c(FALSE)
)

# inconsistency bb_acceptedName - bb_acceptedKey
verification_test3 <- tibble::tibble(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474-ae80a4f18e92"),
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(NA_integer_),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_acceptedKingdom = c("Animalia"),
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = c("dummy example 1: backbone_accepted should be updated"),
  verifiedBy = c("Damiano Oldoni"),
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE)
)

# accepted taxa present (only synonyms and unmatched taxa allowed)
verification_test4 <- tibble::tibble(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474-ae80a4f18e92"),
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("ACCEPTED"),
  bb_acceptedKey = c(5851603),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_acceptedKingdom = c("Animalia"),
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = NA_character_,
  verifiedBy = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE)
)

# outdated must to be TRUE or FALSE.
verification_test5 <- tibble::tibble(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474-ae80a4f18e92"),
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(5851603),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_acceptedKingdom = c("Animalia"),
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = NA_character_,
  verifiedBy = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(NA)
)

# datasetKey should be 36 characters long
verification_test6 <- tibble::tibble(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474-ae80a4f18e92,other stuff"),
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(5851603),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_acceptedKingdom = c("Animalia"),
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = NA_character_,
  verifiedBy = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE)
)

# commas not allowed in datasetKey
verification_test7 <- tibble::tibble(
  taxonKey = c(141117238),
  scientificName = c("Aspius aspius"),
  datasetKey = c("e4746398-f7c4-47a1-a474,ae80a4f18e92"),
  bb_key = c(2360181),
  bb_scientificName = c("Aspius aspius (Linnaeus, 1758)"),
  bb_kingdom = c("Animalia"),
  bb_rank = c("SPECIES"),
  bb_taxonomicStatus = c("SYNONYM"),
  bb_acceptedKey = c(5851603),
  bb_acceptedName = c("Leuciscus aspius (Linnaeus, 1758)"),
  bb_acceptedKingdom = c("Animalia"),
  bb_acceptedRank = c("SPECIES"),
  bb_acceptedTaxonomicStatus = c("ACCEPTED"),
  verificationKey = c(2427091),
  remarks = NA_character_,
  verifiedBy = NA_character_,
  dateAdded = c(as.Date("2010-01-01")),
  outdated = c(FALSE)
)

test_that("verify_taxa column names are correct", {
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test1
  ),
  paste(
    "The following columns of verification are not present:",
    "taxonKey, scientificName, datasetKey, bb_key, bb_scientificName,",
    "bb_kingdom, bb_rank, bb_taxonomicStatus,",
    "bb_acceptedKey, bb_acceptedName, bb_acceptedKingdom, bb_acceptedRank,",
    "bb_acceptedTaxonomicStatus, verificationKey, remarks, verifiedBy,",
    "dateAdded, outdated. Did you maybe forget to provide the mapping of",
    "columns named differently than the default names?"
  ),
  fixed = TRUE
  )
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test2
  ),
  paste(
    "The following columns of verification are not present:",
    "datasetKey, bb_kingdom, bb_acceptedKingdom, dateAdded.",
    "Did you maybe forget to provide the mapping of columns named differently",
    "than the default names?"
  ),
  fixed = TRUE
  )
})

test_that("synonym relations are inconsistent", {
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test3
  ),
  "bb_acceptedName and bb_acceptedKey should be both NA or both present.",
  fixed = TRUE
  )
})

test_that("accepted taxa in verification input", {
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test4
  ),
  "Only synonyms and unmatched taxa allowed in verification.",
  fixed = TRUE
  )
})

test_that("restrictions on input columns of verification", {
  expect_error(verify_taxa(
    taxa = my_taxa,
    verification = verification_test5
  ),
  "Only logicals (TRUE/FALSE) allowed in 'outdated' of verification.",
  fixed = TRUE
  )
})

test_that("valid datsetKey values", {
  expect_error(
    verify_taxa(
      taxa = my_taxa,
      verification = verification_test6
    ),
    paste(
      "Incorrect datesetKey:", verification_test6$datasetKey,
      "Is expected to be 36-character UUID."
    )
  )
  expect_error(
    verify_taxa(
      taxa = my_taxa,
      verification = verification_test7
    ),
    paste(
      "Incorrect datesetKey:", verification_test7$datasetKey,
      "Is expected to be 36-character UUID."
    )
  )
})

context("output_verify_taxa")

# output
output1 <- verify_taxa(taxa = my_taxa, verification = my_verification)
output2 <- verify_taxa(taxa = my_taxa)
output3 <- verify_taxa(taxa = my_taxa_vk, verification = my_verification)
output4 <- verify_taxa(
  taxa = my_taxa_other_colnames,
  verification = my_verification_other_colnames,
  datasetKey = "checklist",
  scientificName = "scientific_names",
  verification_bb_scientificName = "backbone_scientific_names",
  verification_bb_acceptedName = "backbone_accepted_names",
  verification_outdated = "is_outdated",
  verification_verifiedBy = "author_verification"
)
outputs <- list(output1, output2, output3, output4)
test_that("output structure", {
  expect_true(all(purrr::map_lgl(outputs, function(x) {
    class(x) == "list"
  })))
  expect_true(all(purrr::map_lgl(outputs, function(x) {
    length(x) == 3
  })))
  expect_true(all(purrr::map_lgl(outputs, function(x) {
    class(x$info) == "list"
  })))
  expect_true(length(output1$info) == 8)
  expect_true(length(output2$info) == 8)
  expect_equivalent(output1$info, output3$info)
  expect_true(all())
  expect_true(all(purrr::map_lgl(outputs, function(x) {
    is.data.frame(x$taxa)
  })))
  expect_equivalent(output1$taxa, output3$taxa)
  expect_true(
    all(purrr::map_lgl(outputs, function(x) {
      is.data.frame(x$verification)
    }))
  )
  expect_equivalent(output1$verification, output3$verification)
  expect_true(all(purrr::map_lgl(output1$info, ~ is.data.frame(.))))
  expect_true(all(purrr::map_lgl(output2$info, ~ is.data.frame(.))))
  expect_true(all(purrr::map_lgl(output1$info, ~ (!"grouped_df" %in% class(.)))))
  expect_true(all(purrr::map_lgl(output2$info, ~ (!"grouped_df" %in% class(.)))))
  expect_equivalent(output1$info, output3$info)
  expect_true(
    all(names(output4$verification) == names(my_verification_other_colnames))
  )
  expect_true(
    all(purrr::map_lgl(
      list(
        output4$info$outdated_unmatched_taxa,
        output4$info$outdated_synonyms
      ), function(x) {
        all(names(x) == names(my_verification_other_colnames))
      }
    ))
  )
  expect_true(all(names(output4$info$new_synonyms) ==
    names(my_verification_other_colnames)))
  expect_true(all(names(output4$info$new_unmatched_taxa) ==
    names(my_verification_other_colnames)))
  expect_true(
    all(names(output4$info$updated_bb_scientificName) ==
      c(
        "taxonKey", "bb_key", "bb_acceptedKey",
        "backbone_scientific_names", "updated_backbone_scientific_names"
      ))
  )
  expect_true(
    all(names(output4$info$updated_bb_acceptedName) ==
      c(
        "taxonKey", "bb_key", "bb_acceptedKey",
        "backbone_accepted_names", "updated_backbone_accepted_names"
      ))
  )
})

test_that("consitency input - output", {
  expect_true(nrow(output1$taxa) == nrow(my_taxa))
  expect_true(nrow(output2$taxa) == nrow(my_taxa))
  expect_true(ncol(output1$taxa) == ncol(my_taxa) + 1)
  expect_true(ncol(output2$taxa) == ncol(my_taxa) + 1)
  expect_true(all(output1$taxa$taxonKey == my_taxa$taxonKey))
  expect_true(all(output2$taxa$taxonKey == my_taxa$taxonKey))
  expect_true(
    nrow(output1$verification) ==
      nrow(my_verification) +
        nrow(output1$info$new_synonyms) +
        nrow(output1$info$new_unmatched_taxa)
  )
  expect_true(
    nrow(output2$verification) ==
      nrow(output2$info$new_synonyms) +
        nrow(output2$info$new_unmatched_taxa)
  )
  expect_true(nrow(output1$verification %>%
    filter(!is.na(verificationKey))) <=
    nrow(output1$info$check_verificationKey))
  expect_true(
    nrow(my_taxa %>%
      filter(bb_taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL"))) ==
      nrow(output2$taxa %>%
        filter(!is.na(verificationKey)))
  )
  expect_true(all(output1$info$new_synonyms$outdated == FALSE))
  expect_true(all(output2$info$new_synonyms$outdated == FALSE))
  expect_true(all(output1$info$new_unmatched_taxa$outdated == FALSE))
  expect_true(all(output2$info$new_unmatched_taxa$outdated == FALSE))
  expect_true(all(output2$verification$outdated == FALSE))
  expect_true(all(output1$info$outdated_unmatched_taxa$outdated == TRUE))
  expect_true(all(output2$info$outdated_unmatched_taxa$outdated == TRUE))
  expect_true(all(output1$info$outdated_synonyms$outdated == TRUE))
  expect_true(all(output2$info$outdated_synonyms$outdated == TRUE))
  expect_true(
    nrow(output1$info$outdated_synonyms) +
      nrow(output1$info$outdated_unmatched_taxa) ==
      nrow(dplyr::filter(output1$verification, outdated == TRUE))
  )
  expect_true(
    nrow(output2$info$outdated_synonyms) +
      nrow(output2$info$outdated_unmatched_taxa) ==
      nrow(dplyr::filter(output2$verification, outdated == TRUE))
  )
})

col_types_verification <- readr::cols(
  taxonKey = readr::col_double(),
  scientificName = readr::col_character(),
  datasetKey = readr::col_character(),
  bb_key = readr::col_double(),
  bb_scientificName = readr::col_character(),
  bb_kingdom = readr::col_character(),
  bb_rank = readr::col_character(),
  bb_taxonomicStatus = readr::col_character(),
  bb_acceptedKey = readr::col_double(),
  bb_acceptedName = readr::col_character(),
  bb_acceptedKingdom = readr::col_character(),
  bb_acceptedRank = readr::col_character(),
  bb_acceptedTaxonomicStatus = readr::col_character(),
  verificationKey = readr::col_character(),
  remarks = readr::col_character(),
  verifiedBy = readr::col_character(),
  dateAdded = readr::col_date(format = "%Y-%m-%d"),
  outdated = readr::col_logical()
)

col_types_output_taxa <- readr::cols(
  taxonKey = readr::col_double(),
  scientificName = readr::col_character(),
  datasetKey = readr::col_character(),
  bb_key = readr::col_double(),
  bb_scientificName = readr::col_character(),
  bb_kingdom = readr::col_character(),
  bb_rank = readr::col_character(),
  bb_taxonomicStatus = readr::col_character(),
  bb_acceptedName = readr::col_character(),
  bb_acceptedKey = readr::col_double(),
  verificationKey = readr::col_character(),
  taxonID = readr::col_character()
)

col_types_updated_names <- readr::cols(
  taxonKey = readr::col_double(),
  bb_key = readr::col_double(),
  bb_acceptedKey = readr::col_double()
)

output1_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_taxa.tsv"
    ),
    col_types = col_types_output_taxa
  )
output2_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_taxa.tsv"
    ),
    col_types = col_types_output_taxa
  )

output1_verification <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_verification.tsv"
    ),
    col_types = col_types_verification
  )
output2_verification <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_verification.tsv"
    ),
    col_types = col_types_verification
  )

output1_new_synonyms <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_new_synonyms.tsv"
    ),
    col_types = col_types_verification
  )
output2_new_synonyms <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_new_synonyms.tsv"
    ),
    col_types = col_types_verification
  )

output1_new_unmatched_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_new_unmatched_taxa.tsv"
    ),
    col_types = col_types_verification
  )
output2_new_unmatched_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_new_unmatched_taxa.tsv"
    ),
    col_types = col_types_verification
  )

output1_outdated_unmatched_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_outdated_unmatched_taxa.tsv"
    ),
    col_types = col_types_verification
  )

output2_outdated_unmatched_taxa <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_outdated_unmatched_taxa.tsv"
    ),
    col_types = col_types_verification
  )

output1_outdated_synonyms <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_outdated_synonyms.tsv"
    ),
    col_types = col_types_verification
  )

output2_outdated_synonyms <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_outdated_synonyms.tsv"
    ),
    col_types = col_types_verification
  )

output1_updated_bb_scientificName <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_updated_bb_scientificName.tsv"
    ),
    col_types = col_types_updated_names
  )

output2_updated_bb_scientificName <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_updated_bb_scientificName.tsv"
    ),
    col_types = col_types_updated_names
  )

output1_updated_bb_acceptedName <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_updated_bb_acceptedName.tsv"
    ),
    col_types = col_types_updated_names
  )

output2_updated_bb_acceptedName <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_updated_bb_acceptedName.tsv"
    ),
    col_types = col_types_updated_names
  )

output1_duplicates <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output1_duplicates.tsv"
    ),
    col_types = readr::cols(n = readr::col_integer())
  )
output2_duplicates <-
  readr::read_tsv(
    file = paste0(
      "./data_test_output_verify_taxa/",
      "output2_duplicates.tsv"
    ),
    col_types = readr::cols(n = readr::col_integer())
  )

test_that("output data.frames are correct", {
  expect_equivalent(output1$taxa, output1_taxa)
  expect_equivalent(output2$taxa, output2_taxa)
  # output4 with default column names should be exactly equal to output1
  output4_default_names_verification <-
    output4$verification %>%
    dplyr::rename(
      bb_scientificName = backbone_scientific_names,
      bb_acceptedName = backbone_accepted_names,
      outdated = is_outdated,
      verifiedBy = author_verification
    )
  expect_equivalent(output1$verification, output4_default_names_verification)
  expect_equivalent(
    output1$verification %>%
      # new synonyms and unmatched get date of today
      dplyr::select(-c(dateAdded, remarks)),
    output1_verification %>%
      # new synonyms and unmatched got paste date
      dplyr::select(-c(dateAdded, remarks))
  )
  expect_equivalent(
    output2$verification %>%
      # new synonyms and unmatched get date of today
      dplyr::select(-dateAdded),
    output2_verification %>%
      # new synonyms and unmatched got paste date
      dplyr::select(-dateAdded)
  )

  output4_default_names_new_synonyms <-
    output4$info$new_synonyms %>%
    dplyr::rename(
      bb_scientificName = backbone_scientific_names,
      bb_acceptedName = backbone_accepted_names,
      outdated = is_outdated,
      verifiedBy = author_verification
    )
  expect_equivalent(
    output1$info$new_synonyms,
    output4_default_names_new_synonyms
  )
  expect_equivalent(
    output1$info$new_synonyms %>%
      # new synonyms get date of today
      dplyr::select(-dateAdded),
    output1_new_synonyms %>%
      # unmatched got past date
      dplyr::select(-dateAdded)
  )
  expect_equivalent(
    output2$info$new_synonyms %>%
      # new synonyms get date of today
      dplyr::select(-dateAdded),
    output2_new_synonyms %>%
      # unmatched got past date
      dplyr::select(-dateAdded)
  )

  output4_default_names_new_unmatched_taxa <-
    output4$info$new_unmatched_taxa %>%
    dplyr::rename(
      bb_scientificName = backbone_scientific_names,
      bb_acceptedName = backbone_accepted_names,
      outdated = is_outdated,
      verifiedBy = author_verification
    )
  expect_equivalent(
    output1$info$new_unmatched_taxa,
    output4_default_names_new_unmatched_taxa
  )
  expect_equivalent(
    output1$info$new_unmatched_taxa %>%
      # unmatched get date of today
      dplyr::select(-dateAdded),
    output1_new_unmatched_taxa %>%
      # unmatched got past date
      dplyr::select(-dateAdded)
  )
  expect_equivalent(
    output2$info$new_unmatched_taxa %>%
      # unmatched get date of today
      dplyr::select(-dateAdded),
    output2_new_unmatched_taxa %>%
      # unmatched got past date
      dplyr::select(-dateAdded)
  )

  output4_default_names_outdated_unmatched_taxa <-
    output4$info$outdated_unmatched_taxa %>%
    dplyr::rename(
      bb_scientificName = backbone_scientific_names,
      bb_acceptedName = backbone_accepted_names,
      outdated = is_outdated,
      verifiedBy = author_verification
    )
  expect_equivalent(
    output1$info$outdated_unmatched_taxa,
    output4_default_names_outdated_unmatched_taxa
  )
  expect_equivalent(
    output1$info$outdated_unmatched_taxa,
    output1_outdated_unmatched_taxa
  )
  expect_equivalent(
    output2$info$outdated_unmatched_taxa,
    output2_outdated_unmatched_taxa
  )

  output4_default_names_outdated_synonyms <-
    output4$info$outdated_synonyms %>%
    dplyr::rename(
      bb_scientificName = backbone_scientific_names,
      bb_acceptedName = backbone_accepted_names,
      outdated = is_outdated,
      verifiedBy = author_verification
    )
  expect_equivalent(
    output1$info$outdated_synonyms,
    output4_default_names_outdated_synonyms
  )
  expect_equivalent(
    output1$info$outdated_synonyms,
    output1_outdated_synonyms
  )
  expect_equivalent(
    output2$info$outdated_synonyms,
    output2_outdated_synonyms
  )

  output4_default_names_updated_bb_scientificName <-
    output4$info$updated_bb_scientificName %>%
    dplyr::rename(
      bb_scientificName = backbone_scientific_names,
      updated_bb_scientificName = updated_backbone_scientific_names
    )
  expect_equivalent(
    output1$info$updated_bb_scientificName,
    output4_default_names_updated_bb_scientificName
  )
  expect_equivalent(
    output1$info$updated_bb_scientificName,
    output1_updated_bb_scientificName
  )
  expect_equivalent(
    output2$info$updated_bb_scientificName,
    output2_updated_bb_scientificName
  )

  output4_default_names_updated_bb_acceptedName <-
    output4$info$updated_bb_acceptedName %>%
    dplyr::rename(
      bb_acceptedName = backbone_accepted_names,
      updated_bb_acceptedName = updated_backbone_accepted_names
    )
  expect_equivalent(
    output1$info$updated_bb_acceptedName,
    output4_default_names_updated_bb_acceptedName
  )
  expect_equivalent(
    output1$info$updated_bb_acceptedName,
    output1_updated_bb_acceptedName
  )
  expect_equivalent(
    output2$info$updated_bb_acceptedName,
    output2_updated_bb_acceptedName
  )

  output4_default_names_duplicates <-
    output4$info$duplicates %>%
    dplyr::rename(bb_scientificName = backbone_scientific_names)
  expect_equivalent(
    output1$info$duplicates,
    output4_default_names_duplicates
  )
  expect_equivalent(output1$info$duplicates, output1_duplicates)
  expect_equivalent(output2$info$duplicates, output2_duplicates)
  # check_verification_key df no tested here: output of another TrIAS function
  # only check 0 rows with output2
  expect_true(nrow(output2$info$check_verificationKey) == 0)
})
