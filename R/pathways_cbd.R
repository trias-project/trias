#' Pathways of introduction as defined by CBD
#'
#' Function to get all CBD pathays of introdution at level 1 (`pathway_level1`)
#' and level 2 (`pathway_level2`). Added pathway `unknown` at level 1 and level
#' 2 for classifying taxa without pathway (at level 1 or level 2) information.
#'
#' @return A tibble data.frame with 2 columns: `pathway_level1` and
#'   `pathway_level2`.
#' @export
#' @usage pathways_cbd()

pathways_cbd <- function() pathwayscbd

pathwayscbd <- dplyr::tibble(
  pathway_level1 = c(
    rep("release", 9),
    rep("escape", 13),
    rep("contaminant", 11),
    rep("stowaway", 12),
    rep("corridor", 3),
    rep("unaided", 2),
    "unknown"
  ),
  pathway_level2 = c(
    "biological_control",
    "erosion_control",
    "fishery",
    "hunting",
    "landscape_improvement",
    "conservation",
    "use",
    "other",
    "unknown",
    "agriculture",
    "aquaculture",
    "zoo",
    "pet",
    "farm",
    "forestry",
    "fur_farm",
    "horticulture",
    "ornamental",
    "research",
    "food_bait",
    "other",
    "unknown",
    "nursery",
    "bait",
    "food",
    "on_animals",
    "animal_parasite",
    "on_plants",
    "plant_parasite",
    "seed",
    "timber",
    "habitat_material",
    "unknown",
    "fishing_equipment",
    "container",
    "airplane",
    "ship",
    "machinery",
    "people_luggage",
    "organic_packing",
    "ballast_water",
    "hull_fouling",
    "vehicles",
    "other",
    "unknown",
    "water",
    "land",
    "unknown",
    "natural_dispersal",
    "unknown",
    "unknown"
  )
)
