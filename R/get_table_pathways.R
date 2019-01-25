#' Pathway count indicator figure
#'
#' @param data data.frame According to the specification of the Trias pipeline
#' @param category character One of the kingdoms as given in GBIF: \itemize{
#'   \item{"Plantae"} \item{Animalia} \item{} \item{"Fungi"} \item{"Chromista"}
#'   \item{"Archaea"} \item{Bacteria} \item{Protozoa} \item{Viruses}
#'   \item{incertae sedis} } It can also be one of the following not kingdoms:
#'   #'\itemize{\item{Chordata} \item{Not Chordata} }
#' @param n_species integer The maximum number of species to return as examples
#'   per pathway. For groups with less species than \code{n_species}, all
#'   species are given. Default: 5.
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' indicator_pathway(data, "Plantae", n_species = 8)
get_table_pathways <- function(data, category, n_species = 5) {
  
  # handle asymmetric category system (Chordata, Not Chordta are not kingdoms) 
  if (!category %in% c("Chordata", "Not Chordata")) {
    filtered_data <- data %>% filter(kingdom == category)
  } else {
    if (category == "Chordata") {
      filtered_data <- data %>% filter(phylum == category)
    } else {
      filtered_data <- data %>% 
        filter(kingdom == "Animalia") %>%
        filter(phylum != category)
    }
  }
  
  # Create groups basd on pathway level1 and level2
  preprocess_data <- filtered_data %>% 
    distinct(scientificName, species, genus, family, order, 
             class, phylum, kingdom, speciesKey, country,
             pathway_level1, pathway_level2) %>%
    mutate(pathway_level1 = ifelse(is.na(pathway_level2), 
                                   NA, .$pathway_level1)) %>%
    group_by(pathway_level1, pathway_level2)
  
  # Assess size of sample per group
  pathway_data <- preprocess_data %>% 
    count() %>% 
    rowwise() %>%
    mutate(size_sample = ifelse(n > n_species,
                                n_species, n))
  # Make df with sample species
  samples <- pmap_dfr(list(pathway_data$pathway_level1,
                           pathway_data$pathway_level2,
                           pathway_data$size_sample),
                      function(p1, p2, s) {
                        set_species <- if (!is.na(p1)) {
                          preprocess_data %>%
                            filter(pathway_level1 == p1) %>% 
                            filter(pathway_level2 == p2)
                        } else {
                          preprocess_data %>%
                            filter(is.na(pathway_level1))
                        }
                        examples <- sample_n(set_species,
                                             s) %>% pull(species) 
                        data.frame(examples = str_c(examples, collapse = ", "),
                                   stringsAsFactors = FALSE) %>% 
                          as_tibble() %>%
                          mutate(pathway_level1 = p1,
                                 pathway_level2 = p2)
                      })
  
  # Join pathways and samples together
  pathway_data <- left_join(pathway_data, samples,
                            by = c("pathway_level1", "pathway_level2")) %>%
    select(-size_sample)
  
  # Create output table (untidy)
  pathway_data <- pathway_data %>%
    mutate(pathway_level1 = ifelse(!is.na(pathway_level1),
                                   pathway_level1, "unknown"),
           pathway_level2 = ifelse(pathway_level1 == "unknown",
                                   "",
                                   pathway_level2)) %>%
    mutate(examples = ifelse(!is.na(examples),
                             examples,
                             str_c(sample_n(preprocess_data %>% 
                                              filter(is.na(pathway_level1) |
                                                       is.na(pathway_level1)),
                                            n)$species,
                                   collapse = ", "))) %>%
    ungroup()
  
  pathway_data$pathway_level1[
    duplicated(pathway_data$pathway_level1) == TRUE] <- ""
  
  return(pathway_data)
}