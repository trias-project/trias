#' Pathways of introduction
#'
#' @description Make a figure of pathways of introduction. Use
#'   \code{subcategory} option to take into account only main categories or
#'   include subcategories as well.
#'
#' @param data A data frame.
#' @param subcategory (boolean) If TRUE, the subcategory levels are included in
#'   the visualisation.
#'
#' @return A ggplot2 figure.
#'
#' @export
#'
#' @importFrom dplyr %>% filter group_by count ungroup mutate_at mutate
#' @importFrom ggplot2 ggplot geom_bar xlab ylab facet_grid theme element_text
#'   element_blank coord_flip
#' @importFrom stringr str_replace
#' @importFrom INBOtheme theme_inbo
#'
#' @examples
#' \dontrun{
#' indicator_pathway(data)
#' indicator_pathway(data, subcategory = TRUE)
#' }
indicator_pathway <- function(data, subcategory = TRUE) {
  
  # focus on pathway data counts
  df_pathway <- data  %>%
    filter(kingdom %in% c('Plantae', 'Animalia')) %>%
    group_by(pathway_level1, 
             pathway_level2, kingdom) %>%
    count() %>%
    ungroup()
  
  # uppercase on category names for figure styling
  df_pathway <- mutate_at(df_pathway, 
                          .vars = c("pathway_level1"), 
                          .funs = toupper)
  # replace underscore by spaces
  df_pathway <- df_pathway %>%
    mutate(pathway_level2 = str_replace(pathway_level2, "_", " "))
  
  if (subcategory) {
    ggplot(df_pathway, aes(x = reorder(pathway_level2, -n), 
                           y = n, fill = kingdom)) +
      geom_bar(stat = "identity", width = .65) +
      ylab("Number of species") +
      facet_grid(. ~ pathway_level1, scales = "free_x",
                 space = "free_x") +
      theme_inbo(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1,
                                   vjust = 0.2, size = 15),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 15, angle = 90,
                                   hjust = 0.5),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(angle = 90, size = 12),
        legend.direction = "horizontal",
        legend.position = "right",
        legend.text = element_text(angle = 90, size = 14),
        legend.title = element_blank(),
        legend.text.align = -.6)
  } else {
    ggplot(df_pathway,  aes(x = reorder(pathway_level1, n, function(x){sum(x)}),
                            y = n, fill = kingdom)) +
      geom_bar(stat = "summary", fun.y = sum,
               width = .65) + 
      xlab("Pathway") +
      ylab("Number of species") + 
      coord_flip() +
      theme_inbo(base_size = 14) +
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(size = 14))
  }
}
