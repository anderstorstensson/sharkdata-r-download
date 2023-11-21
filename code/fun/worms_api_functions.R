library(worrms)
library(tidyverse)

update_worms_taxonomy <- function(aphiaid) {
  worms_class <- data.frame()
  for (ids in aphiaid) {
    tryCatch({
      worms_class_i <- wm_classification(ids) %>%
        select(-AphiaID) %>%
        mutate(scientific_name = last(scientificname)) %>%
        pivot_wider(names_from = rank, values_from = scientificname) %>%
        mutate(worms_hierarchy = paste(na.omit(.), collapse = " - "),
               aphia_id = ids) %>%
        mutate(Kingdom = ifelse("Kingdom" %in% names(.), Kingdom, NA),
               Phylum = ifelse("Phylum" %in% names(.), Phylum, NA),
               Class = ifelse("Class" %in% names(.), Class, NA),
               Order = ifelse("Order" %in% names(.), Order, NA),
               Family = ifelse("Family" %in% names(.), Family, NA),
               Genus = ifelse("Genus" %in% names(.), Genus, NA),
               Species = ifelse("Species" %in% names(.), Species, NA)) %>%
        rename(worms_kingdom = Kingdom,
               worms_phylum = Phylum,
               worms_class = Class,
               worms_order = Order,
               worms_family = Family,
               worms_genus = Genus,
               worms_species = Species)
    }, error=function(e) {
      worms_class_i <<- data.frame(aphia_id = ids)
    })
    worms_class <- bind_rows(worms_class, worms_class_i)
  }

  names <- c("aphia_id", "scientific_name", names(worms_class)[grepl("worms_", names(worms_class))])
  
  return(worms_class %>%
           select(any_of(names)) %>%
           relocate(worms_hierarchy, .after = last_col()))
}
