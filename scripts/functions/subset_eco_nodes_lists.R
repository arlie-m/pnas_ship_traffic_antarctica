#function for creating node lists for activity types for ecoregion networks
activity_types <- c("fishing",
                    "tourism",
                    "research",
                    "supply",
                    "other")
subset_eco_node_lists <- function(attributes_list, ecoregion_list) {
  activity_eco_node_lists<-list()
  for(i in 1:length(attributes_list)){
    node_attributes <- attributes_list[[i]] %>% 
      filter(from_ecoregion == to_ecoregion) %>% 
      mutate(ecoregion = from_ecoregion) %>% 
      ungroup() %>% 
      dplyr::select(-from_ecoregion, -to_ecoregion)
    nodes_list <- ecoregion_list %>%
      left_join(node_attributes, by = "ecoregion") %>% 
      dplyr::select(-from, -to, -move, -from_realm, -to_realm)
    activity_eco_node_lists[[i]] <- nodes_list
  }
  names(activity_eco_node_lists) <- activity_types
  return(activity_eco_node_lists)
}
