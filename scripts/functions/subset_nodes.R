#function for creating node lists for activity types
activity_types <- c("fishing",
                    "tourism",
                    "research",
                    "supply",
                    "other")
subset_node_lists <- function(attributes_list, port_list) {
  activity_node_lists<-list()
  for(i in 1:length(attributes_list)){
    node_attributes <- attributes_list[[i]] %>% 
      filter(from_place == to_place) %>% 
      mutate(place = from_place) %>% 
      ungroup() %>% 
      dplyr::select(-from_place, -to_place, -from_ecoregion, -to_ecoregion)
    nodes_list <- port_list %>%
      left_join(node_attributes, by = "place") %>% 
      dplyr::select(-from, -to, -move, -from_realm, -to_realm)
    activity_node_lists[[i]] <- nodes_list
  }
  names(activity_node_lists) <- activity_types
  return(activity_node_lists)
}
