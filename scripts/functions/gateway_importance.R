#function to check the importance of different gateway ports for different activity types
types <- c("all",
           "fishing",
           "tourism",
           "research",
           "supply",
           "other")
gateway_importance <- function(list_of_networks) {
  gateway_ports <- list()
  for( i in 1:length(list_of_networks)) {
    gateway_test <- list_of_networks[[i]] %>% 
      activate(edges) %>% 
      convert(to_subgraph, to_realm == "Southern Ocean", subset_by = "edges") %>% 
      activate(nodes) %>% 
      mutate(strength_out = centrality_degree(mode='out', weights = n_voyages, normalized = T)) %>% 
      filter(strength_out > 0)
    gateway_nodes <- gateway_test %>% 
      activate(nodes) %>% 
      filter(realm != "Southern Ocean") %>% 
      arrange(desc(strength_out)) %>% 
      dplyr::select(place, country, ecoregion, province, realm, strength_out, everything())
    gateway_ports[[i]] <- kable(gateway_nodes, format = "html", row.names = TRUE) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = F,
                    fixed_thead = TRUE,
                    font_size = 11,
                    row_label_position = "l") %>%
      scroll_box(height = "300px")
  }
  names(gateway_ports) <- types
  return(gateway_ports)
}