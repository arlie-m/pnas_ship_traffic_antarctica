#function to examine the ecoregions with strongest connections to and from Antarctica
types <- c("all",
           "fishing",
           "tourism",
           "research",
           "supply",
           "other")
gateway_region_importance <- function(list_of_eco_networks, output_nodes) {
  if (output_nodes) {
     gateway_regions <- list()
     for( i in 1:length(list_of_eco_networks)) {
       gateway_test <- list_of_eco_networks[[i]] %>% 
        activate(edges) %>% 
        filter(to_realm == "Southern Ocean" | from_realm == "Southern Ocean") %>% 
        activate(nodes) %>% 
        mutate(strength_out = centrality_degree(mode='out', weights = n_voyages, normalized = T)) %>%
        mutate(strength_in = centrality_degree(mode='in', weights = n_voyages, normalized = T)) %>%
        filter(strength_out > 0 | strength_in > 0)
       gateway_nodes <- gateway_test %>% 
        activate(nodes) %>% 
        arrange(desc(strength_out), desc(strength_in)) %>% 
        dplyr::select(ecoregion, province, realm, strength_out, strength_in, lat_zone, n_voyages,
                      n_ships, total_time, median_time, mean_time)
       gateway_regions[[i]] <- kable(gateway_nodes, format = "html") %>%
         kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                       full_width = F,
                       fixed_thead = TRUE,
                       font_size = 11) %>%
         scroll_box(height = "300px")
     }
     names(gateway_regions) <- types
     return(gateway_regions)
  } else {
    gateway_regions <- list()
    for( i in 1:length(list_of_eco_networks)) {
      gateway_test <- list_of_eco_networks[[i]] %>% 
        activate(edges) %>% 
        filter(to_realm == "Southern Ocean" | from_realm == "Southern Ocean") %>% 
        filter(n_voyages > 0)
      gateway_edges <- gateway_test %>% 
        activate(edges) %>% 
        arrange(desc(n_voyages))
      gateway_regions[[i]] <- kable(gateway_edges, format = "html") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                      full_width = F,
                      fixed_thead = TRUE,
                      font_size = 11) %>%
        scroll_box(height = "300px")
    }
    names(gateway_regions) <- types
    return(gateway_regions)
   }
  }

