#function to check the activity intensity in Antarctic locations for different activity types
types <- c("all",
           "fishing",
           "tourism",
           "research",
           "supply",
           "other")
ant_risk <- function(list_of_networks) {
  antarctic_locations <- list()
  for( i in 1:length(list_of_networks)) {
    antarctic_nodes <- list_of_networks[[i]] %>% 
      activate(nodes) %>% 
      filter(realm == "Southern Ocean") %>%
      arrange(desc(n_voyages)) %>% 
      mutate(ave_time = duration(median_time, "seconds")) %>% 
      mutate(rank = row_number()) %>% 
      dplyr::select(rank, place, ecoregion, n_voyages, ave_time, n_ships) 
    antarctic_locations[[i]] <- kable(antarctic_nodes, format = "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = F,
                    fixed_thead = TRUE,
                    font_size = 11) %>%
      scroll_box(height = "300px")
  }
  names(antarctic_locations) <- types
  return(antarctic_locations)
}
