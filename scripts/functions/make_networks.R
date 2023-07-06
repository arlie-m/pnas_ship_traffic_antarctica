# This is a function that prepares the edge and nodes lists for networks and then makes the networks
# and calculates the metrics I'm interested in. The output is a list of network objects.
types <- c("all",
           "fishing",
           "tourism",
           "research",
           "supply",
           "other")
make_networks <- function(edges, nodes, nodes_are_ports) {
  if (nodes_are_ports) {
  port_networks <- list()
  for( i in 1:length(edges)) {
    edge_list_no_stays <- edges[[i]] %>% 
      filter(to != from) %>% 
      mutate(internal = case_when(from_realm == "Southern Ocean" & to_realm == "Southern Ocean" ~ "Within Antarctica",
                                  from_realm == "Southern Ocean" | to_realm == "Southern Ocean" ~ "To/from Antarctica",
                                  TRUE ~ "Outside Antarctica")) %>% 
      mutate(internal = case_when(from_place == "Grytviken" ~ "To/from Antarctica",
                                  to_place == "Grytviken" ~ "To/from Antarctica",
                                  TRUE ~ internal))
    edgelist_nodes <- tibble(place = edge_list_no_stays$from)
    edgelist_nodes_2 <- tibble(place = edge_list_no_stays$to)
    edgelist_nodes <- full_join(edgelist_nodes, edgelist_nodes_2) %>% 
      distinct()
    nodes_filtered <- semi_join(nodes[[i]], edgelist_nodes, by = "place")
    tmp_network <- tbl_graph(nodes= tibble(nodes_filtered), edges = edge_list_no_stays, directed = TRUE)
    tmp_network <- tmp_network %>%
      mutate(betweenness_centrality = centrality_betweenness(normalized = T)) %>% 
      mutate(closeness_centrality = centrality_closeness(normalized = T)) %>% 
      mutate(strength_out = centrality_degree(mode='out', weights = tmp_network$n_voyages, normalized = F)) %>% 
      mutate(strength_in = centrality_degree(mode='in', weights = tmp_network$n_voyages, normalized = F)) %>%
      mutate(strength_total = centrality_degree(mode='all', weights = tmp_network$n_voyages, normalized = F)) %>%
      mutate(centrality_eigen = centrality_eigen(weights = tmp_network$n_voyages, directed=T)) %>% 
      mutate(centrality_hub = centrality_hub(weights = tmp_network$n_voyages)) %>% 
      mutate(centrality_pagerank = centrality_pagerank(weights = tmp_network$n_voyages, directed=T)) %>% 
      mutate(clust_co = transitivity(graph = tmp_network, type = "local", weights = n_voyages))
    tmp_network$layout = cbind(V(tmp_network)$longitude,V(tmp_network)$latitude)
    port_networks[[i]] <- tmp_network
  }
  names(port_networks) <- types
  return(port_networks)
  } else {
    ecoregion_networks <- list()
    for( i in 1:length(edges)) {
      edge_list_no_stays <-edges[[i]] %>% 
        filter(to != from) %>% 
        mutate(internal = case_when(from_realm == "Southern Ocean" & to_realm == "Southern Ocean" ~ "Within Antarctica",
                                    from_realm == "Southern Ocean" | to_realm == "Southern Ocean" ~ "To or from Antarctica",
                                    TRUE ~ "Outside Antarctica"))
      edgelist_nodes <- tibble(ecoregion = edge_list_no_stays$from)
      edgelist_nodes_2 <- tibble(ecoregion = edge_list_no_stays$to)
      edgelist_nodes <- full_join(edgelist_nodes, edgelist_nodes_2) %>% 
        distinct()
      nodes_filtered <- semi_join(nodes[[i]], edgelist_nodes, by = "ecoregion") %>% 
        dplyr::select(ecoregion, everything())
      tmp_network <- tbl_graph(nodes = tibble(nodes_filtered),
                               edges = edge_list_no_stays,
                               directed = TRUE, 
                               node_key = "ecoregion")
      tmp_network <-  tmp_network %>%
        mutate(betweenness_centrality = centrality_betweenness(normalized = T)) %>% 
        mutate(closeness_centrality = centrality_closeness(normalized = T)) %>% 
        mutate(strength_out = centrality_degree(mode='out', weights = tmp_network$n_voyages, normalized = T)) %>% 
        mutate(strength_in = centrality_degree(mode='in', weights = tmp_network$n_voyages, normalized = T)) %>%
        mutate(strength_total = centrality_degree(mode='all', weights = n_voyages, normalized = F)) %>%
        mutate(centrality_eigen = centrality_eigen(weights = tmp_network$n_voyages, directed=T)) %>% 
        mutate(centrality_hub = centrality_hub(weights = tmp_network$n_voyages)) %>% 
        mutate(centrality_pagerank = centrality_pagerank(weights = tmp_network$n_voyages, directed=T))
      tmp_network$layout = cbind(V(tmp_network)$longitude,V(tmp_network)$latitude)
      ecoregion_networks[[i]] <- tmp_network
    }
    names(ecoregion_networks) <- types
    return(ecoregion_networks)
  }
}