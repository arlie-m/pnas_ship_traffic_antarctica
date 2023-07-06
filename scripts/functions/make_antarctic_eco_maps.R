#function for making ecoregion maps for Antarctica
types <- c("all",
           "fishing",
           "tourism",
           "research",
           "supply",
           "other")
library(rnaturalearth)
world_sf <- ne_countries(country = "antarctica", scale = 50, returnclass = "sp")
antarctica <- st_as_sf(world_sf, coords = c("x", "y"))
box <-  c(xmin = -180, ymin = -90, xmax = 180, ymax = -30)
antarctica <- st_crop(antarctica,box)
antarctica <- sf::st_transform(antarctica, crs = 3031)
new_limits <- st_multipoint(matrix(data = c(0, 90, 180, -90, -50,-50,-50, -50), ncol = 2), dim = XY)
new_limits <- st_sfc(new_limits, crs = 4326)
new_limits <- sf::st_transform(new_limits, crs = 3031)

make_antarctic_eco_maps <- function(list_of_networks, points_for_layout) {
  #create a new set of longitudes for places outside the Southern Ocean - this is so that the labels and points do not overlap
  label_lon <-  list_of_networks[[1]] %>% 
    activate(edges) %>% 
    filter(to_realm == "Southern Ocean" | from_realm == "Southern Ocean") %>% 
    activate(nodes) %>% 
    left_join(as_tibble(points_for_layout)) %>% 
    mutate(strength_out = centrality_degree(mode='out', weights = n_voyages, normalized = T)) %>%
    mutate(strength_in = centrality_degree(mode='in', weights = n_voyages, normalized = T)) %>%
    mutate(strength_total = centrality_degree(mode='all', weights = n_voyages, normalized = T)) %>%
    filter(strength_total != 0) %>% 
    filter(realm != "Southern Ocean") %>%
    dplyr::select(ecoregion, longitude) %>% 
    as_tibble() %>%
    arrange(longitude) %>% 
    mutate(label_lon = c(-15, -122, -107, -83, -81, -79, -77, -75, -73, -71, -65, -59, -57, -53, -51, -45, -39, -29, -23, -15, -8, -5, -2, 4, 6, 8, 10, 18, 22, 24, 29, 48, 60, 82, 100, 110, 114, 116, 118, 120, 122, 134, 136, 148, 151, 163, 168, 173, 177)) %>% 
    dplyr::select(ecoregion, label_lon)
  #Create some new attributes from the network with all the ships
  port_networks_ant_all <- list_of_networks[[1]] %>% 
    activate(edges) %>% 
    convert(to_subgraph, to_realm == "Southern Ocean" | from_realm == "Southern Ocean", subset_by = "edges") %>% 
    activate(nodes) %>% 
    left_join(as_tibble(points_for_layout)) %>% 
    mutate(y_nodes_alt = case_when(latitude > -60 ~ -55, TRUE ~ latitude),
           plot_name = case_when(latitude > -60 ~ TRUE, TRUE ~ FALSE),
           label_lat = case_when(latitude > -60 ~ -55, TRUE ~ latitude)) %>% 
    left_join(label_lon, by = "ecoregion") %>%  
    mutate(label_lon = case_when(is.na(label_lon) ~ longitude, TRUE ~ label_lon),
          x_nodes_alt = case_when(latitude > -60 ~ label_lon, TRUE ~ longitude)) %>% 
    as_tibble() %>% 
    dplyr::select(ecoregion, plot_name,y_nodes_alt, x_nodes_alt, label_lon, label_lat)
  #generate sf objects for both point (node) and label coordinates and transform them so that they can be added back to the node attributes for plotting
  nodes_port_network_ant <- st_as_sf(port_networks_ant_all, coords = c("x_nodes_alt", "y_nodes_alt"), remove = FALSE, crs = 4326) %>% 
    st_transform(crs = 3031)
  nodes_port_labels_ant <- st_as_sf(port_networks_ant_all, coords = c("label_lon", "label_lat"), remove = FALSE, crs = 4326) %>%
    st_transform(crs = 3031)
  nodes_list_coords <- st_coordinates(nodes_port_network_ant)
  nodes_port_network_ant$x_node <- nodes_list_coords[,1]
  nodes_port_network_ant$y_node <- nodes_list_coords[,2]
  nodes_label_coords <- st_coordinates(nodes_port_labels_ant)
  nodes_port_network_ant$x_label <- nodes_label_coords[,1]
  nodes_port_network_ant$y_label <- nodes_label_coords[,2]
  ship_maps_ant <- list()
  for( i in 1:length(list_of_networks)) {
    ship_graph_ant <- list_of_networks[[i]] %>% 
      activate(edges) %>% 
      convert(to_subgraph, to_realm == "Southern Ocean" | from_realm == "Southern Ocean", subset_by = "edges") %>% 
      activate(nodes) %>% 
      left_join(nodes_port_network_ant, by = "ecoregion") %>%  
      mutate(isolated = node_is_isolated()) %>% 
      convert(to_subgraph, isolated == FALSE, subset_by = "nodes") %>% 
      mutate(n_visits = case_when(plot_name == TRUE ~ 0, TRUE ~ n_voyages))
    #create a new layout for the graph
    ship_graph_ant$layout_3031 = cbind(V(ship_graph_ant)$x_node,V(ship_graph_ant)$y_node)
    ship_map_ant <- ggraph(ship_graph_ant, layout = ship_graph_ant$layout_3031) +
      geom_sf(data = new_limits, 
              color = "white") +
      geom_sf(data = antarctica,
              mapping = aes(),
              color = "seashell3",
              fill = "seashell2") +
      geom_edge_arc(aes(color = internal,
                        alpha = n_voyages),
                    arrow = arrow(length = unit(2, 'mm')),
                    strength = 0.3) + 
      geom_node_point(aes(filter = plot_name == FALSE,
                          color = n_visits),
                      size = 0.8) +
      geom_node_point(aes(filter = plot_name == TRUE),
                      size = 0.8) +
      scale_edge_alpha('Number of voyages', range = c(0.3, 1), limits = c(1, 1400), n.breaks = 7) +
      scale_color_viridis(name = "Number of visits", limits = c(1, 11000)) +
      scale_edge_color_viridis(discrete = TRUE, name = NULL, option = "magma", 
                               begin = 0.3, end = 0.8, direction = -1) +
      geom_node_text(aes(x = x_label, y = y_label,
                         filter = plot_name == TRUE, 
                         label = ecoregion,
                         angle = ifelse(label_lon < 0, 270 - label_lon, 90 - label_lon),
                         hjust =  ifelse(label_lon < 0, 1.1, -0.1)),
                     size = 2) +
      theme_minimal() +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank())
    ship_maps_ant[[i]] <- ship_map_ant
  }
  names(ship_maps_ant) <- types
  return(ship_maps_ant)
}