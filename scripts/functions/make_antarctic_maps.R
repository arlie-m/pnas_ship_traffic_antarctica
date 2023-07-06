#function for making Antarctica maps, must have created the networks first, otherwise this won't work.
#create a list of types so that function outputs can be assigned to the appropriate group
types <- c("all",
           "fishing",
           "tourism",
           "research",
           "supply",
           "other")
#load natural earth package for data
library(rnaturalearth)
#load a map, transform it to a sf object, crop it to the desired bounding box
sf::sf_use_s2(FALSE)
world_sf <- ne_countries(country = "antarctica", scale = 50, returnclass = "sf")
antarctica <- st_as_sf(world_sf, coords = c("x", "y"))  
box <-  c(xmin = -180, ymin = -90, xmax = 180, ymax = -50)
antarctica <- st_crop(antarctica,box) 
antarctica <- sf::st_transform(antarctica, crs = 3031) 
new_limits <- st_multipoint(matrix(data = c(0, 90, 180, -90, -50,-46,-58, -46), ncol = 2), dim = XY)
new_limits <- st_sfc(new_limits, crs = 4326)
new_limits <- sf::st_transform(new_limits, crs = 3031)

#Now create a subgraph and add the new node attribute information to it
#this is where the function and loop will be so that I can do it to all subgraphs in one go, 
make_antarctic_maps <- function(list_of_networks) {
  #create a new set of longitudes for places outside the Southern Ocean - this is so that the labels and points do not overlap
  label_lon <-  list_of_networks[[1]] %>% 
    activate(edges) %>% 
    filter(to_realm == "Southern Ocean" | from_realm == "Southern Ocean") %>% 
    activate(nodes) %>% 
    mutate(strength_out = centrality_degree(mode='out', weights = n_voyages, normalized = T)) %>%
    mutate(strength_in = centrality_degree(mode='in', weights = n_voyages, normalized = T)) %>%
    mutate(strength_total = centrality_degree(mode='all', weights = n_voyages, normalized = T)) %>%
    filter(strength_total != 0) %>% 
    filter(realm != "Southern Ocean") %>%
    dplyr::select(place, longitude) %>% 
    as_tibble() %>%
    arrange(longitude) %>% 
    #change the label longitude to something approximately near the relevant locations and grouped in provinces that looks good on map
    mutate(label_lon = c(-150, -122, -30, -28, -114, -100, -98, -96, -88, -94, -92, -86, -84, -82, -80, -78, -76, -62, -60, -58, -74, -56, -72, -70, -68,
                         -54, -66, -52, -50, -48, -46, -44, -42, -40, -38, -36, -34, -16, -14, -10, -20, -2, 0, 2, 4, 6, 16, 8, 10, 18, 20, 24, 26, 12, 30, 
                         56, 82, 104, 106, 110, 112, 138, 140, 114, 118, 126, 120, 122, 128, 144, 148, 168, 170, 172, 174)) %>% 
    dplyr::select(place, label_lon)
  #Create some new attributes from the network with all the ships to make alternate node and label locations
  #and also create options for whether to display label or not in final map.
  port_networks_ant_all <- list_of_networks[[1]] %>% 
    activate(edges) %>% 
    convert(to_subgraph, to_realm == "Southern Ocean" | from_realm == "Southern Ocean", subset_by = "edges") %>% 
    activate(nodes) %>% 
    mutate(y_nodes_alt = case_when(latitude > -60 ~ -60, TRUE ~ latitude),
           place_alt = case_when(latitude > -60 ~ paste(place), TRUE ~ place),
           plot_name = case_when(latitude > -60 ~ TRUE, TRUE ~ FALSE),
           label_lat = case_when(latitude > -60 ~ -60, TRUE ~ latitude)) %>% 
    left_join(label_lon, by = "place") %>%  
    mutate(label_lon = case_when(is.na(label_lon) ~ longitude, TRUE ~ label_lon),
           x_nodes_alt = case_when(latitude > -60 ~ label_lon, TRUE ~ longitude)) %>% 
    as_tibble() %>% 
    dplyr::select(place, place_alt, plot_name,y_nodes_alt, x_nodes_alt, label_lon, label_lat)
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
  #Now that the relevant mapping attributes have been created, we can create the map, with a subgraph for each type
  for( i in 1:length(list_of_networks)) {
    ship_graph_ant <- list_of_networks[[i]] %>% 
      activate(edges) %>% 
      convert(to_subgraph, to_realm == "Southern Ocean" | from_realm == "Southern Ocean", subset_by = "edges") %>% 
      activate(nodes) %>% 
      left_join(nodes_port_network_ant, by = "place") %>%  
      mutate(isolated = node_is_isolated()) %>% 
      convert(to_subgraph, isolated == FALSE, subset_by = "nodes") %>% #only include nodes that are connected
      mutate(n_visits = n_voyages) %>% 
      arrange(n_visits)
    #create a new layout for the graph based on the new coordinates created above
    ship_graph_ant$layout_3031 = cbind(V(ship_graph_ant)$x_node,V(ship_graph_ant)$y_node)
    #make the map
    ship_map_ant <- ggraph(ship_graph_ant, layout = ship_graph_ant$layout_3031) +
      geom_sf(data = new_limits, 
              color = "white") +
      geom_sf(data = antarctica,
              mapping = aes(),
              color = "seashell3",
              fill = "seashell2") +
      geom_edge_arc(data = get_edges(collapse = 'all'),
                    aes(color = internal,
                        alpha = n_voyages),
                    strength = 0.1) + 
      geom_node_point(aes(color = n_visits,
                          size = centrality_eigen)) +
      #geom_node_point(aes(filter = plot_name == TRUE,
                         # color = n_visits),
                   #   size = 0.8) +
      scale_edge_alpha('Number of voyages', range = c(0.3, 1), limits = c(1, 170), n.breaks = 5) +
      scale_color_viridis(name = "Number of visits") +
      scale_size(name = "Eigenvector centrality") +
      scale_edge_color_viridis(discrete = TRUE, name = NULL, option = "magma", 
                               begin = 0.3, end = 0.8, direction = -1) +
      geom_node_text(aes(x = x_label, y = y_label,
                         filter = plot_name == TRUE, 
                         label = place_alt,
                         angle = ifelse(label_lon < 0, 270 - label_lon, 90 - label_lon),
                         hjust =  ifelse(label_lon < 0, 1.1, -0.1)),
                     size = 7/.pt) +
      theme_void() +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank())
    ship_maps_ant[[i]] <- ship_map_ant
  }
  names(ship_maps_ant) <- types
  return(ship_maps_ant)
}


