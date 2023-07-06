#function to plot the world maps showing the different networks
types <- c("all",
           "fishing",
           "tourism",
           "research",
           "supply",
           "other")
world <- ne_countries() %>%
  st_as_sf() %>% 
  st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
ata_points <- data.frame(
  longitude = c(-180, 180, 180, 180, 180, 180, 180, 180, -180, -180, -180, -180, -180, -180),
  latitude = c(-60, -60, -65,-70, -75, -80, -85, -90, -90, -85, -80, -75, -70, -65),
  ata = "Antarctic Treaty Area"
)
ata_polygon <- ata_points %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
ata_polygon$label = "Antarctic Treaty Area"

make_plots <- function(list_of_networks) {
    port_network_all <- list_of_networks[[1]] %>% 
      activate(nodes) %>% 
      as_tibble() %>% 
      dplyr::select(place, latitude, longitude)
    nodes_port_network <- st_as_sf(port_network_all, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>% 
      st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
    nodes_list_coords <- st_coordinates(nodes_port_network)
    nodes_port_network$x_node <- nodes_list_coords[,1]
    nodes_port_network$y_node <- nodes_list_coords[,2]
  world_port_maps <- list()
  for( i in 1:length(list_of_networks)) {
    tmp_network <- list_of_networks[[i]] %>% 
      activate(nodes) %>% 
      arrange(n_voyages) %>% 
      left_join(nodes_port_network, by = "place")
    tmp_network$layout = cbind(V(tmp_network)$x_node,V(tmp_network)$y_node)
    #make the map
    world_map <- ggraph(tmp_network, layout = tmp_network$layout) + 
      geom_sf(data = world,
                   fill = "seashell2") +
      geom_sf(data = ata_polygon,
              fill = "lightsalmon",
              colour = "lightsalmon",
              alpha = 0.2) +
      annotate(geom = "label",
                   label = "Antarctic Treaty Area", 
                   colour = "black",
                   x = 0, y = -9800000,
                   size = 2.46) +
      geom_edge_link(aes(edge_alpha = n_voyages)) +
      geom_node_point(aes(color = n_voyages,
                          size = centrality_eigen)) +
      scale_color_viridis(name = "Number of visits", limits = c(1, 1140)) +
      scale_edge_alpha_continuous(name = "Number of voyages", limits = c(1, 170),  n.breaks = 5) +
      scale_size(name = "Eigenvector centrality") +
      theme_void()
    world_port_maps[[i]] <- world_map
  }
  names(world_port_maps) <- types
  return(world_port_maps)
}
  
  
  
  