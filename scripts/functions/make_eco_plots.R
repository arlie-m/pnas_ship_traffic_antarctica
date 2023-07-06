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
make_eco_plots <- function(list_of_networks, points_for_layout) {
  world_eco_maps <- list()
  for( i in 1:length(list_of_networks)) {
    tmp_network <- list_of_networks[[i]] %>% 
      activate(nodes) %>% 
      left_join(as_tibble(points_for_layout)) %>% 
      mutate(mean_time = as.double(mean_time), total_time = as.double(total_time))
    tmp_network$layout = cbind(V(tmp_network)$x_node,V(tmp_network)$y_node)
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
      geom_node_point(aes(filter = !is.na(total_time),
                          color = total_time/86400,
                          size = centrality_eigen)) +
      scale_color_viridis(name = "Cumulative time spent (days)", limits = c(0, 16000)) +
      scale_edge_alpha_continuous(name = "Number of voyages", limits = c(1, 1600), n.breaks = 5) +
      scale_size(name = "Eigenvector Centrality", limits = c(0, 1), range = c(0.25, 4)) +
      theme_void()
    world_eco_maps[[i]] <- world_map
  }
  names(world_eco_maps) <- types
  return(world_eco_maps)
}

