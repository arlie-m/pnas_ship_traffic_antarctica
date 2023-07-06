#script and function for making Antarctic Peninsula/Scotia Arc/South America maps
types <- c("all",
           "fishing",
           "tourism",
           "research",
           "supply",
           "other")
library(rnaturalearth)
library(viridis)
ne_world <- ne_countries(scale = 10, returnclass = "sp")
world_sf <- st_as_sf(ne_world, coords = c("x", "y"))
scotia_box <-  c(xmin = -70, ymin = -70, xmax = -40, ymax = -60)
scotia_arc <- st_crop(world_sf,scotia_box)
make_scotia_maps <- function(list_of_networks){
  #create a subsetted graph to make appropriate new coordinates for nodes and labels
  port_networks_scotia_all <- list_of_networks[[1]] %>%
    activate(edges) %>% 
    convert(to_subgraph, to_province == "Scotia Sea" | from_realm == "Scotia Sea", subset_by = "edges") %>% 
    activate(nodes) %>% 
    mutate(y_nodes_alt = case_when(latitude > -60 ~ -60, 
                                   latitude < -70 ~ -70,
                                   TRUE ~ latitude),
           place_alt = case_when(latitude > -55 ~ paste('To', place, sep= " "),
                                 latitude < -70 ~ paste('To', place, sep= " "),
                                 TRUE ~ place),
           x_nodes_alt = case_when(longitude < -70 ~ -70,
                                   longitude > -40 ~ -40,
                                   TRUE ~ longitude)) %>%  
    as_tibble() %>% 
    dplyr::select(place, place_alt, y_nodes_alt, x_nodes_alt)
  #generate sf objects for both point (node) and label coordinates and transform them so that they can be added back to the node attributes for plotting
  nodes_port_network_scotia <- st_as_sf(port_networks_scotia_all, coords = c("x_nodes_alt", "y_nodes_alt"), remove = FALSE, crs = 4326)
  nodes_list_coords <- st_coordinates(nodes_port_network_scotia)
  nodes_port_network_scotia$x_node <- nodes_list_coords[,1]
  nodes_port_network_scotia$y_node <- nodes_list_coords[,2]
  #Create the subsetted graphs and maps for each type
  scotia_maps <- list()
  for( i in 1:length(list_of_networks)){
    ship_graph_scotia <- list_of_networks[[i]] %>% 
      activate(edges) %>% 
      convert(to_subgraph, to_province == "Scotia Sea" | from_province == "Scotia Sea", subset_by = "edges") %>% 
      activate(nodes) %>% 
      left_join(nodes_port_network_scotia, by = "place") %>%  
      mutate(isolated = node_is_isolated()) %>% 
      convert(to_subgraph, isolated == FALSE, subset_by = "nodes") %>% 
      mutate(plot_name = case_when(n_voyages > 250 & province == "Scotia Sea" ~ TRUE, TRUE ~ FALSE)) %>% 
      arrange(n_voyages)
    #create a new layout for the graph
    ship_graph_scotia$layout_scotia = cbind(V(ship_graph_scotia)$x_node,V(ship_graph_scotia)$y_node)
    #create the map
    ship_map_scotia <- ggraph(ship_graph_scotia, layout = ship_graph_scotia$layout_scotia) +
      geom_sf(data = scotia_arc,
              mapping = aes(),
              color = "seashell3",
              fill = "seashell2") +
      geom_edge_arc(aes(color = internal,
                       alpha = n_voyages),
                    arrow = arrow(length = unit(2, 'mm')),
                    strength = 0.1) + 
      geom_node_point(aes(color = n_voyages, size = n_voyages)
                      ) +
      #removed a previous attempt to have different colour scales for sites within and outside the Scotia Arc
      #geom_node_point(aes(filter = province != "Scotia Sea"),
                      #size = 0.4) +
      scale_edge_alpha('Number of voyages', range = c(0.3, 1), limits = c(1,80), n.breaks = 4) +
      scale_color_viridis(name = "Number of visits", 
                          #limits = c(0, 430)
                          ) +
      scale_edge_color_viridis(discrete = TRUE, name = NULL, option = "magma", 
                               begin = 0.3, end = 0.8, direction = -1) +
      theme_void() +
      theme(axis.title.x=element_blank(),
           axis.title.y=element_blank())
    scotia_maps[[i]] <- ship_map_scotia
  }
  names(scotia_maps) <- types
  return(scotia_maps)
}
