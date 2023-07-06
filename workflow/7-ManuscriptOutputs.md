7-ManuscriptOutputs
================
Arlie McCarthy
03/02/2021

# Set-up

``` r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.3     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidygraph)
```

    ## 
    ## Attaching package: 'tidygraph'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(ggraph)
library(nngeo)
```

    ## Loading required package: sf

    ## Linking to GEOS 3.8.1, GDAL 3.2.1, PROJ 7.2.1

``` r
library(raster)
```

    ## Loading required package: sp

    ## 
    ## Attaching package: 'sp'

    ## The following object is masked from 'package:ggraph':
    ## 
    ##     geometry

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:tidygraph':
    ## 
    ##     select

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(rgdal)
```

    ## rgdal: version: 1.5-23, (SVN revision 1121)
    ## Geospatial Data Abstraction Library extensions to R successfully loaded
    ## Loaded GDAL runtime: GDAL 3.2.1, released 2020/12/29
    ## Path to GDAL shared files: /Library/Frameworks/R.framework/Versions/4.1/Resources/library/rgdal/gdal
    ## GDAL binary built with GEOS: TRUE 
    ## Loaded PROJ runtime: Rel. 7.2.1, January 1st, 2021, [PJ_VERSION: 721]
    ## Path to PROJ shared files: /Library/Frameworks/R.framework/Versions/4.1/Resources/library/rgdal/proj
    ## PROJ CDN enabled: FALSE
    ## Linking to sp version:1.4-5
    ## To mute warnings of possible GDAL/OSR exportToProj4() degradation,
    ## use options("rgdal_show_exportToProj4_warnings"="none") before loading rgdal.
    ## Overwritten PROJ_LIB was /Library/Frameworks/R.framework/Versions/4.1/Resources/library/rgdal/proj

``` r
library(cowplot)
library(here)
```

    ## here() starts at /Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis

``` r
library(rnaturalearth)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following object is masked from 'package:raster':
    ## 
    ##     crosstab

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following object is masked from 'package:raster':
    ## 
    ##     union

    ## The following object is masked from 'package:tidygraph':
    ## 
    ##     groups

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:igraph':
    ## 
    ##     %--%, union

    ## The following object is masked from 'package:cowplot':
    ## 
    ##     stamp

    ## The following objects are masked from 'package:raster':
    ## 
    ##     intersect, union

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggrepel)

source(here("scripts", "functions", "make_networks.R"))
source(here("scripts", "functions", "make_world_plots.R"))
source(here("scripts", "functions", "make_eco_plots.R"))
source(here("scripts", "functions", "make_antarctic_maps.R"))
```

    ## Spherical geometry (s2) switched off

    ## although coordinates are longitude/latitude, st_intersection assumes that they are planar

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
source(here("scripts", "functions", "make_antarctic_eco_maps.R"))
```

    ## although coordinates are longitude/latitude, st_intersection assumes that they are planar

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
source(here("scripts", "functions", "antarctic_risk.R"))
```

Need to import all data for making networks AND make the networks

``` r
#Port network edge lists
edge_lists <- list(
  edge_list_all = read_csv(here("cleaned_data", "edge_list.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_fishing = read_csv(here("cleaned_data", "edge_list_fishing.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_tourism = read_csv(here("cleaned_data", "edge_list_tourism.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_research = read_csv(here("cleaned_data", "edge_list_research.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_supply = read_csv(here("cleaned_data", "edge_list_supply.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_other = read_csv(here("cleaned_data", "edge_list_other.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  )
)
nodes_lists <- list(
  nodes_list = read_csv(here("cleaned_data", "nodes_list.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_fishing = read_csv(here("cleaned_data", "nodes_list_fishing.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_tourism = read_csv(here("cleaned_data", "nodes_list_tourism.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_research = read_csv(here("cleaned_data", "nodes_list_research.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_supply = read_csv(here("cleaned_data", "nodes_list_supply.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_other = read_csv(here("cleaned_data", "nodes_list_other.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  )
)

#Ecoregion edge lists
edge_eco_lists <- list(
  edge_list_eco = read_csv(here("cleaned_data", "edge_list_eco.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_eco_fishing = read_csv(here("cleaned_data", "edge_list_eco_fishing.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_eco_tourism = read_csv(here("cleaned_data", "edge_list_eco_tourism.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_eco_research = read_csv(here("cleaned_data", "edge_list_eco_research.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_eco_supply = read_csv(here("cleaned_data", "edge_list_eco_supply.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  edge_list_eco_other = read_csv(here("cleaned_data", "edge_list_eco_other.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  )
)

#Ecoregion node lists
nodes_eco_lists <- list(
  nodes_list_ecoregion = read_csv(here("cleaned_data", "nodes_list_ecoregion.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_eco_fishing = read_csv(here("cleaned_data", "nodes_list_eco_fishing.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_eco_tourism = read_csv(here("cleaned_data", "nodes_list_eco_tourism.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_eco_research = read_csv(here("cleaned_data", "nodes_list_eco_research.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_eco_supply = read_csv(here("cleaned_data", "nodes_list_eco_supply.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  ),
  nodes_list_eco_other = read_csv(here("cleaned_data", "nodes_list_eco_other.csv"),
    col_types = cols(
      mean_time = col_number(),
      median_time = col_number(),
      total_time = col_number()
    )
  )
)

#Marine ecoregions of the world
MEOW <- st_read(here::here("data", "MEOW", "meow_ecos.shp")) %>%
  clean_names()
```

    ## Reading layer `meow_ecos' from data source 
    ##   `/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/data/MEOW/meow_ecos.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 232 features and 9 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -180 ymin: -89.9 xmax: 180 ymax: 86.9194
    ## Geodetic CRS:  WGS 84

``` r
#Filtered edgelist (not summarised)
edgelist_filtered <- read_csv(here("cleaned_data", "edgelist_filtered.csv"))
```

## Making the networks

I created a function (`make_networks`) that does the final preparation
for and creates directed networks using tidygraph for both the
port-to-port networks and ecoregion networks. The function also
calculates the centrality measures that will be used later on.

``` r
port_networks <- make_networks(edge_lists, nodes_lists, nodes_are_ports = TRUE)
ecoregion_networks <- make_networks(edge_eco_lists, nodes_eco_lists, nodes_are_ports = FALSE)
```

# Port Network Maps

Here I create and save two plots, a main one for the network with all
ships and a second one with maps for each activity type.

``` r
world_port_maps <- make_plots(list_of_networks = port_networks)
world_port_maps_all <- plot_grid(world_port_maps$all + theme(
  text = element_text(size = 7),
  legend.position = "bottom",
  legend.direction = "horizontal",
    legend.box = "horizontal") +
    guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
           edge_alpha = guide_legend(title.position = "top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5))
)
world_port_maps_all
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
save_plot(here("figures", "world_port_maps_all.pdf"), world_port_maps_all, base_width = 183, base_height = 110, units = "mm")
save_plot(here("figures", "world_port_maps_all.png"), world_port_maps_all, base_width = 183, base_height = 110, units = "mm")
```

``` r
left_plots <- align_plots(world_port_maps$fishing + theme(legend.position = "none"),
  world_port_maps$research + theme(legend.position = "none"),
  world_port_maps$other + theme(legend.position = "none"),
  align = "v", axis = "l"
)
top_row <- plot_grid(left_plots[[1]],
  world_port_maps$tourism + theme(legend.position = "none"),
  left_plots[[2]],
  world_port_maps$supply + theme(legend.position = "none"),
  nrow = 2,
  labels = c("a Fishing", "b Tourism", "c Research", "d Supply"),
  label_size = 8
)
legend <- get_legend(
  # create some space to the left of the legend
  world_port_maps$other + theme(
    legend.box.margin = margin(0, 0, 0, 12),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)
  )
)
bottom_row <- plot_grid(left_plots[[3]],
  legend,
  labels = c("e Other", NULL), nrow = 1, label_size = 8
)
world_port_maps_activities <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(2, 1))
world_port_maps_activities
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
save_plot(here("figures", "world_port_maps_activities.pdf"), world_port_maps_activities, base_width = 183, base_height = 170, units = "mm")
save_plot(here("figures", "world_port_maps_activities.png"), world_port_maps_activities, base_width = 183, base_height = 170, units = "mm")
```

# Ecoregion network maps

Make combined world ecoregion plots for the paper.

``` r
#Centroid points for the ecoregions
eco_points <- st_point_on_surface(MEOW) %>% 
  st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
eco_points_variables <- do.call(rbind, st_geometry(eco_points)) %>%
  as_tibble() %>%
  setNames(c("x_node", "y_node"))
eco_points <- eco_points %>%
  add_column(
    x_node = eco_points_variables$x_node,
    y_node = eco_points_variables$y_node
  )
eco_points$geometry <- NULL
#Creating the maps
world_eco_maps <- make_eco_plots(
  list_of_networks = ecoregion_networks,
  points_for_layout = eco_points)
```

Create the publication-ready figures.

``` r
left_plots <- align_plots(world_eco_maps$all + theme(legend.position = "none"),
  world_eco_maps$tourism + theme(legend.position = "none"),
  world_eco_maps$supply + theme(legend.position = "none"),
  align = "v", axis = "l"
)
top_row <- plot_grid(left_plots[[1]],
  world_eco_maps$fishing + theme(legend.position = "none"),
  left_plots[[2]],
  world_eco_maps$research + theme(legend.position = "none"),
  left_plots[[3]],
  world_eco_maps$other + theme(legend.position = "none"),
  nrow = 3,
  labels = c("a All ships", " b Fishing", "c Tourism", "d Research", "e Supply", "f Other"),
  label_size = 8
)
legend <- get_legend(
  world_eco_maps$other + theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)
  )
)
world_eco_maps_activities <- plot_grid(top_row, legend, ncol = 1, rel_heights = c(4, 1))
world_eco_maps_activities
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
save_plot(here("figures", "world_eco_maps_activities.pdf"), world_eco_maps_activities, base_width = 183, base_height = 160, units = "mm")
save_plot(here("figures", "world_eco_maps_activities.png"), world_eco_maps_activities, base_width = 183, base_height = 160, units = "mm")
```

# Antarctica port-network map

``` r
port_maps_ant <- make_antarctic_maps(list_of_networks = port_networks)

port_maps_ant_all <- plot_grid(port_maps_ant$all +
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.position = "bottom",#c(0.5, -0.05),
    legend.box = "horizontal",
    legend.direction = "vertical"
    ))
port_maps_ant_all
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggsave(
  filename = "port_map_ant_all.pdf", plot = port_maps_ant_all,
  path = here("figures"), width = 183, height = 192, units = "mm", device = "pdf"
)
ggsave(
  filename = "port_map_ant_all.png", plot = port_maps_ant_all,
  path = here("figures"), width = 183, height = 192, units = "mm", device = "png"
)
```

## Make combined Antarctica maps for activity types

These are not used in the paper, but consistent with other plots.

``` r
left_plots <- align_plots(port_maps_ant$fishing + theme(legend.position = "none"),
  port_maps_ant$research + theme(legend.position = "none"),
  port_maps_ant$other + theme(legend.position = "none"),
  align = "v", axis = "l"
)
top_row <- plot_grid(left_plots[[1]],
  port_maps_ant$tourism + theme(legend.position = "none"),
  left_plots[[2]],
  port_maps_ant$supply + theme(legend.position = "none"),
  nrow = 2,
  labels = c("Fishing", "Tourism", "Research", "Supply"),
  label_size = 10
)
legend <- get_legend(
  # create some space to the left of the legend
  port_maps_ant$other + theme(
    legend.direction = "vertical",
    legend.box = "horizontal"
  )
)
bottom_row <- plot_grid(left_plots[[3]],
  legend,
  labels = c("Other", NULL), nrow = 1, label_size = 10
)
port_maps_ant_activities <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(2, 1))
save_plot(here("figures", "port_maps_ant_activities.pdf"), port_maps_ant_activities, base_width = 7.204, base_height = 7)
save_plot(here("figures", "port_maps_ant_activities.png"), port_maps_ant_activities, base_width = 7.204, base_height = 7)
```

# Identifying and Mapping the highest risk areas around Antarctica

Here I create a list of the ‘highest risk’ areas. Down the line, I
combine this with information on sea ice, temperature etc that can be
put into a table for the paper.

The ‘highest risk’ sites are selected by ranking Antarctic locations for
number of visits, number of ships and time stopped. A location’s final
rank is determined by multiplying the ranks together and arranging
lowest to highest.

``` r
antarctic_locations <- port_networks$all %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  filter(realm == "Southern Ocean") %>% 
  arrange(desc(n_voyages)) %>% 
  mutate(rank_voyages = row_number()) %>% 
  arrange(desc(n_ships)) %>% 
  mutate(rank_ships = row_number()) %>% 
  arrange(desc(median_time)) %>% 
  mutate(rank_time = row_number()) %>% 
  mutate(rank_score = (rank_voyages + rank_ships + rank_time)/3) %>% 
  arrange(rank_score) %>% 
  mutate(rank = row_number())


antarctic_locations %>% group_by(ecoregion) %>% arrange(rank) %>% slice(1) %>% dplyr::select(ecoregion, place, rank, everything())
```

    ## # A tibble: 10 × 31
    ## # Groups:   ecoregion [10]
    ##    ecoregion place  rank country area  latitude longitude feature province realm
    ##    <chr>     <chr> <int> <chr>   <chr>    <dbl>     <dbl> <chr>   <chr>    <chr>
    ##  1 Amundsen… Cape…   161 Southe… Anta…    -73.9    -116.  Cape    Contine… Sout…
    ##  2 Antarcti… Kris…     3 Southe… Anta…    -64.8     -64.1 Cove    Scotia … Sout…
    ##  3 East Ant… Indi…    74 Southe… Anta…    -70.0      12.0 Bay     Contine… Sout…
    ##  4 East Ant… Alas…   225 Southe… Anta…    -67.5      45.7 Bight   Contine… Sout…
    ##  5 East Ant… Long…    22 Southe… Anta…    -69.4      76.1 Reef    Contine… Sout…
    ##  6 Peter th… Sels…   261 Southe… Anta…    -68.8     -90.7 <NA>    Subanta… Sout…
    ##  7 Ross Sea  Mari…    19 Southe… Anta…    -74.7     164.  Station Contine… Sout…
    ##  8 South Or… Berr…     7 Southe… Anta…    -60.7     -45.6 Head    Scotia … Sout…
    ##  9 South Sh… Dovi…     1 Southe… Anta…    -62.4     -59.7 Rock    Scotia … Sout…
    ## 10 Weddell … Pola…    92 Southe… Anta…    -70.3      -2.8 <NA>    Contine… Sout…
    ## # … with 21 more variables: from_province <chr>, to_province <chr>,
    ## #   n_voyages <dbl>, n_ships <dbl>, n_trips <dbl>, total_time <dbl>,
    ## #   median_time <dbl>, mean_time <dbl>, n_time <dbl>,
    ## #   betweenness_centrality <dbl>, closeness_centrality <dbl>,
    ## #   strength_out <dbl>, strength_in <dbl>, strength_total <dbl>,
    ## #   centrality_eigen <dbl>, centrality_hub <dbl>, centrality_pagerank <dbl>,
    ## #   rank_voyages <int>, rank_ships <int>, rank_time <int>, rank_score <dbl>

``` r
antarctic_locations %>% filter(place == "Anchorage Island" | place == "Arrival Heights" | place == "Atka Bank" | place == "Cuvier Island") %>% dplyr::select(ecoregion, place, rank, everything())
```

    ## # A tibble: 4 × 31
    ##   ecoregion  place  rank country area  latitude longitude feature province realm
    ##   <chr>      <chr> <int> <chr>   <chr>    <dbl>     <dbl> <chr>   <chr>    <chr>
    ## 1 Ross Sea   Arri…    31 Southe… Anta…    -77.8     167.  Heights Contine… Sout…
    ## 2 East Anta… Anch…    55 Southe… Anta…    -68.6      77.9 Island  Contine… Sout…
    ## 3 East Anta… Cuvi…    87 Southe… Anta…    -66.6     140.  Island  Contine… Sout…
    ## 4 Weddell S… Atka…   218 Southe… Anta…    -70.5      -9   Bank    Contine… Sout…
    ## # … with 21 more variables: from_province <chr>, to_province <chr>,
    ## #   n_voyages <dbl>, n_ships <dbl>, n_trips <dbl>, total_time <dbl>,
    ## #   median_time <dbl>, mean_time <dbl>, n_time <dbl>,
    ## #   betweenness_centrality <dbl>, closeness_centrality <dbl>,
    ## #   strength_out <dbl>, strength_in <dbl>, strength_total <dbl>,
    ## #   centrality_eigen <dbl>, centrality_hub <dbl>, centrality_pagerank <dbl>,
    ## #   rank_voyages <int>, rank_ships <int>, rank_time <int>, rank_score <dbl>

``` r
top_locations <- antarctic_locations %>% 
  arrange(rank) %>% 
  head(20) %>% 
  dplyr::select(rank, place, n_voyages, n_ships, median_time)
table_24_rank <- antarctic_locations %>%
  filter(place == "Anchorage Island" | 
         place == "Arrival Heights" | 
         place == "Atka Bank" |
         place == "Cuvier Island") %>%
  dplyr::select(rank, place, n_voyages, n_ships, median_time) %>% 
  full_join(top_locations) %>% 
  arrange(rank) %>% 
  mutate(median_time_hours = median_time/3600)
```

## Mapping the high risk locations and ecoregions to show relative traffic.

I also create the elements to form the base of the map that I will plot
these high risk locations over.

``` r
# make sure the relevant locations, bounding box etc are present
ne_world <- ne_countries(scale = 10, returnclass = "sp")
world_sf <- st_as_sf(ne_world, coords = c("x", "y"))
box <- c(xmin = -70, ymin = -68, xmax = -45, ymax = -60)
scotia_arc <- st_crop(world_sf, box)
```

Now I subset the graph so that it only includes my selected locations.

``` r
ship_graph_ant <- port_networks$all %>%
  activate(nodes) %>%
  convert(to_subgraph, place %in% top_locations$place, subset_by = "nodes") %>%
  arrange(n_voyages) %>%
  mutate(place = case_when(place == "Buen Tiempo, Rada" ~ "Deception Island", TRUE ~ place)) %>% 
  mutate(rank = case_when(
    place == "Dovizio Rock" ~ 1,
    place == "Maxwell Bay" ~ 2,
    place == "Kristie Cove" ~ 3,
    place == "British Point" ~ 6,
    place == "Deception Island" ~ 5,
    place == "Berry Head" ~ 7,
    place == "Gloria, Punta" ~ 4,
    place == "South Bay" ~ 8,
    place == "Kerr Point" ~ 10,
    place == "Potter Cove" ~ 9,
    place == "Andvord Bay" ~ 20,
    place == "Walker Bay" ~ 13,
    place == "Cheshire Island" ~ 11,
    place == "Mario Zucchelli Station" ~ 19,
    place == "Point Thomas" ~ 14,
    place == "Theta Islands" ~ 12,
    place == "Coughtrey Peninsula" ~ 15,
    place == "Bombay Island" ~ 16,
    place == "Girardi, islote" ~ 18,
    place == "Argentine Islands" ~ 17,
  ))
# create a new layout for the graph
ship_graph_ant$layout_scotia <- cbind(V(ship_graph_ant %>% filter(place != "Mario Zucchelli Station"))$longitude, V(ship_graph_ant %>% filter(place != "Mario Zucchelli Station"))$latitude)
```

Now I create a map of these locations with some relevant attributes and
save it as a preliminary output. Because the map is only of the Scotia
Arc and Mario Zuchelli Station is in the Ross Sea I remove it from the
map.

``` r
ant_risk_map <- ggraph(ship_graph_ant %>% filter(place != "Mario Zucchelli Station"), layout = ship_graph_ant$layout_scotia) +
  geom_sf(
    data = scotia_arc,
    mapping = aes(),
    color = "seashell3",
    fill = "seashell2"
  ) +
  geom_node_label(aes(label = rank),
    repel = TRUE,
    size = 2.46,
  ) +
  geom_node_point(aes(
    size = n_voyages,
    color = mean_time / 86400
  )) +
  annotate(geom = "label",
           label = "Location rank\n (1 = highest introduction risk)", 
           x = -50, y = -67,
           size = 2.46) +
  annotate(geom = "text",
           label = "Antarctic Peninsula", 
           x = -64, y = -66.8,
           size = 2.46,
           angle = '45') +
  annotate(geom = "text",
           label = "South Shetland Islands", 
           x = -65, y = -62.4,
           size = 2.46) +
  annotate(geom = "text",
           label = "South Orkney Islands", 
           x = -50, y = -60,
           size = 2.46) +
  annotate(geom = "text",
           label = "Anvers Island", 
           x = -67, y = -64.2,
           size = 2.46) +
  annotate(geom = "label",
           label = "1, 2, 5, 8, 9, 13, 14, 18", 
           x = -53, y = -62.5,
           size = 2.46) +
  annotate(geom = "label",
          label = "3, 4, 10, 12, 15 20", 
         x = -58, y = -65.2,
         size = 2.46) +
  scale_color_viridis(name = "Mean time at location (days)") +
  scale_size(name = "Number of visits") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
ant_risk_map
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggsave(
  filename = "ant_risk_map.pdf", plot = ant_risk_map,
  path = here("outputs"), width = 10, height = 8, device = "pdf"
)
```

Now, for the companion Ecoregions plot showing size and colour for
visits and time spent.

``` r
# create the basemap and bounding box
world_sf <- ne_countries(country = "antarctica", scale = 50, returnclass = "sp")
antarctica <- st_as_sf(world_sf, coords = c("x", "y"))
box_close <- c(xmin = -180, ymin = -90, xmax = 180, ymax = -55)
antarctica_close <- st_crop(antarctica, box_close)
antarctica_close <- sf::st_transform(antarctica_close, crs = 3031)
new_limits_close <- st_multipoint(matrix(data = c(0, 90, 180, -90, -60, -60, -60, -60), ncol = 2), dim = XY)
new_limits_close <- st_sfc(new_limits_close, crs = 4326)
new_limits_close <- sf::st_transform(new_limits_close, crs = 3031)

# make sure coordinates for plotting are the correct crs
nodes_map_eco_ant <- st_as_sf(eco_points, coords = c("x_node", "y_node"), remove = FALSE, crs = 4326) %>%
  st_transform(crs = 3031)
nodes_list_coords_eco_ant <- st_coordinates(nodes_map_eco_ant)
nodes_map_eco_ant$x_node <- nodes_list_coords_eco_ant[, 1]
nodes_map_eco_ant$y_node <- nodes_list_coords_eco_ant[, 2]
nodes_map_eco_ant <- nodes_map_eco_ant %>%
  st_drop_geometry()

# include information for adding Mario Zucchelli Station
ship_graph_mzs <- ship_graph_ant %>%
  activate(nodes) %>%
  convert(to_subgraph, place == "Mario Zucchelli Station", subset_by = "nodes") %>%
  as_tibble() %>% 
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>% 
  st_transform(crs = 3031)

# subset the graph
ship_graph_eco_ant <- ecoregion_networks$all %>%
  activate(nodes) %>%
  convert(to_subgraph, realm == "Southern Ocean", subset_by = "nodes") %>%
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  filter(ecoregion != "South Georgia", ecoregion != "South Sandwich Islands") %>% 
  left_join(MEOW) %>% 
  mutate(eco_abbrev = c("A/BS", "AP", "EAEL", "EAWL", "EADML", "PFI", "RS", "SOI", "SSI", "WS")) %>% 
  st_as_sf() 

eco_ant_map <- ggplot() +
  geom_sf(data = antarctica_close,
          mapping = aes()) +
  geom_sf(
    data = ship_graph_eco_ant,
    mapping = aes(
      fill = mean_time / 86400),
    colour = "slategrey",
  ) +
  geom_sf(
    data = antarctica_close,
    mapping = aes(),
    color = "seashell3",
    fill = "seashell2"
  ) +
  geom_sf_label(data = ship_graph_eco_ant,
                mapping = aes(label = n_voyages),
                show.legend = TRUE,
                size = 2.46
                ) +
  geom_sf_text(data = ship_graph_eco_ant,
               mapping = aes(label = eco_abbrev),
               nudge_x = -650000,
               nudge_y = 50000,
               size = 2.46) +
  annotate(geom = "label",
           label = "Number of visits", 
           x = 1139998.98, y = -3132121.45,
           size = 2.46) +
  geom_sf(data = ship_graph_mzs,
             mapping = aes(),
               size = 2.46) +
  annotate(geom = "text",
           label = "Mario Zucchelli Station\n (rank 19)", 
           x = 1839998.98, y = -1732121.45,
           size = 2.46) +
  scale_fill_viridis(name = "Mean time in ecoregion (days)", guide = "colorbar") +
  scale_size(guide = NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
eco_ant_map
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Combine the two plots for the paper and save them as pdf and png
versions that comply with journal requirements.

``` r
ant_risk_eco_map <- plot_grid(
  ant_risk_map + theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7)) +
      guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
             size = guide_legend(title.position="top", title.hjust = 0.5)),
  eco_ant_map + theme(
      legend.position = c(0.25, 0),
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7)) +
      guides(fill = guide_colourbar(title = "Mean time in ecoregion (days)",
                                      title.position="top",
                                      title.hjust = 0.5)),
  ncol = 2,
  labels = c(
      "a Locations with high introduction risk",
      "b Antarctic Ecoregions"),
  label_size = 8,
  align = "v", 
  hjust = -0.1
)
ant_risk_eco_map
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
save_plot(here("figures", "ant_risk_eco_map.pdf"), ant_risk_eco_map, base_width = 183, base_height = 120, units = "mm")
save_plot(here("figures", "ant_risk_eco_map.png"), ant_risk_eco_map, base_width = 183, base_height = 120, units = "mm")
```

## Additional information for table

Now I need to access additional information to include in my table. The
‘Top 20’ locations identified above will go into a table with some
detailed information and I will also include 4 locations from other
Antarctic regions for comparison. I will choose the most-visited
locations in my network that are near: a) Mawson Station  
b) Neumayer Station c) Dumont Durville  
d) Scott/McMurdo

To do that I need to import the COMNAP station catalogue and map it
against my Antarctic locations.

``` r
comnap_stations <- read_csv(here("data",
                                 "comnap-antarctic-facilities-3.0.1",
                                 "dist",
                                 "csv",
                                 "COMNAP_Antarctic_Facilities_Master.csv"),
  col_types = cols(
    X22 = col_skip(), X23 = col_skip(),
    X24 = col_skip(), X25 = col_skip(),
    X26 = col_skip(), X27 = col_skip()
  )
)
comnap_stations_sf <- st_as_sf(comnap_stations,
  stringsAsFactors = TRUE,
  coords = c("lon_dd", "lat_dd")
)
st_crs(comnap_stations_sf) <- 4326
comnap_stations_3031 <- st_transform(comnap_stations_sf, crs = 3031)

target_stations <- c("Davis", "Neumayer III", "Dumont d'Urville", "McMurdo")
comnap_stations_labels <- comnap_stations_3031 %>% 
  filter(name_eng %in% target_stations)
```

Adjust graph object for plotting just locations

``` r
#bounding box use 'antarctica_close' made previously
#subset network graph
ship_graph_ant <- port_networks$all %>%
  activate(nodes) %>%
  convert(to_subgraph, realm == "Southern Ocean", subset_by = "nodes") %>%
  arrange(n_voyages) %>%
  mutate(place = case_when(place == "Buen Tiempo, Rada" ~ "Deception Island", TRUE ~ place)) %>% 
  as_tibble() %>% 
  st_as_sf(stringsAsFactors = TRUE,
  coords = c("longitude", "latitude"), 
  remove = FALSE,
  crs = 4326) %>% 
  st_transform(crs = 3031)
```

Now I map both the stations and locations

``` r
stations_locations_map <- ggplot() +
  geom_sf(data = antarctica_close,
          mapping = aes()) +
  geom_sf(
    data = ship_graph_ant,
    colour = "slategrey",
  ) +
  geom_sf(
    data = comnap_stations_3031,
    color = "coral"
  ) +
  geom_sf_text(data = comnap_stations_labels,
    aes(label = name_eng)
  )
stations_locations_map
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
Locations close to target stations

``` r
extra_locs <- st_nn(comnap_stations_labels, ship_graph_ant, maxdist = 35000, returnDist = TRUE, k = 3)
comnap_stations_labels$close_loc <- extra_locs[["nn"]]
comnap_stations_labels$dist_loc <- extra_locs[["dist"]]
locs_to_compare <- ship_graph_ant[c(188, 163, 16, 17, 109, 233, 107, 225),]
locs_to_keep <- c("Anchorage Island", "Cuvier Island", "Atka Bank", "Arrival Heights")
table_locations <- c(top_locations$place, locs_to_keep)
```

From the above, I have identified 4 locations that are close to the
stations of interest:

1.  “Anchorage Island” (2.2km from Davis)  
2.  “Cuvier Island” (1.6km from Dumont d’Urville)  
3.  “Atka Bank” (33km from Neumayer III)
4.  “Arrival Heights” (3.5 from McMurdo)

### Visit trends

First, I assess whether the number of visits to my locations is
increasing, decreasing or staying the same. I do this with a cumulative
average for visits from all vessel types.

``` r
edgelist_table_24 <- edgelist_filtered %>%
  filter(from_place %in% table_locations | to_place %in% table_locations) %>%
  arrange(move) %>%
  mutate(year = year(from_tms)) %>%
  group_by(move, year, from_place, to_place, from_ecoregion, to_ecoregion, from_province, to_province, from_realm, to_realm) %>%
  summarise(n_voyages = as.numeric(n()), n_ships = n_distinct(vessel_id), n_trips = n_distinct(trip_id.x)) %>%
  mutate(from = from_place, to = to_place) %>%
  filter(from == to) %>%
  dplyr::select(from, to, everything()) %>%
  arrange(from, year) %>%
  ungroup() %>%
  mutate(
    year1 = case_when(
      year == 2014 ~ n_voyages,
      year == 2015 ~ lag(n_voyages),
      year == 2016 ~ lag(n_voyages, n = 2),
      year == 2017 ~ lag(n_voyages, n = 3),
      year == 2018 ~ lag(n_voyages, n = 4)
    ),
    year2 = case_when(
      year == 2014 ~ lead(n_voyages),
      year == 2015 ~ n_voyages,
      year == 2016 ~ lag(n_voyages),
      year == 2017 ~ lag(n_voyages, n = 2),
      year == 2018 ~ lag(n_voyages, n = 3)
    ),
    year3 = case_when(
      year == 2014 ~ lead(n_voyages, n = 2),
      year == 2015 ~ lead(n_voyages),
      year == 2016 ~ n_voyages,
      year == 2017 ~ lag(n_voyages),
      year == 2018 ~ lag(n_voyages, n = 2)
    ),
    year4 = case_when(
      year == 2014 ~ lead(n_voyages, n = 3),
      year == 2015 ~ lead(n_voyages, n = 2),
      year == 2016 ~ lead(n_voyages),
      year == 2017 ~ n_voyages,
      year == 2018 ~ lag(n_voyages)
    ),
    year5 = case_when(
      year == 2014 ~ lead(n_voyages, n = 4),
      year == 2015 ~ lead(n_voyages, n = 3),
      year == 2016 ~ lead(n_voyages, n = 2),
      year == 2017 ~ lead(n_voyages),
      year == 2018 ~ n_voyages
    )
  ) %>%
  group_by(from) %>%
  mutate(ave = case_when(
    year == 2014 ~ year1,
    year == 2015 ~ mean(c(year1, year2)),
    year == 2016 ~ mean(c(year1, year2, year3)),
    year == 2017 ~ mean(c(year1, year2, year3, year4)),
    year == 2018 ~ mean(c(year1, year2, year3, year4, year5)),
  )) %>%
  mutate(from = case_when(from == "Buen Tiempo, Rada" ~ "Deception Island", TRUE ~ from))
```

The cumulative average and yearly counts are generated for each of the
20 locations and plot saved for inclusion in manuscript.

``` r
visits_per_year <- ggplot(data = edgelist_table_24) +
  geom_bar(
    mapping = aes(
      x = year,
      y = n_voyages,
      alpha = 0.5
    ),
    stat = "identity",
    fill = "lightslategrey"
  ) +
  geom_path(
    mapping = aes(
      x = year,
      y = ave
    ),
    color = "cyan4"
  ) +
  facet_wrap(. ~ from, nrow = 5) +
  labs(
    y = "Number of visits",
    x = "Year"
  ) +
  guides(colour = "none", alpha = "none") +
  theme_bw() +
  theme(text = element_text(size = 8))
visits_per_year
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
save_plot(here("figures", "top_locs_visits_per_year.pdf"), visits_per_year, base_width = 183, base_height = 120, units = "mm")
save_plot(here("figures", "top_locs_visits_per_year.png"), visits_per_year, base_width = 183, base_height = 120, units = "mm")
```

### Extra data on temperature, sea-ice and nearby structures

Data required:  
- point information for the 20 top locations - spatially explicit winter
temperature readings for locations around Antarctica (from NOAA’s World
Ocean Atlas) - spatially explicit data for COMNAP facilities - number of
ice free days per year around Antarctica.

For each of these I assign the nearest available measure to each
location, except for the facilities which I generate a distance to the
nearest jetty or wharf.

#### Generate sf object for 24 table locations

``` r
table_24_sf <- port_networks$all %>%
  filter(place %in% table_locations) %>%
  as_tibble() %>% 
  mutate(loc_type = case_when(place %in% locs_to_keep == TRUE ~ "comparison", TRUE ~ "top_20"))
table_24_sf <- st_as_sf(table_24_sf,
  stringsAsFactors = TRUE,
  coords = c("longitude", "latitude")
)
st_crs(table_24_sf) <- 4326
```

#### Import Temperature Data

``` r
temp_data <- read_csv(here("data", "NOAA", "woa18_A5B7_t15an04.csv"), skip = 1) %>%
  clean_names()
temp_data <- temp_data %>%
  rename(
    latitude = number_comma_separated_latitude,
    x0 = and_values_at_depths_m_0
  ) %>%
  dplyr::select(latitude, longitude, everything())
temp_sf <- st_as_sf(temp_data,
  stringsAsFactors = TRUE,
  coords = c("longitude", "latitude")
)
st_crs(temp_sf) <- 4326
temp_sf_scotia <- st_crop(temp_sf, box)

box_close_ant <- st_sfc(
  st_multipoint(
    matrix(
      data = c(0, 90, 180, -90, -55,-55,-55, -55),
      ncol = 2),
    dim = "XY"),
  crs = 4326) %>% 
  st_transform(crs = 3031)

temp_sf_ant <-  st_transform(temp_sf,crs =  3031) %>% 
  st_crop(box_close_ant)
```

#### Import COMNAP facilities data

I adjusted the csv file from COMNAP to include whether and what kind of
ship landing facilities were at each station. The information came from
the COMANP stations catalogue. For this section I am only interested in
human structures, i.e. jetties and wharves, so I remove all others.

``` r
comnap_stations_sf <- st_as_sf(comnap_stations,
  stringsAsFactors = TRUE,
  coords = c("lon_dd", "lat_dd")
)
st_crs(comnap_stations_sf) <- 4326
comnap_stations_sf_scotia <- st_crop(comnap_stations_sf, box) %>%
  filter(!is.na(ship_landing_facilities) &
           ship_landing_facilities != "None" &
           ship_landing_facilities != "Ice pier" &
           ship_landing_facilities != "Ice shelf")
```

First, a quick plot the data to make sure that I have them all the data
I expect.

``` r
ggplot() +
  geom_sf(
    data = antarctica_close,
    mapping = aes(),
    color = "seashell3",
    fill = "seashell2"
  ) +
  geom_sf(
    data = temp_sf_ant,
    mapping = aes(color = x0)
  ) +
  geom_sf(
    data = comnap_stations_sf,
    mapping = aes(),
    color = "red",
    shape = 17
  ) +
  geom_sf(
    data = table_24_sf,
    mapping = aes(),
    color = "orange"
  ) +
  scale_color_gradient(name = "Mean Winter Temperature")
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->
Great! so it appears I have mean winter temperature, station location
and my location data mapping correctly over each other. Now I need to
find the distance to the nearest pier/jetty and extract the temperature
for the closest points

### Nearest temperatures and structures

``` r
close_piers <- st_nn(table_24_sf, comnap_stations_sf, maxdist = 10000, returnDist = TRUE, k = 1)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |===                                                                   |   4%  |                                                                              |======                                                                |   8%  |                                                                              |=========                                                             |  12%  |                                                                              |============                                                          |  17%  |                                                                              |===============                                                       |  21%  |                                                                              |==================                                                    |  25%  |                                                                              |====================                                                  |  29%  |                                                                              |=======================                                               |  33%  |                                                                              |==========================                                            |  38%  |                                                                              |=============================                                         |  42%  |                                                                              |================================                                      |  46%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================                                |  54%  |                                                                              |=========================================                             |  58%  |                                                                              |============================================                          |  62%  |                                                                              |===============================================                       |  67%  |                                                                              |==================================================                    |  71%  |                                                                              |====================================================                  |  75%  |                                                                              |=======================================================               |  79%  |                                                                              |==========================================================            |  83%  |                                                                              |=============================================================         |  88%  |                                                                              |================================================================      |  92%  |                                                                              |===================================================================   |  96%  |                                                                              |======================================================================| 100%

``` r
table_24_sf$close_jetty <- close_piers[["nn"]]
table_24_sf$dist_jetty <- close_piers[["dist"]]
nearest_temperature <- st_nn(table_24_sf, temp_sf, k = 1)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |===                                                                   |   4%  |                                                                              |======                                                                |   8%  |                                                                              |=========                                                             |  12%  |                                                                              |============                                                          |  17%  |                                                                              |===============                                                       |  21%  |                                                                              |==================                                                    |  25%  |                                                                              |====================                                                  |  29%  |                                                                              |=======================                                               |  33%  |                                                                              |==========================                                            |  38%  |                                                                              |=============================                                         |  42%  |                                                                              |================================                                      |  46%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================                                |  54%  |                                                                              |=========================================                             |  58%  |                                                                              |============================================                          |  62%  |                                                                              |===============================================                       |  67%  |                                                                              |==================================================                    |  71%  |                                                                              |====================================================                  |  75%  |                                                                              |=======================================================               |  79%  |                                                                              |==========================================================            |  83%  |                                                                              |=============================================================         |  88%  |                                                                              |================================================================      |  92%  |                                                                              |===================================================================   |  96%  |                                                                              |======================================================================| 100%

``` r
table_24_sf$rowid <- as.numeric(nearest_temperature)
table_24_temp <- temp_sf %>%
  rowid_to_column() %>%
  filter(rowid %in% nearest_temperature) %>%
  dplyr::select(rowid, x0) %>%
  st_drop_geometry()
table_24 <- merge(table_24_sf, table_24_temp) %>%
  dplyr::select(place, rowid, x0, everything())
```

### What about the minimum temperatures in Ushuaia?

I am interested in just how different the current (coldest) temperatures
in the Southern South America are to get an idea of how the temperature
difference that must be overcome for species living in those areas to
survive in Antarctica.

``` r
box_sa <- c(xmin = -85, ymin = -66, xmax = -45, ymax = -45)
temp_sf_sa <- st_crop(temp_sf, box_sa)
world <- ne_countries(scale = 50, returnclass = "sp")
world_sf <- st_as_sf(world, coords = c("x", "y"))
south_am <- st_crop(world_sf, box_sa)

ggplot() +
  geom_sf(
    data = south_am,
    mapping = aes(),
    color = "seashell3",
    fill = "seashell2"
  ) +
  geom_sf(
    data = antarctica_close,
    mapping = aes(),
    color = "seashell3",
    fill = "seashell2"
  ) +
  geom_sf(
    data = temp_sf_ant,
    mapping = aes(color = x0)
  ) +
  geom_sf(
    data = comnap_stations_sf,
    mapping = aes(),
    color = "tomato4",
    shape = 17
  ) +
  geom_sf(
    data = table_24_sf,
    mapping = aes(),
    color = "orange"
  ) +
  coord_sf(crs = 3031) +
  scale_color_gradient(name = "Mean Winter Temperature")
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

### And Antarctic summer temperatures?

These are the closest average sea temperatures to the coldest South
American temperatures.

``` r
temp_data_summer <- read_csv(here("data", "NOAA", "woa18_A5B7_t13an04.csv"), skip = 1) %>%
  clean_names()
temp_data_summer <- temp_data_summer %>%
  rename(
    latitude = number_comma_separated_latitude,
    x0 = and_values_at_depths_m_0
  ) %>%
  dplyr::select(latitude, longitude, everything())
temp_sf_summer <- st_as_sf(temp_data_summer,
  stringsAsFactors = TRUE,
  coords = c("longitude", "latitude")
)
st_crs(temp_sf_summer) <- 4326
temp_sf_ant_summer <- st_transform(temp_sf,crs =  3031) %>% 
  st_crop(box_close_ant)
ggplot() +
  geom_sf(
    data = antarctica_close,
    mapping = aes(),
    color = "seashell3",
    fill = "seashell2"
  ) +
  geom_sf(
    data = temp_sf_ant_summer,
    mapping = aes(color = x0)
  ) +
  geom_sf(
    data = comnap_stations_sf,
    mapping = aes(),
    color = "tomato4",
    shape = 17
  ) +
  geom_sf(
    data = table_24_sf,
    mapping = aes(),
    color = "orange"
  ) +
  scale_color_gradient(name = "Mean Summer Temperature")
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->
\#\# Number of ice free days

Louise Ireland (MAGIC Team at BAS) calculated the number of ice free
days in each calendar year 2014-2018, based on pixels that are 6.25km
square and an ice free threshold of 15% (i.e. if 15% of the pixel or
more is ice free), which is the same threshold used to delineate sea ice
edges.

To extract the number of ice free days per year I import the 5 geotiffs
and extract the values that overlap with (or are closest to) my 20
locations.

``` r
GDALinfo(here("data", "SeaIceData", "asi-AMSR2-s6250_ice_free_days_2014.tif"))
```

    ## rows        1328 
    ## columns     1264 
    ## bands       1 
    ## lower left origin.x        -3950000 
    ## lower left origin.y        -3950000 
    ## res.x       6250 
    ## res.y       6250 
    ## ysign       -1 
    ## oblique.x   0 
    ## oblique.y   0 
    ## driver      GTiff 
    ## projection  +proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84
    ## +units=m +no_defs 
    ## file        /Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/data/SeaIceData/asi-AMSR2-s6250_ice_free_days_2014.tif 
    ## apparent band summary:
    ##   GDType hasNoDataValue NoDataValue blockSize1 blockSize2
    ## 1 UInt16           TRUE         999          3       1264
    ## apparent band statistics:
    ##   Bmin Bmax    Bmean      Bsd
    ## 1    0  365 280.4713 122.0661
    ## Metadata:
    ## AREA_OR_POINT=Area

``` r
ice_free_2014 <- raster(x = here("data", "SeaIceData", "asi-AMSR2-s6250_ice_free_days_2014.tif"))
ice_free_2015 <- raster(x = here("data", "SeaIceData", "asi-AMSR2-s6250_ice_free_days_2015.tif"))
ice_free_2016 <- raster(x = here("data", "SeaIceData", "asi-AMSR2-s6250_ice_free_days_2016.tif"))
ice_free_2017 <- raster(x = here("data", "SeaIceData", "asi-AMSR2-s6250_ice_free_days_2017.tif"))
ice_free_2018 <- raster(x = here("data", "SeaIceData", "asi-AMSR2-s6250_ice_free_days_2018.tif"))
```

Next step is to extract the values that correspond to my top 20
locations (`top_20_sf`). Since 3 locations do not fall on a pixel and I
used a 5km buffer for the area that ships entered I have used a 5km
(5000m) buffer the ice free days and taken the mean.

``` r
table_24_sp <- as_Spatial(table_24_sf)
table_24_sf$ice_free_2014_5km <- raster::extract(ice_free_2014, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$ice_free_2015_5km <- raster::extract(ice_free_2015, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$ice_free_2016_5km <- raster::extract(ice_free_2016, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$ice_free_2017_5km <- raster::extract(ice_free_2017, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$ice_free_2018_5km <- raster::extract(ice_free_2018, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
```

For Atka Bank each year and some other locations in select years, the
5km buffer did not give data for ice-free days. This is because pixels
with no data (representing land and ice shelves) change from year to
year and some points (with the 5km buffer) in certain years fell
entirely within no data pixels. Those occurrences have been calculated
with 10km buffer.

``` r
table_24_sf$ice_free_2014_10km <- raster::extract(ice_free_2014, table_24_sp, buffer = 10000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$ice_free_2015_10km <- raster::extract(ice_free_2015, table_24_sp, buffer = 10000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$ice_free_2016_10km <- raster::extract(ice_free_2016, table_24_sp, buffer = 10000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$ice_free_2017_10km <- raster::extract(ice_free_2017, table_24_sp, buffer = 10000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$ice_free_2018_10km <- raster::extract(ice_free_2018, table_24_sp, buffer = 10000, small = TRUE, fun = mean, na.rm = TRUE)
```

Then for my 5-year period (2014-2018) I calculate the mean and SD, to be
included in the table.

``` r
table_24_df <- table_24_sf %>%
  as_tibble() %>%
  mutate(ice_free_2014 = case_when(is.na(ice_free_2014_5km) ~ ice_free_2014_10km, !is.na(ice_free_2014_5km) ~ ice_free_2014_5km),
         ice_free_2015 = case_when(is.na(ice_free_2015_5km) ~ ice_free_2015_10km, !is.na(ice_free_2015_5km) ~ ice_free_2015_5km),
         ice_free_2016 = case_when(is.na(ice_free_2016_5km) ~ ice_free_2016_10km, !is.na(ice_free_2016_5km) ~ice_free_2016_5km),
         ice_free_2017 = case_when(is.na(ice_free_2017_5km) ~ ice_free_2017_10km, !is.na(ice_free_2017_5km) ~ice_free_2017_5km),
         ice_free_2018 = case_when(is.na(ice_free_2018_5km) ~ ice_free_2018_10km, !is.na(ice_free_2018_5km) ~ice_free_2018_5km)
         ) %>% 
  mutate(
    ice_free_mean = apply(dplyr::select(., c(
      ice_free_2014,
      ice_free_2015,
      ice_free_2016,
      ice_free_2017,
      ice_free_2018
    )), 1, mean, na.rm = TRUE),
    ice_free_sd = apply(dplyr::select(., c(
      ice_free_2014,
      ice_free_2015,
      ice_free_2016,
      ice_free_2017,
      ice_free_2018
    )), 1, sd, na.rm = TRUE)
  )
```

Data from my list of top 20 are copied and pasted into the manuscript,
but the full table is here.

### SST above 0ºC

0ºC, while still above the freezing temperature of seawater is an
important threshold for marine organisms because the rate of metabolic
reactions is substantially slower at 0ºC and organisms typically require
specialist adaptations to fulfill their energy requirements and
reproduce if living at extremely low temperatures most of the time. SST
is also an important, thought not the only, factor determining the
realised niches of marine organisms and their potential to establish
populations in new regions. We have included the proportion of days per
year above this threshold from 2014-2018, based on sea surface data
(SST) available through the AQUA MODIS satellite. The data are available
for 8-day periods, but not each pixel has data for every 8-day period
(due to factors such as sea-ice and cloud coverage). Therefore three
data sets have been extracted for each calendar year in 2014-2018:
first, the number of 8-day periods above 0ºC for each year for each
pixel; second, the number of 8-periods each year for each pixel with SST
data (above or below 0ºC; third, total number of 8-periods (with or
without data) per pixel per year. Values were extracted by Louise
Ireland, British Antarctic Survey.

``` r
GDALinfo(here("data", "SSTData", "arlie_sst", "sst_2014_over0.tif"))
```

    ## rows        4320 
    ## columns     8640 
    ## bands       1 
    ## lower left origin.x        -180 
    ## lower left origin.y        -90.00001 
    ## res.x       0.04166667 
    ## res.y       0.04166667 
    ## ysign       -1 
    ## oblique.x   0 
    ## oblique.y   0 
    ## driver      GTiff 
    ## projection  NA 
    ## file        /Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/data/SSTData/arlie_sst/sst_2014_over0.tif 
    ## apparent band summary:
    ##   GDType hasNoDataValue NoDataValue blockSize1 blockSize2
    ## 1  Int16          FALSE           0          1       8640
    ## apparent band statistics:
    ##     Bmin  Bmax Bmean Bsd
    ## 1 -32768 32767    NA  NA

``` r
sst_2014_over0 <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2014_over0.tif"))
sst_2015_over0 <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2015_over0.tif"))
sst_2016_over0 <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2016_over0.tif"))
sst_2017_over0 <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2017_over0.tif"))
sst_2018_over0 <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2018_over0.tif"))
sst_2014_notnull <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2014_notnull.tif"))
sst_2015_notnull <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2015_notnull.tif"))
sst_2016_notnull <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2016_notnull.tif"))
sst_2017_notnull <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2017_notnull.tif"))
sst_2018_notnull <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2018_notnull.tif"))
sst_2014_totalgrids <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2014_totalgrids.tif"))
sst_2015_totalgrids <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2015_totalgrids.tif"))
sst_2016_totalgrids <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2016_totalgrids.tif"))
sst_2017_totalgrids <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2017_totalgrids.tif"))
sst_2018_totalgrids <- raster(x = here("data", "SSTData", "arlie_sst", "sst_2018_totalgrids.tif"))
```

Now that I have the geotiffs with SST information, I extract the values
using the same process as for the sea ice data. Again, becuase some
locations are not on a pixel I use the average of pixels within a 5km
buffer.

``` r
table_24_sf$sst_2014_over0_5km <- raster::extract(sst_2014_over0, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2015_over0_5km <- raster::extract(sst_2015_over0, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2016_over0_5km <- raster::extract(sst_2016_over0, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2017_over0_5km <- raster::extract(sst_2017_over0, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2018_over0_5km <- raster::extract(sst_2018_over0, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
```

Now the not null grids.

``` r
table_24_sf$sst_2014_notnull_5km <- raster::extract(sst_2014_notnull, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2015_notnull_5km <- raster::extract(sst_2015_notnull, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2016_notnull_5km <- raster::extract(sst_2016_notnull, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2017_notnull_5km <- raster::extract(sst_2017_notnull, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2018_notnull_5km <- raster::extract(sst_2018_notnull, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
```

Now the total grids

``` r
table_24_sf$sst_2014_totalgrids_5km <- raster::extract(sst_2014_totalgrids, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2015_totalgrids_5km <- raster::extract(sst_2015_totalgrids, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2016_totalgrids_5km <- raster::extract(sst_2016_totalgrids, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2017_totalgrids_5km <- raster::extract(sst_2017_totalgrids, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
table_24_sf$sst_2018_totalgrids_5km <- raster::extract(sst_2018_totalgrids, table_24_sp, buffer = 5000, small = TRUE, fun = mean, na.rm = TRUE)
```

From the totalgrids data, we know that there are 46 8-day periods for
each year and every location.

Now, I calculate the % of periods with data above 0 (and SD), as well as
the % of periods with data (with SD).

``` r
table_24_df_sst <- table_24_sf %>%
  as_tibble() %>%
  right_join(table_24_df) %>% 
  mutate(ice_free_periods_2014 = ice_free_2014/365*46,
         ice_free_periods_2015 = ice_free_2015/365*46,
         ice_free_periods_2016 = ice_free_2016/365*46,
         ice_free_periods_2017 = ice_free_2017/365*46,
         ice_free_periods_2018 = ice_free_2018/365*46
         ) %>% 
  mutate(sst_pcent_2014 = sst_2014_over0_5km/46*100,
         sst_pcent_2015 = sst_2015_over0_5km/46*100,
         sst_pcent_2016 = sst_2016_over0_5km/46*100,
         sst_pcent_2017 = sst_2017_over0_5km/46*100,
         sst_pcent_2018 = sst_2018_over0_5km/46*100
         ) %>% 
  mutate(sst_notnull_pcent_2014 = sst_2014_notnull_5km/46*100,
         sst_notnull_pcent_2015 = sst_2015_notnull_5km/46*100,
         sst_notnull_pcent_2016 = sst_2016_notnull_5km/46*100,
         sst_notnull_pcent_2017 = sst_2017_notnull_5km/46*100,
         sst_notnull_pcent_2018 = sst_2018_notnull_5km/46*100
         ) %>% 
  mutate(
    over0_mean = apply(dplyr::select(., c(
      sst_pcent_2014,
      sst_pcent_2015,
      sst_pcent_2016,
      sst_pcent_2017,
      sst_pcent_2018
    )), 1, mean, na.rm = TRUE),
    over0_sd = apply(dplyr::select(., c(
      sst_pcent_2014,
      sst_pcent_2015,
      sst_pcent_2016,
      sst_pcent_2017,
      sst_pcent_2018
    )), 1, sd, na.rm = TRUE),
    notnull_mean = apply(dplyr::select(., c(
      sst_notnull_pcent_2014,
      sst_notnull_pcent_2015,
      sst_notnull_pcent_2016,
      sst_notnull_pcent_2017,
      sst_notnull_pcent_2018
    )), 1, mean, na.rm = TRUE),
    notnull_sd = apply(dplyr::select(., c(
      sst_notnull_pcent_2014,
      sst_notnull_pcent_2015,
      sst_notnull_pcent_2016,
      sst_notnull_pcent_2017,
      sst_notnull_pcent_2018
    )), 1, sd, na.rm = TRUE)
  )
```

Next, I look at just the summarised data for these 24 locations so it
can be incorporated in to the manuscript table.

``` r
table_24_rank_all <- table_24_df_sst %>% 
  dplyr::select(place, ice_free_mean, ice_free_sd, over0_mean, over0_sd, notnull_mean, notnull_sd) %>% 
  right_join(table_24_rank) %>% 
  dplyr::select(rank, place, n_voyages, n_ships, median_time_hours, over0_mean, over0_sd, ice_free_mean, ice_free_sd) %>% 
  arrange(rank)
```

For the manuscript I create a map showing:  
- the Antarctic locations used in the network - the top visited
locations - the comparison locations - stations from the COMNAP list -
Davis, Dumont d’Urville, Neumayer III, and Scott/McMurdo

Locations can be circles and stations can be squares, colour can
differentiate between selected and non-selected sites.

``` r
name_eng <- c("Antarctic\nPeninsula", "South\nShetland\nIslands")
lat <- c(-65, -62)
lon <- c(-63, -59)
extra_labels <- data.frame(name_eng, lat, lon) %>% 
  st_as_sf(coords = c("lon", "lat"), remove = TRUE, dim = "XY", crs = 4326) %>% 
  st_transform(crs = 3031)

stations_locations_map <- ggplot() +
  geom_sf(data = antarctica_close,
          mapping = aes()) +
  geom_sf(
    data = ship_graph_ant,
    aes(colour = "Network locations",
        shape = "Network locations"),
  ) +
  geom_sf(
    data = table_24_sf,
    aes(colour = "Select network locations",
        shape = "Select network locations"),
    size = 3
  ) +
  geom_sf(
    data = comnap_stations_3031,
    aes(colour = "Stations and facilities",
        shape = "Stations and facilities"),
  ) +
  geom_label_repel(data = comnap_stations_labels,
                  stat = "sf_coordinates",
    aes(label = name_eng, geometry = geometry),
    nudge_x = c(400000, 500000, -1000000, -1000000),
    nudge_y = c(200000, -300000, -30000, -800000)
  ) +
  geom_label_repel(data = extra_labels,
                  stat = "sf_coordinates",
    aes(label = name_eng, geometry = geometry),
    nudge_x = c(-4000000, -50000),
    nudge_y = c(-2000000, -2000000)
  )  +
  geom_sf(
    data = comnap_stations_labels,
    aes(colour = "Select stations",
        shape = "Select stations"),
    size = 3
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
   scale_colour_manual(name = "Location type",
                       values = c("deepskyblue4",
                                  "deepskyblue",
                                  "tomato4",
                                  "tomato")) +
  scale_shape_manual(name = "Location type",
                     values = c(19, 19, 17, 17))

stations_locations_map
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/7-ManuscriptOutputs_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
save_plot(here("figures", "stations_locations_map.pdf"), stations_locations_map, base_width = 183, base_height = 120, units = "mm")
save_plot(here("figures", "stations_locations_map.png"), stations_locations_map, base_width = 183, base_height = 120, units = "mm")
```

This map is combined with another figure showing the visite per year to
top locations to go in the extended data section of the manuscript.

``` r
top_locs_and_stations_map <- plot_grid(visits_per_year, stations_locations_map +
            theme(
                 legend.title = element_text(size = 7),
                 legend.text = element_text(size = 7)
            ),
  ncol = 1,
  labels = c(
      "a Visits per year to select locations",
      "b Antarctic Facilities and network locations"),
  label_size = 8,
  align = "v",
  vjust = 1,
  hjust = -0.1,
  rel_heights = c(1,1))

save_plot(here("figures", "visit_year_stations_locations_map.pdf"), top_locs_and_stations_map, base_width = 183, base_height = 200, units = "mm")
save_plot(here("figures", "visits_year_stations_locations_map.png"), top_locs_and_stations_map, base_width = 183, base_height = 200, units = "mm")
```

# Time spent in ecoregions collecting biofouling

If ships collect biofouling based on how much time they spend in
different regions, then this is an indication of what pools/subsets of
species to look at. This is an important area of consideration when
thinking about which species might be transferred to Antarctica.

``` r
ecoregion_networks$all %>%
  activate(nodes) %>%
  arrange(desc(total_time)) %>%
  as_tibble() %>%
  group_by(ecoregion) %>%
  summarise(total_time = sum(total_time, na.rm = TRUE)) %>%
  arrange(desc(total_time))
```

    ## # A tibble: 181 × 2
    ##    ecoregion                             total_time
    ##    <chr>                                      <dbl>
    ##  1 Channels and Fjords of Southern Chile 1377201300
    ##  2 Rio de la Plata                       1162411380
    ##  3 East China Sea                         941725140
    ##  4 Antarctic Peninsula                    867381660
    ##  5 North Sea                              738240000
    ##  6 South Shetland Islands                 644148900
    ##  7 Namaqua                                528610620
    ##  8 Central Kuroshio Current               491371200
    ##  9 Malvinas/Falklands                     454577940
    ## 10 Sea of Japan/East Sea                  415797000
    ## # … with 171 more rows
