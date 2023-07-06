5-Networkfiles
================
Arlie McCarthy
25/11/2019

# Preparation for all kinds of network objects

First, packages and functions used in this script

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.6     ✓ dplyr   1.0.3
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(sf)
```

    ## Linking to GEOS 3.9.0, GDAL 3.2.0, PROJ 7.2.1

``` r
library(sp)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(here)
```

    ## here() starts at /Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
source(here("scripts", "functions", "function_not_in.R"))
source(here("scripts", "functions", "subset_edgelists.R"))
source(here("scripts", "functions", "subset_nodes.R"))
source(here("scripts", "functions", "subset_eco_edge_lists.R"))
source(here("scripts", "functions", "subset_eco_nodes_lists.R"))
```

With the trips\_networks file saved in the previous script, I manually
went through every observation and verified that they were in the
correct order. Every port call was given move sequence number by Informa
but these needed to be integrated into the southern ocean observations.
The observations were sorted by vessel\_id and date-time, then manually
checked to ensure that the arrival and sailed observations for port
calls were immediately before/after each other. A ‘move\_id’ was added
to every observation with port calls and satellite observations
correctly sorted for each vessel.

In doing so, \~70 observations that were not listed as being estimated
datetimes, ended up with observations no longer in chronological order.
I assumed that these were port calls that should have included datetime
qualifiers so added ‘B’.

Multiple networks are made for different aspects of analysis. The
different networks are made by generating different edgelists. Some of
the metrics calculated for later use are calculated before the network
is created.

``` r
# The primary trips object to be used to create the network objects and metrics
trips_networks <- read_csv(here::here("cleaned_data", "trips_networks.csv")) %>%
  clean_names()
circles_main <- read_csv(here::here("cleaned_data", "circles_main.csv")) %>%
  clean_names()
port_locations_main <- read_csv(here::here("cleaned_data", "port_locations_main.csv"),
                                col_types = cols(movement_type = col_character(),
                                                 notes = col_character(),
                                                 source = col_character())) %>%
  mutate(
    place = case_when(
      place == "Bahia Fildes" ~ "Maxwell Bay",
      TRUE ~ as.character(place)
    ),
    country = case_when(
      country == "Antarctica" ~ "Southern Ocean",
      TRUE ~ as.character(country)
    ),
    area = case_when(
      area == "Antarctica Treaty Area" ~ "Antarctica and the Southern Ocean",
      TRUE ~ as.character(area)
    )
  ) %>%
  clean_names()
vessels_main <- read_csv(here("cleaned_data", "vessels_main.csv"))
ant_locs_vessels <- read_csv(here("cleaned_data", "ant_locs_vessels.csv"))
so_obs <- read_csv(here("cleaned_data", "so_obs.csv"))
```

# Preparing for port-to-port networks

## Creating the node list

In these networks, nodes are ports so I need to create a list of all the
places (both Antarctic and worldwide that the ships visited, including
important attribute data).

``` r
#The circles are my Antarctic locations with buffers so these are used to
#create Antarctic nodes for my port network
nodes_ant <- circles_main %>% 
  dplyr::select(place, scar_common_id, feature_type_name, lat_2, lon_2, origin, country) %>%
  add_column(area = "Antarctica and the Southern Ocean") %>%
  rename(feature = feature_type_name)
nodes_ant$latitude <- nodes_ant$lat_2
nodes_ant$longitude <- nodes_ant$lon_2

#My worldwide nodes are my port locations, which also needs a few 
#adjustments to ensure that it lines up with the observations and 
#the Antarctic nodes. 
nodes_world <- port_locations_main %>%
  filter(!is.na(latitude)) %>%
  semi_join(trips_networks, by = "place") %>%
  add_column(feature = "Port") %>%
  mutate(feature = case_when(
    place == "Maxwell Bay" ~ "Bay",
    TRUE ~ as.character(feature)
  )) %>%
  group_by(place) %>%
  slice(1)
nodes_world$lat_2 <- nodes_world$latitude
nodes_world$lon_2 <- nodes_world$longitude
#The Antarctic and worldwide nodes can now be joined
nodes_list_all <- full_join(nodes_world, nodes_ant) %>%
  ungroup()
```

    ## Joining, by = c("place", "country", "area", "latitude", "longitude", "feature", "lat_2", "lon_2")

With a full list of nodes, I can assign each node (port) to an ecoregion
based on the Marine Ecoregions of the World (from Spalding2017).

``` r
nodes_list_sf <- sf::st_as_sf(nodes_list_all, coords = c("lon_2", "lat_2"))
st_crs(nodes_list_sf) <- 4326
MEOW <- st_read(here::here("data", "MEOW", "meow_ecos.shp")) %>%
  clean_names()
```

    ## Reading layer `meow_ecos' from data source `/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/data/MEOW/meow_ecos.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 232 features and 9 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -180 ymin: -89.9 xmax: 180 ymax: 86.9194
    ## geographic CRS: WGS 84

``` r
nodes_list_sf <- sf::st_join(nodes_list_sf, MEOW["ecoregion"])
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar
    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
nodes_list_sf <- st_join(nodes_list_sf, MEOW["province"])
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar
    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
nodes_list_sf <- st_join(nodes_list_sf, MEOW["realm"])
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar
    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
nodes_list_all <- as_tibble(nodes_list_sf) %>%
  dplyr::arrange(realm, province, ecoregion, place) %>%
  dplyr::select(-geometry, -place_id, -scar_common_id, -movement_type, -source, -notes, -origin) %>%
  clean_names()
```

## Preparation for creating edgelists

Now I need to create groups for creating separate trips. A trip is a
sequence of connected observations for a ship. Some ships had very
patchy observations, sometimes separated by years. To avoid connecting
locations in a sequence that should not be connected, I created a new
trip for each ship if there were no observations for a 120-day period at
any point. As a result, some ships have only 1 trip (they have the most
complete data), whereas others have multiple. 120 days was chosen as the
cut-off because some ships can spend months offshore, e.g. fishing. This
is only really used for creating sensible connections between ports etc,
rather than for analysis down the line.

``` r
trips_networks$date <- date(trips_networks$date_time)
trips_networks$gap <- c(0, diff(trips_networks$date) > 120) # identifies rows where
# the date_time is greater than 120 days later than the previous
# (dataframe ordered by ship and then date)

# cumulative sum of 'gap' variable
trips_networks$group <- cumsum(trips_networks$gap) + 1
trips_networks$trip_id <- paste(trips_networks$vessel_id,
  trips_networks$group,
  sep = "_"
)
```

## Edgelist for networks

Now I create the edge list. This is essentially a slightly reshaped
version of the trips\_networks dataframe.

But first, I check that all the locations in my observations
(`trips_networks`) are present in my `nodes_list_all` so that I will be
able to make a network.

``` r
length(unique(trips_networks$place))
```

    ## [1] 2118

``` r
trips_networks_check <- trips_networks %>%
  group_by(place) %>%
  filter(n() > 1)
length(unique(trips_networks_check$place))
```

    ## [1] 2110

``` r
anti_join(nodes_list_all, trips_networks, by = "place") %>%
  dplyr::select(place) %>%
  distinct()
```

    ## # A tibble: 0 x 1
    ## # … with 1 variable: place <chr>

``` r
anti_join(trips_networks, nodes_list_all, by = "place") %>%
  dplyr::select(place) %>%
  distinct()
```

    ## # A tibble: 1 x 1
    ##   place   
    ##   <chr>   
    ## 1 offshore

The difference between the length of `trips_networks` and
`trips_networks_check` tells me that there are 8 places that only appear
once, i.e. single visit to or from by one ship but I don’t see that as
an issue. They might be lost eventually when the network is created but
that’s fine.

There are 2117 places on my nodes list and 2118 unique places visited in
the trips\_networks dataframe (`offshore` is not a node in my network
but is used to classify observations outside a node).

``` r
duplicated_rows <- trips_networks %>%
  filter(place != "offshore") %>%
  mutate(duplicate = case_when(place != lag(place) & place != lead(place) ~ TRUE, TRUE ~ FALSE)) %>%
  filter(duplicate == TRUE)
```

An edge list needs a ‘from’ column and a ‘to’ column so that the network
packages can read it properly. These should be the first columns of the
dataframe. Other variables need to be renamed so that it is clear they
refer to the ‘from’ or ‘to’ observation of each pair. The two dataframes
are then combined by the ‘move\_id’ (which is adjusted for the ‘to’
observations to ensure they are matched to the previous observation).
The joining creates an ‘unsummarised’ edgelist, which contains a ‘from’
and ‘to’ observation for every movement every ship has undergone.

``` r
edgelist_from <- trips_networks %>%
  bind_rows(duplicated_rows) %>%
  rename(from_place = place,
         from_tms = date_time,
         from_ecoregion = ecoregion,
         from_province = province,
         from_realm = realm,
         arrival_qualifier = qualifier) %>%
  arrange(move_id) %>%
  rowid_to_column() %>%
  mutate(move_id = rowid) %>%
  filter(!is.na(from_place), from_place != "offshore")

edgelist_to <- trips_networks %>%
  bind_rows(duplicated_rows) %>%
  rename(to_place = place,
         to_tms = date_time,
         to_ecoregion = ecoregion,
         to_province = province,
         to_realm = realm,
         sailing_qualifier = qualifier) %>%
  arrange(move_id) %>%
  rowid_to_column() %>%
  mutate(move_id = rowid) %>%
  filter(!is.na(to_place), to_place != "offshore") %>%
  mutate(move_id = lag(move_id))

edgelist_unsummarised_main <- dplyr::left_join(edgelist_from, edgelist_to, by = "move_id") %>%
  dplyr::filter(trip_id.x == trip_id.y) %>%
  dplyr::select(vessel_id.x,
                from_place,
                to_place,
                from_tms,
                to_tms,
                from_ecoregion,
                to_ecoregion,
                from_province,
                to_province,
                from_realm,
                to_realm,
                trip_id.x,
                move_id,
                lat_2.x,
                lon_2.x,
                lat_2.y,
                lon_2.y,
                arrival_qualifier,
                sailing_qualifier) %>%
  arrange(trip_id.x, move_id) %>%
  unite("move", c("from_place", "to_place"), remove = FALSE) %>%
  mutate(gap = case_when(move == lag(move) ~ 0, TRUE ~ 1), group = cumsum(gap)) %>%
  group_by(group, move, vessel_id.x, from_place, to_place) %>%
  slice(1, n()) %>% # unfortunately this creates 2 identical rows if there is only 1 row, when really I only want two rows if there are multiple rows per group
  mutate(to_tms_2 = lead(to_tms)) %>%
  slice(1) %>%
  mutate(time_issues = case_when(from_tms > to_tms_2 ~ TRUE, TRUE ~ FALSE)) %>%
  dplyr::rename(vessel_id = vessel_id.x,
                from_lat = lat_2.x,
                from_lon = lon_2.x,
                to_lat = lat_2.y,
                to_lon = lon_2.y)
```

At this point, because not all ships visit the Antarctic locations that
are nodes in my network, I need to filter the edgelist so that only
ships that visit Antarctic nodes are included.

``` r
edgelist_filtered <- edgelist_unsummarised_main %>%
  filter(vessel_id %in% ant_locs_vessels$vessel_id) %>%
  mutate(from_ecoregion = case_when(
    from_place == "Éstonija, podnjatie" & to_place == "Éstonija, podnjatie" ~ to_ecoregion,
                                    TRUE ~ from_ecoregion)
    )
#One observation falls within the 5km radius of an Antarctic location but is in a different ecoregion,
#because the location is on the edge. To prevent issues down the line when joining the time 
#information to other node information, the ecoregion is changed to reflect the ecoregion of 
#the location, not the ecoregion of the observation.
write_csv(edgelist_filtered, here("cleaned_data", "edgelist_filtered.csv"), col_names = TRUE)
```

Once the edgelist has been created and excess ships been removed, there
are 38738 observations that will be included in a port-to-port network.

The edgelist is then summarised (`edgelist_main`) so that there is only
one row for any given from/to combination. Additional variables are
created that can be used to weight the network and calculate information
about voyages. They include:

-   `n_voyages` = number of voyages on a route from a given node to
    another  
-   `n_vessels` = number of distinct vessels that travelled that route  
-   `n_trips` = number of disctinct ‘trips’ (that I created) along that
    route

``` r
edgelist_main <- edgelist_filtered %>%
  arrange(move) %>%
  group_by(move, from_place, to_place,
           from_ecoregion,
           to_ecoregion,
           from_province,
           to_province,
           from_realm,
           to_realm) %>%
  summarise(n_voyages = n(),
            n_ships = n_distinct(vessel_id),
            n_trips = n_distinct(trip_id.x)) %>%
  mutate(from = from_place, to = to_place) %>%
  dplyr::select(from, to, everything())
```

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

## Time spent in ports and Antarctic locations

Calculating the time spent in locations and voyaging between locations
requires the times that were automatically generated by Informa’s system
to be removed, creating different edgelists from the ones used to create
the network. These are use to calculate the time spent attributes, which
are then joined to the node and edge lists in the network.

This chunk calculates metrics associated with time spent stopped in
ports, and time taken to travel between ports which are then attributes
added to to the edge list (voyage times) and nodes list (time in port).

``` r
#I start with the filtered, but unsummarised edgelist and remove the qualified times
unsummarised_times <- edgelist_filtered %>%
  filter(arrival_qualifier %!in% c("B", "A", "T") & sailing_qualifier %!in% c("B", "A", "T"))

times_spent <- unsummarised_times %>%
  dplyr::ungroup() %>%
  dplyr::mutate(time_spent = lubridate::as.duration(from_tms %--% to_tms_2)) %>%
  dplyr::filter(time_spent != lubridate::as.duration(0)) %>% 
  #removing all the zeros removes all the occurrances of the start and end time being 
  #exactly the same and therefore either a single location fix (that was given a to and from)
  #or an instance of a ship being in two places at once.
  dplyr::arrange(move) %>% 
  #most of the very short remaining times are a reflection of moving to or from NA to an 
  #Antarctic location, which is fine. It simply shows that there were location fixes at 
  #1min intervals for some ships as they moved into that area.
  dplyr::group_by(move, from_place, to_place, from_ecoregion, to_ecoregion) %>%
  dplyr::summarise(total_time = as.duration(sum(as.double(time_spent))),
                   median_time = as.duration(median(as.double(time_spent))),
                   mean_time = as.duration(mean(as.double(time_spent))),
                   n_time = n()
                   )
```

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion'. You can override using the `.groups` argument.

Now I need to join the times to the main edgelist, which I call
edge\_list\_all (as in all ships) and will be used to create the main
port-to-port network.

``` r
edge_list_all <- edgelist_main %>%
  full_join(times_spent, by = c("move", "from_place", "to_place", "from_ecoregion", "to_ecoregion"))
```

Now I take the attribute data from the edge\_list and join it to the
list of ports, as a new nodes list for the port-to-port network for all
ships.

``` r
node_attributes <- edge_list_all %>%
  filter(from_place == to_place) %>%
  mutate(place = from_place) %>%
  ungroup() %>%
  dplyr::select(-from_place, -to_place, -from_ecoregion, -to_ecoregion)

nodes_list <- nodes_list_all %>%
  left_join(node_attributes, by = "place") %>%
  dplyr::select(-from, -to, -move, -from_realm, -to_realm)
```

## Edge and nodes lists for each activity type

Next, I need to do the same as above but for the 5 different activity
types. Although it is possible to subset graphs using network packages,
since so many of my attributes need to be calculated before I create the
networks, I need to generate separate edge and node lists for each.

I made two functions: one that would create edge lists for each activity
type, and one that would take those edge lists and create node lists.
The functions are in the `scripts/functions` directory called
`subset_edgelists.R` and `subset_nodes.R`.

``` r
edge_lists <- subset_edge_lists(
  vessel_filter = activity_types,
  all_vessels = vessels_main,
  unsummarised_observations = edgelist_filtered
)
```

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_place', 'to_place', 'from_ecoregion'. You can override using the `.groups` argument.

``` r
nodes_lists <- subset_node_lists(
  attributes_list = edge_lists,
  port_list = nodes_list_all
)
```

Now save the port-to-port network nodes list and edge lists.

``` r
write_csv(nodes_list, here("cleaned_data", "nodes_list.csv"), col_names = TRUE)
write_csv(nodes_lists$fishing, here("cleaned_data", "nodes_list_fishing.csv"), col_names = TRUE)
write_csv(nodes_lists$tourism, here("cleaned_data", "nodes_list_tourism.csv"), col_names = TRUE)
write_csv(nodes_lists$research, here("cleaned_data", "nodes_list_research.csv"), col_names = TRUE)
write_csv(nodes_lists$supply, here("cleaned_data", "nodes_list_supply.csv"), col_names = TRUE)
write_csv(nodes_lists$other, here("cleaned_data", "nodes_list_other.csv"), col_names = TRUE)
write_csv(edge_list_all, here("cleaned_data", "edge_list.csv"), col_names = TRUE)
write_csv(edge_lists$fishing, here("cleaned_data", "edge_list_fishing.csv"), col_names = TRUE)
write_csv(edge_lists$tourism, here("cleaned_data", "edge_list_tourism.csv"), col_names = TRUE)
write_csv(edge_lists$research, here("cleaned_data", "edge_list_research.csv"), col_names = TRUE)
write_csv(edge_lists$supply, here("cleaned_data", "edge_list_supply.csv"), col_names = TRUE)
write_csv(edge_lists$other, here("cleaned_data", "edge_list_other.csv"), col_names = TRUE)
```

Clear the objects I no longer need.

``` r
rm(
  circles_main,
  ant_locs_vessels,
  edge_list_all,
  edge_lists,
  edgelist_filtered,
  edgelist_from,
  edgelist_to,
  edgelist_main,
  edgelist_unsummarised_main,
  node_attributes,
  nodes_ant,
  nodes_list_all,
  nodes_list_sf,
  nodes_lists,
  nodes_world,
  port_locations_main,
  times_spent,
  trips_networks_check,
  unsummarised_times
)
```

# Ecoregion node and edge lists

This section on creating the ecoregion edgelists requires a different
set of to and from lists of observations because I want to include all
the observations from ecoregions, not just those that enter one of my
Antarctic locations. The port calls also need to be grouped by
ecoregion, rather than the port names. In essence, however, it is the
same process that was used for the port-to-port network.

## Preparation for ecoregion node and edge lists

``` r
#Create a from dataframe
edgelist_eco_from <- trips_networks %>%
  rename(from_place = place,
         from_tms = date_time,
         from_ecoregion = ecoregion,
         from_province = province,
         from_realm = realm,
         arrival_qualifier = qualifier) %>%
  arrange(move_id) %>%
  filter(!is.na(from_place), from_ecoregion != "offshore")
#Create a to dataframe
edgelist_eco_to <- trips_networks %>%
  rename(to_place = place,
         to_tms = date_time,
         to_ecoregion = ecoregion,
         to_province = province,
         to_realm = realm,
         sailing_qualifier = qualifier) %>%
  arrange(move_id) %>%
  filter(!is.na(to_place), to_ecoregion != "offshore") %>%
  mutate(move_id = lag(move_id))
#Join the from and to dataframes
edgelist_eco_unsummarised <- dplyr::left_join(edgelist_eco_from, edgelist_eco_to, by = "move_id") %>%
  dplyr::filter(trip_id.x == trip_id.y) %>%
  dplyr::select(vessel_id.x,
                from_place,
                to_place,
                from_tms,
                to_tms,
                from_ecoregion,
                to_ecoregion,
                from_province,
                to_province,
                from_realm,
                to_realm,
                trip_id.x,
                move_id,
                lat_2.x,
                lon_2.x,
                lat_2.y,
                lon_2.y,
                arrival_qualifier,
                sailing_qualifier) %>%
  arrange(trip_id.x, move_id) %>%
  unite("move", c("from_ecoregion", "to_ecoregion"), remove = FALSE) %>%
  mutate(gap = case_when(move == lag(move) ~ 0, TRUE ~ 1), group = cumsum(gap)) %>%
  group_by(group, move, vessel_id.x, from_place, to_place) %>%
  slice(1, n()) %>% # unfortunately this creates 2 identical rows if there is only 1 row,
  #when really I only want two rows if there are multiple rows per group
  mutate(to_tms_2 = lead(to_tms)) %>%
  slice(1) %>%
  mutate(time_issues = case_when(from_tms > to_tms_2 ~ TRUE, TRUE ~ FALSE)) %>%
  dplyr::rename(vessel_id = vessel_id.x,
                from_lat = lat_2.x,
                from_lon = lon_2.x,
                to_lat = lat_2.y,
                to_lon = lon_2.y)
```

Although, all ships should visit the ecoregions that are nodes in my
network, I need to filter the edgelist so that only ships that visit
Antarctic ecoregions nodes are included.

To do that, I create a list of vessels that are found in Antarctic
ecoregions (based on the dataframe of Southern Ocean observations,
`so_obs`). I then filter my ecoregion edgelist against that list of
vessels.

``` r
vessels_ecoregions <- so_obs %>%
  filter(ecoregion != "offshore") %>%
  group_by(vessel_id) %>%
  slice(1)

edgelist_eco_filtered <- edgelist_eco_unsummarised %>%
  filter(vessel_id %in% vessels_ecoregions$vessel_id)

write_csv(edgelist_eco_filtered, here("cleaned_data", "edgelist_eco_filtered.csv"), col_names = TRUE)
```

Once the excess ships been removed, there are 70345 observations that
will be included in an ecoregion-to-ecoregion network. From this point,
it es essentially the same process as for the port-to-port network,
except that all joining and grouping is by “ecoregion” instead of
“place”. New functions were written to create the ecoregion networks and
via activity type, as per the chunks below which give the node and edge
lists for the ecoregion network with all ships.

``` r
edgelist_eco_main <- edgelist_eco_filtered %>%
  arrange(move) %>%
  group_by(move, from_ecoregion, to_ecoregion, from_realm, to_realm) %>%
  summarise(n_voyages = n(), n_ships = n_distinct(vessel_id), n_trips = n_distinct(trip_id.x)) %>%
  mutate(from = from_ecoregion, to = to_ecoregion) %>%
  dplyr::select(from, to, everything())
```

    ## `summarise()` has grouped output by 'move', 'from_ecoregion', 'to_ecoregion', 'from_realm'. You can override using the `.groups` argument.

## Time spent in ecoregions

Calculating the time spent in ecoregion, as for the ports, requires the
times that were automatically generated by Informa’s system to be
removed, creating different edgelists and to/from objects than the ones
used to create the network. These are used to calculate the time spent
attributes, which are then joined to the node and edge lists in the
network.

This chunk calculates metrics associated with time spent stopped in
ports which are then attributes added to to the nodes list.

``` r
ecoregion_unsummarised_times <- edgelist_eco_filtered %>%
  filter(arrival_qualifier %!in% c("B", "A", "T") & sailing_qualifier %!in% c("B", "A", "T"))

ecoregion_times <- ecoregion_unsummarised_times %>%
  dplyr::ungroup() %>%
  dplyr::mutate(time_spent = lubridate::as.duration(from_tms %--% to_tms_2)) %>%
  dplyr::filter(time_spent != lubridate::as.duration(0)) %>%
  dplyr::arrange(move) %>%
  dplyr::group_by(move, from_ecoregion, to_ecoregion) %>%
  dplyr::summarise(total_time = as.duration(sum(as.double(time_spent))),
                   median_time = as.duration(median(as.double(time_spent))),
                   mean_time = as.duration(mean(as.double(time_spent))),
                   n_time = n()
                   )
```

    ## `summarise()` has grouped output by 'move', 'from_ecoregion'. You can override using the `.groups` argument.

The ecoregion times are joined to the main edgelist and called the
`edge_list_eco_all`.

``` r
edge_list_eco_all <- edgelist_eco_main %>%
  full_join(ecoregion_times, by = c("move", "from_ecoregion", "to_ecoregion"))
```

To create the nodes list for the ecoregions, it is essentially the same
process as for the port-to-port network, but in this case the MEOW list
of ecoregions is the base dataframe to which attribute information is
added.

``` r
node_eco_attributes <- edge_list_eco_all %>%
  filter(from_ecoregion == to_ecoregion) %>%
  mutate(ecoregion = from_ecoregion) %>%
  ungroup() %>%
  dplyr::select(-from_ecoregion, -to_ecoregion)

nodes_list_eco_all <- MEOW %>%
  dplyr::select(ecoregion, everything()) %>%
  left_join(node_eco_attributes, by = "ecoregion") %>%
  dplyr::select(-move, -from, -to, -from_realm, -to_realm)
```

As for the port-to-port networks, I now use a couple of functions to
create the edge and node lists for the different activity type ecorgion
networks. Functions can be seen in the `scripts/functions` directory.

``` r
edge_lists_eco <- subset_eco_edge_lists(
  vessel_filter = activity_types,
  eco_vessels = vessels_ecoregions,
  all_vessels = vessels_main,
  unsummarised_observations = edgelist_eco_filtered
)
```

    ## `summarise()` has grouped output by 'move', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_ecoregion'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_ecoregion'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_ecoregion'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_ecoregion'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_ecoregion', 'to_ecoregion', 'from_province', 'to_province', 'from_realm'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'move', 'from_ecoregion'. You can override using the `.groups` argument.

``` r
nodes_lists_eco <- subset_eco_node_lists(
  attributes_list = edge_lists_eco,
  ecoregion_list = MEOW
)
```

Now, I can save all these node and edgelists ready to make the networks.

``` r
write_csv(nodes_list_eco_all, here("cleaned_data", "nodes_list_ecoregion.csv"), col_names = TRUE)
write_csv(nodes_lists_eco$fishing, here("cleaned_data", "nodes_list_eco_fishing.csv"), col_names = TRUE)
write_csv(nodes_lists_eco$tourism, here("cleaned_data", "nodes_list_eco_tourism.csv"), col_names = TRUE)
write_csv(nodes_lists_eco$research, here("cleaned_data", "nodes_list_eco_research.csv"), col_names = TRUE)
write_csv(nodes_lists_eco$supply, here("cleaned_data", "nodes_list_eco_supply.csv"), col_names = TRUE)
write_csv(nodes_lists_eco$other, here("cleaned_data", "nodes_list_eco_other.csv"), col_names = TRUE)
write_csv(edge_list_eco_all, here("cleaned_data", "edge_list_eco.csv"), col_names = TRUE)
write_csv(edge_lists_eco$fishing, here("cleaned_data", "edge_list_eco_fishing.csv"), col_names = TRUE)
write_csv(edge_lists_eco$tourism, here("cleaned_data", "edge_list_eco_tourism.csv"), col_names = TRUE)
write_csv(edge_lists_eco$research, here("cleaned_data", "edge_list_eco_research.csv"), col_names = TRUE)
write_csv(edge_lists_eco$supply, here("cleaned_data", "edge_list_eco_supply.csv"), col_names = TRUE)
write_csv(edge_lists_eco$other, here("cleaned_data", "edge_list_eco_other.csv"), col_names = TRUE)
```

### Checking my obvservations against verified observations

In the previous script, I explain that I checked field observations in
Bahia Fildes/Maxwell Bay against the satellite observations in my data.
As Pig Rock and Potter Cove are also areas within Maxwell Bay I selected
the three locations. The filtered list of observations was manually
compared to the field observations in the excel file supplied.

``` r
maxwell_bay_check <- trips_networks %>%
  filter(place == "Maxwell Bay" | place == "Pig Rock" | place == "Potter Cove") %>%
  arrange(date_time)
```
