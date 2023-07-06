3 - Antarctic Locations
================
Arlie McCarthy
24/03/2021

## Summary

This script selects locations around Antarctica that will act as the
eqivalent to the worldwide port in the network.

The SCAR Gazeteer of Antarctic Place names is used to identify possible
locations to include. A thorough selection process is used to select the
final locations from the inital 19836 unique places included in the
Gazeteer (data is available from the `antanym` package).

The following sections of this script create and check the Antarctic
locations:

1.  Import main files (outputs from previous script 2 - Cleaning stages
    4 and 5)  
2.  Import data on Antarctic locations and place names
3.  Account for uncertainty in AIS satellite readings by creating
    ‘buffer’ zones around select locations
4.  Check different buffer sizes and select appropriate one
5.  Select the final Antarctic locations (only ones visited by ships,
    none overlapping)
6.  Assign Southern Ocean observations to Antarctic locations so that
    they have an equivalent to port calls.

There is a lot in this script that certainly could have been written and
run more efficiently, but it would now take me more time to change it so
I have left it in the way that works.

## 1 Import main files

During analysis in 5-Networkfiles, it became apparent that I needed to
create manually check and verify the order of each observation. While
this script was originally run with the `trips_main` file it is now run
with the `trips_networks_verified` script to ensure the final set of
verified observations are used to generate the Antarctic locations.
Because this script creates the `trips_networks` file all subsequent
files that used the initial trips networks file now run CORRECTLY using
the `trips_networks` file.

``` r
trips_main <- read_csv(here("data", "trips_networks_verified.csv"),
                       col_types = cols(
                         date_time = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
                         tms = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ")))

vessels_main <- read_csv(here("data", "vessels_main.csv"))
port_locations_main <- read_csv(here("data", "port_locations_main.csv"),
  col_types = cols(
    movement_type = col_character(),
    notes = col_character(), source = col_character()
  )
)
```

## 2 Import data on Antarctic place names

To be able create a network that includes connectivity between different
places around Antarctica I need to be able to assign each of my
sightings to relevant places around Antarctica. To do this, I take the
information from the SCAR Gazetteer on Antarctic Place Names, create a
5km radius circle around the point and assign any sightings within the
each cicle to the Antarctic Place.

These can be downloaded from the antanym package. To be able to work
without needing to download the data each time from the SCAR Gazeteer I
saved it locally as a file that could be uploaded.

``` r
ant_names <- antanym::an_read(cache = "session", simplified = TRUE)
write_csv(ant_names, here("data", "ant_names.csv"))
```

The following chunk is only needed if you are not downloading the the
Antarctic place names for the session.

``` r
ant_names <- read_csv(here("data", "ant_names.csv")) %>% clean_names()
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   gaz_id = col_double(),
    ##   scar_common_id = col_double(),
    ##   place_name = col_character(),
    ##   place_name_transliterated = col_character(),
    ##   longitude = col_double(),
    ##   latitude = col_double(),
    ##   altitude = col_double(),
    ##   feature_type_name = col_character(),
    ##   date_named = col_datetime(format = ""),
    ##   narrative = col_character(),
    ##   named_for = col_character(),
    ##   origin = col_character(),
    ##   relic = col_logical(),
    ##   gazetteer = col_character()
    ## )

### Checking the place names

The register of names contains multiple names for the same location so
need to filter the list to show preferred locations (i.e. one name/row
per location). I have preferentially used UK names. There are 20003
unique places so need to have a list of places with maximum 20003 rows.
Unfortunately, there are 200 places with a unique SCAR ID but the same
place name, likely to be things like “North cove”. This will become a
problem later if I match places based on name, rather than location id.
So, although I will need to keep the place names, all matching and
filtering etc will be done based on the SCAR common ID.

``` r
length(ant_names$scar_common_id)
```

    ## [1] 38883

``` r
length(unique(ant_names$scar_common_id))
```

    ## [1] 20003

``` r
names_uk <- an_preferred(ant_names, origin = c("United Kingdom"), unmatched = "count")
# I duplicate the coordinates columns so that when I turn the dataframe
# into a sf object I still have normal columns with coordinates
names_uk <- names_uk %>%
  mutate(lat_2 = latitude, lon_2 = longitude)
names_uk$feature_type_name <- as.factor(names_uk$feature_type_name)
length(names_uk$scar_common_id)
```

    ## [1] 20003

``` r
length(unique(names_uk$scar_common_id))
```

    ## [1] 20003

``` r
length(unique(names_uk$place_name))
```

    ## [1] 19803

However, I think there are also some places with different names that
have the same coordinates so I need to check how many there are and if
there is a sensible way of filtering them. It turns out there are 1489
of 20003 locations with duplicated coordinates. These are typically
because they reference geographical features with larger areas that
could overlap. For simplicity, I have taken the place name listed first
for each group with matching coordinates. Down the line, I can always
check place names against the locations with matching coordinates and
choose the most appropriate for display in figures etc.

``` r
names_uk %>%
  group_by(longitude, latitude) %>%
  filter(n() > 1)
```

    ## # A tibble: 1,489 x 16
    ## # Groups:   longitude, latitude [574]
    ##    gaz_id scar_common_id place_name place_name_tran… longitude latitude altitude
    ##     <dbl>          <dbl> <chr>      <chr>                <dbl>    <dbl>    <dbl>
    ##  1 100006             20 Abovedada… Abovedada, punta     -62.0    -64.6       NA
    ##  2 135174             30 Access Po… Access Point         -63.8    -64.8       NA
    ##  3 107435             33 Achæan Ra… Achaean Range        -63.6    -64.5       NA
    ##  4 107436             37 Achilles … Achilles Heel        -63.6    -64.5       NA
    ##  5 107442             49 Acuña Isl… Acuna Island         -44.6    -60.8       NA
    ##  6 107443             50 Acuña Roc… Acuna Rocks          -57.9    -63.3       NA
    ##  7 100014             51 Acuña, ro… Acuna, roca          -57.9    -63.3       NA
    ##  8 121591             72 Cape Adare Cape Adare           170.     -71.3       NA
    ##  9 107446             75 Adelaide   Adelaide             -68.9    -67.8       NA
    ## 10 116745             87 Admiralty… Admiralty Bay (…     -58.4    -62.1       NA
    ## # … with 1,479 more rows, and 9 more variables: feature_type_name <fct>,
    ## #   date_named <dttm>, narrative <chr>, named_for <chr>, origin <chr>,
    ## #   relic <lgl>, gazetteer <chr>, lat_2 <dbl>, lon_2 <dbl>

``` r
# I am taking the first listed place (name). If needed
# the name I display in results etc can be changed later.
names_uk <- names_uk %>%
  group_by(longitude, latitude) %>%
  slice(1)
```

### Creating coastal subset of Antarctic locations

But even the 19113 remaining locations are too many to reasonably be
able to deal with. So I will need to filter down to the ones that are
coastal or at sea. Now I need to add a CRS to the lat/lon data provided
by the gazette. I then need to transform this to a suitable CRS for
Antarctica.

``` r
names_uk <- st_as_sf(names_uk,
                     stringsAsFactors = TRUE,
                     coords = c("longitude", "latitude"))
st_crs(names_uk) <- 4326
st_geometry(names_uk)
```

    ## Geometry set for 19088 features 
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: -179.9167 ymin: -90 xmax: 180 ymax: -60
    ## geographic CRS: WGS 84
    ## First 5 geometries:

    ## POINT (-179.9167 -67.75)

    ## POINT (-179.9167 -67.4)

    ## POINT (-179.8333 -84.4167)

    ## POINT (-179.8333 -68)

    ## POINT (-179.4 -84.8)

``` r
names_uk <- sf::st_transform(names_uk, crs = 3031)
```

Now I access the outline of Antarctica so that I can select locations
that are within 2km of the coastline or offshore. I use a high
resolution map of Antarctica available from the map-data package. I do
not use the super-high resolution version from the polar data centre
because once I have taken the coastline in by 2km the fine scale
accuracy is less important. The version from the polar data centre also
takes so long to plot that it is essentially unusable.

``` r
antarctica <- map("worldHires", "Antarctica", fill = TRUE) 
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/3-AntarcticLocations_files/figure-gfm/IMPORT%20ANTARCTIC%20COASTLINE-1.png)<!-- -->

``` r
antarctica <- st_as_sf(antarctica, coords = c("x", "y"))
antarctica <- sf::st_transform(antarctica, crs = 3031)
```

So now that I have an Antarctic coastline, I need to create an Antarctic
coastline that is 2km (2000m) further inland than the real coastline.
This is so that I don’t accidentally remove any coastal locations
(i.e. a station) that ships would visit. I also plot it against the full
coastline to check that it worked as expected. When doing it the first
time I changed it to 100km to make it really obvious.

``` r
ant_buffer <- st_buffer(antarctica, -2000)

ggplot() +
  geom_sf(data = antarctica, mapping = aes()) +
  geom_sf(data = ant_buffer, mapping = aes(), colour = "coral") +
  coord_sf(crs = st_crs(3031))
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/3-AntarcticLocations_files/figure-gfm/CREATE%20BUFFER%20AROUND%20ANTARCTICA-1.png)<!-- -->

Now that I have a reduced Antarctica, I create a new variable that
indicates whether or not a location falls within the new coastline. In
an new dataframe (ant\_locs) I keep only those that are within 2km of
the original Antarctic coast or in the ocean.

``` r
names_uk$buffer <- as.logical(st_intersects(names_uk, ant_buffer))
ant_locs <- names_uk %>%
  replace_na(list(buffer = FALSE)) %>%
  filter(buffer == FALSE) %>%
  rename(place = place_name) %>%
  add_column(country = "Southern Ocean")
```

Now that I’m down to 7494 locations, time to check again how many have
identical coordinates and how many have identical place names.

``` r
# A list of 7490 distinct locations
as_tibble(ant_locs) %>%
  group_by(lat_2, lon_2) %>%
  distinct()
```

    ## # A tibble: 7,490 x 17
    ## # Groups:   lat_2, lon_2 [7,490]
    ##    gaz_id scar_common_id place place_name_tran… altitude feature_type_na…
    ##     <dbl>          <dbl> <chr> <chr>               <dbl> <fct>           
    ##  1 131337          16766 Scot… Scott Island Ba…       NA Bank            
    ##  2 126072           5858 Hagg… Haggits Pillar         65 Pillar          
    ##  3 131342          12910 Scot… Scott Seamounts        NA <NA>            
    ##  4 126979           6950 Isel… Iselin Bank            NA Bank            
    ##  5 117253           1883 Broc… Brockton /USA/         NA <NA>            
    ##  6 126980           6951 Isel… Iselin Seamount        NA <NA>            
    ##  7 127563           7766 Kosc… Kosco Glacier          NA Glacier         
    ##  8 129682          10656 Oppe… Oppegaard Spur         NA Spur            
    ##  9 130955          12378 Ross… Ross Bank              NA Bank            
    ## 10 112255           2865 Colu… Columbus High          NA Undersea ridge  
    ## # … with 7,480 more rows, and 11 more variables: date_named <dttm>,
    ## #   narrative <chr>, named_for <chr>, origin <chr>, relic <lgl>,
    ## #   gazetteer <chr>, lat_2 <dbl>, lon_2 <dbl>, geometry <POINT [m]>,
    ## #   buffer <lgl>, country <chr>

``` r
# A list of all the 0 duplicated places -
# good to check that there aren't duplicated places.
as_tibble(ant_locs) %>%
  group_by(lat_2, lon_2) %>%
  filter(n() > 1) %>%
  arrange(lat_2, lon_2)
```

    ## # A tibble: 0 x 17
    ## # Groups:   lat_2, lon_2 [0]
    ## # … with 17 variables: gaz_id <dbl>, scar_common_id <dbl>, place <chr>,
    ## #   place_name_transliterated <chr>, altitude <dbl>, feature_type_name <fct>,
    ## #   date_named <dttm>, narrative <chr>, named_for <chr>, origin <chr>,
    ## #   relic <lgl>, gazetteer <chr>, lat_2 <dbl>, lon_2 <dbl>, geometry <GEOMETRY
    ## #   [m]>, buffer <lgl>, country <chr>

``` r
# Check for duplicate names - gives us 78
as_tibble(ant_locs) %>%
  group_by(place) %>%
  filter(n() > 1) %>%
  arrange(place)
```

    ## # A tibble: 78 x 17
    ## # Groups:   place [39]
    ##    gaz_id scar_common_id place place_name_tran… altitude feature_type_na…
    ##     <dbl>          <dbl> <chr> <chr>               <dbl> <fct>           
    ##  1 107508            336 Anch… Anchorage Island       NA Anchorage       
    ##  2    845            337 Anch… Anchorage Island       53 Island          
    ##  3 107615            715 Azim… Azimuth Hill           NA Hill            
    ##  4    675            716 Azim… Azimuth Hill           20 Hill            
    ##  5 107820           1396 Blac… Black Island           NA Island          
    ##  6 122575           1397 Blac… Black Island           NA Island          
    ##  7 107846           9998 Bluf… Bluff Island           NA Bluff           
    ##  8 122651           1487 Bluf… Bluff Island           NA Bluff           
    ##  9 107880           1639 Bota… Botany Bay             NA Bay             
    ## 10 122764           1640 Bota… Botany Bay             NA Bay             
    ## # … with 68 more rows, and 11 more variables: date_named <dttm>,
    ## #   narrative <chr>, named_for <chr>, origin <chr>, relic <lgl>,
    ## #   gazetteer <chr>, lat_2 <dbl>, lon_2 <dbl>, geometry <POINT [m]>,
    ## #   buffer <lgl>, country <chr>

``` r
# Check for duplicate scar_common_ids - none
as_tibble(ant_locs) %>%
  group_by(scar_common_id) %>%
  filter(n() > 1) %>%
  arrange(scar_common_id)
```

    ## # A tibble: 0 x 17
    ## # Groups:   scar_common_id [0]
    ## # … with 17 variables: gaz_id <dbl>, scar_common_id <dbl>, place <chr>,
    ## #   place_name_transliterated <chr>, altitude <dbl>, feature_type_name <fct>,
    ## #   date_named <dttm>, narrative <chr>, named_for <chr>, origin <chr>,
    ## #   relic <lgl>, gazetteer <chr>, lat_2 <dbl>, lon_2 <dbl>, geometry <GEOMETRY
    ## #   [m]>, buffer <lgl>, country <chr>

Since there are still some locations with the same name (but different
IDs and coordinates), I will need to use the ID as the identifier for
the subsequent analyses.

## 3 Creating and choosing a suitable buffer size

I need to create a buffer zone around each location to account for the
fact that a) the AIS location fixes will have some error,  
b) some of the geographical features like a cover are larger than the
coordinates indicate,  
c) biologically if a ship comes within a few kilometers of a place is
equivalent to visiting that place

This section creates buffers of different sizes around each Antarctic
location so that I can choose a suitable buffer zone size.

``` r
circles_ant_1 <- st_buffer(ant_locs, dist = 1000)
circles_ant_2 <- st_buffer(ant_locs, dist = 2000)
circles_ant_5 <- st_buffer(ant_locs, dist = 5000)
circles_ant_10 <- st_buffer(ant_locs, dist = 10000)
circles_ant_15 <- st_buffer(ant_locs, dist = 15000)
circles_ant_20 <- st_buffer(ant_locs, dist = 20000)
circles_ant_25 <- st_buffer(ant_locs, dist = 25000)
```

But to make things more run faster I’m only interested in the locations
that overlap with my Southern Ocean sightings so I need to remove all
circles that do not overlap with any of my ship sightings.

To do that I need to add the location information for Antarctic places
to the trips\_main data. The variables I want to preserve are:
place\_name, scar\_common\_id, feature\_type\_name, lat\_2, lon\_2.

``` r
# start by making a sf object of my trips_main data
trips_sf <- trips_main %>%
  mutate(country = case_when(lat_2 <= -60 ~ "Southern Ocean",
                             TRUE ~ as.character(country))) %>% 
  filter(!is.na(lat_2) & !is.na(lon_2))
trips_sf <- st_as_sf(trips_sf, coords = c("lon_2", "lat_2"),remove = FALSE, crs = 4326)
trips_sf <- sf::st_transform(trips_sf, crs = 3031)
```

Since some of the buffers (1-5km) are used later, they always need to be
created.

``` r
trips_sf_1 <- st_join(trips_sf,
                      circles_ant_1["scar_common_id"], left = TRUE)
trips_sf_2 <- st_join(trips_sf,
                      circles_ant_2["scar_common_id"], left = TRUE)
trips_sf_5 <- st_join(trips_sf,
                      circles_ant_5["scar_common_id"], left = TRUE)
```

Some of the buffers (10-25km) I only need to help me decide which one to
go with. This chunk, in particular, takes a lot longer to run (25km
takes longer than everything else put together) and has been run with
the ultimate outputs saved, so it doesn’t need to be run every time.

``` r
trips_sf_10 <- st_join(trips_sf,
                       circles_ant_10["scar_common_id"], left = TRUE)
trips_sf_15 <- st_join(trips_sf,
                       circles_ant_15["scar_common_id"], left = TRUE)
trips_sf_20 <- st_join(trips_sf,
                       circles_ant_20["scar_common_id"], left = TRUE)
trips_sf_25 <- st_join(trips_sf,
                       circles_ant_25["scar_common_id"], left = TRUE)
```

Next, I filter each set of circles (locations with a buffer), based on
which ones my ships actually visit, give each circle a numerical id and
create a new column list which other circles each circle overlaps with.
I also make another column that tells me how many observations are in
each circle. Again, I split the chunks for 1-5km and 10-25km.

‘v’ indicates that these are the objects with locations (circles)
visited by ships.

``` r
circles_ant_1_v <- circles_ant_1 %>%
  semi_join(as_tibble(trips_sf_1), by = "scar_common_id")
clusters_circles_1 <- st_intersects(circles_ant_1_v, circles_ant_1_v)
circles_ant_1_v$circles_overlap <- clusters_circles_1
circles_ant_1_v <- circles_ant_1_v %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())

circles_ant_2_v <- circles_ant_2 %>%
  semi_join(as_tibble(trips_sf_2), by = "scar_common_id")
clusters_circles_2 <- st_intersects(circles_ant_2_v, circles_ant_2_v)
circles_ant_2_v$circles_overlap <- clusters_circles_2
circles_ant_2_v <- circles_ant_2_v %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())

circles_ant_5_v <- circles_ant_5 %>%
  semi_join(as_tibble(trips_sf_5), by = "scar_common_id")
clusters_circles_5 <- st_intersects(circles_ant_5_v, circles_ant_5_v)
circles_ant_5_v$circles_overlap <- clusters_circles_5
circles_ant_5_v <- circles_ant_5_v %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())
```

Filtered locations for buffer zones of 10-25km.

``` r
circles_ant_10_v <- circles_ant_10 %>%
  semi_join(as_tibble(trips_sf_10), by = "scar_common_id")
clusters_circles_10 <- st_intersects(circles_ant_10_v, circles_ant_10_v)
circles_ant_10_v$circles_overlap <- clusters_circles_10
circles_ant_10_v <- circles_ant_10_v %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())

circles_ant_15_v <- circles_ant_15 %>%
  semi_join(as_tibble(trips_sf_15), by = "scar_common_id")
clusters_circles_15 <- st_intersects(circles_ant_15_v, circles_ant_15_v)
circles_ant_15_v$circles_overlap <- clusters_circles_15
circles_ant_15_v <- circles_ant_15_v %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())

circles_ant_20_v <- circl <- circles_ant_20 %>%
  semi_join(as_tibble(trips_sf_20), by = "scar_common_id")
clusters_circles_20 <- st_intersects(circles_ant_20_v, circles_ant_20_v)
circles_ant_20_v$circles_overlap <- clusters_circles_20
circles_ant_20_v <- circles_ant_20_v %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())

circles_ant_25_v <- circles_ant_25 %>%
  semi_join(as_tibble(trips_sf_25), by = "scar_common_id")
clusters_circles_25 <- st_intersects(circles_ant_25_v, circles_ant_25_v)
circles_ant_25_v$circles_overlap <- clusters_circles_25
circles_ant_25_v <- circles_ant_25_v %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())
```

Now I create summarised objects so that I can assess how many
observations and vessels are in each circle, for each buffer size. Split
again into two chunks.

``` r
so_count_1 <- as_tibble(trips_sf_1) %>%
  dplyr::select(-geometry) %>%
  filter(country == "Southern Ocean") %>%
  group_by(scar_common_id) %>%
  summarise(n_obs = n(), n_vessels = n_distinct(vessel_id))

so_count_2 <- as_tibble(trips_sf_2) %>%
  dplyr::select(-geometry) %>%
  filter(country == "Southern Ocean") %>%
  group_by(scar_common_id) %>%
  summarise(n_obs = n(), n_vessels = n_distinct(vessel_id))

so_count_5 <- as_tibble(trips_sf_5) %>%
  dplyr::select(-geometry) %>%
  filter(country == "Southern Ocean") %>%
  group_by(scar_common_id) %>%
  summarise(n_obs = n(), n_vessels = n_distinct(vessel_id))
```

Summarised information for 10-25km.

``` r
so_count_10 <- as_tibble(trips_sf_10) %>%
  dplyr::select(-geometry) %>%
  filter(country == "Southern Ocean") %>%
  group_by(scar_common_id) %>%
  summarise(n_obs = n(), n_vessels = n_distinct(vessel_id))

so_count_15 <- as_tibble(trips_sf_15) %>%
  dplyr::select(-geometry) %>%
  filter(country == "Southern Ocean") %>%
  group_by(scar_common_id) %>%
  summarise(n_obs = n(), n_vessels = n_distinct(vessel_id))

so_count_20 <- as_tibble(trips_sf_20) %>%
  dplyr::select(-geometry) %>%
  filter(country == "Southern Ocean") %>%
  group_by(scar_common_id) %>%
  summarise(n_obs = n(), n_vessels = n_distinct(vessel_id))

so_count_25 <- as_tibble(trips_sf_25) %>%
  dplyr::select(-geometry) %>%
  filter(country == "Southern Ocean") %>%
  group_by(scar_common_id) %>%
  summarise(n_obs = n(), n_vessels = n_distinct(vessel_id))
```

Then I add the summarised information to the list of locations for each
buffer size.

``` r
circles_ant_1_v <- circles_ant_1_v %>%
  left_join(so_count_1, by = "scar_common_id") %>%
  add_column(buffer_size = 1) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                circle_id,
                lat_2,
                lon_2)

circles_ant_2_v <- circles_ant_2_v %>%
  left_join(so_count_2, by = "scar_common_id") %>%
  add_column(buffer_size = 2) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                circle_id,
                lat_2,
                lon_2)

circles_ant_5_v <- circles_ant_5_v %>%
  left_join(so_count_5, by = "scar_common_id") %>%
  add_column(buffer_size = 5) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                circle_id,
                lat_2,
                lon_2)
```

The same again for the second set of buffered points.

``` r
circles_ant_10_v <- circles_ant_10_v %>%
  left_join(so_count_10, by = "scar_common_id") %>%
  add_column(buffer_size = 10) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                circle_id,
                lat_2,
                lon_2)

circles_ant_15_v <- circles_ant_15_v %>%
  left_join(so_count_15, by = "scar_common_id") %>%
  add_column(buffer_size = 15) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                circle_id,
                lat_2,
                lon_2)

circles_ant_20_v <- circles_ant_20_v %>%
  left_join(so_count_20, by = "scar_common_id") %>%
  add_column(buffer_size = 20) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                circle_id,
                lat_2,
                lon_2)

circles_ant_25_v <- circles_ant_25_v %>%
  left_join(so_count_25, by = "scar_common_id") %>%
  add_column(buffer_size = 25) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                circle_id,
                lat_2,
                lon_2)
```

## 4 Checking a suitable buffer size

Now to plot the observations/counts so that I can assess a sensible
buffer size.

First I create a single object with all the buffer checks and save it.
This is so that I don’t need to re-run all the time-consuming chunks
above for the 10-25km buffer zones if I only want to re-create the
plots.

``` r
# smoosh them all together, by adding rows
buffer_check <- bind_rows(circles_ant_1_v,
                          circles_ant_2_v,
                          circles_ant_5_v,
                          circles_ant_10_v,
                          circles_ant_15_v,
                          circles_ant_20_v,
                          circles_ant_25_v)

buffer_check <- as_tibble(buffer_check) %>%
  tibble::rowid_to_column("row_id") %>%
  drop_na() %>% 
  dplyr::select(-geometry)

# save the buffer_check object so that I don't need
write_csv(buffer_check, here("cleaned_data", "buffer_check.csv"))
```

Import the saved file so that the plots can still be made without
running the rest of the time-consuming code.

``` r
buffer_check <- read_csv(here("cleaned_data", "buffer_check.csv")) %>% clean_names()
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   row_id = col_double(),
    ##   scar_common_id = col_double(),
    ##   n_obs = col_double(),
    ##   n_vessels = col_double(),
    ##   buffer_size = col_double(),
    ##   circle_id = col_double(),
    ##   lat_2 = col_double(),
    ##   lon_2 = col_double()
    ## )

Create plots to compare the different buffer sizes and check which ones
best capture my ship sightings.

``` r
buffer_check_circles <- buffer_check %>%
  group_by(buffer_size) %>%
  summarise(n_locs = n_distinct(scar_common_id))

plot_circles <- ggplot(buffer_check_circles, aes(x = buffer_size, y = n_locs)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Buffer size around location point (km)",
       y = "Number of locations with ship observations",
       title = "1. Locations included for each buffer size",
       subtitle = "A larger buffer size increases the chance of at least one observation overlapping with the location")
```

Plot of number of observations and summary stats.

``` r
plot_obs <- ggplot(buffer_check, aes(buffer_size, n_obs)) +
  geom_point(aes(colour = "data"), position = "jitter", alpha = 0.3, size = 1) +
  geom_violin(aes(group = buffer_size)) +
  stat_summary(fun = "median", geom = "point", aes(colour = "median")) +
  stat_summary(fun = "mean", geom = "point", aes(colour = "mean")) +
  scale_colour_manual("Summary statistic",
                      values = c("median" = "lightseagreen",
                                 "mean" = "coral",
                                 "data" = "lightgrey")) +
  theme_minimal() +
  labs(colour = "Summary Statistic",
       x = "Buffer size around location point (km)",
       y = "Number of observations",
       title = "2. Density of locations with given number of observations for each buffer size",
       subtitle = "A smaller buffer size results on most locations containing very few observations, and increasing
the buffer size increases both the maximum, average and spread of number of observations for 
each location.") +
  guides(legend.position = "right")
```

Plot of number of vessels and summary stats.

``` r
plot_vessels <- ggplot(buffer_check, aes(buffer_size, n_vessels)) +
  geom_point(aes(colour = "data"), position = "jitter", alpha = 0.3, size = 1) +
  geom_violin(aes(group = buffer_size)) +
  stat_summary(fun = "median", geom = "point", aes(colour = "median")) +
  stat_summary(fun = "mean", geom = "point", aes(colour = "mean")) +
  scale_colour_manual("Summary statistic",
                      values = c("median" = "lightseagreen",
                                 "mean" = "coral",
                                 "data" = "lightgrey")) +
  theme_minimal() +
  labs(colour = "Summary Statistic",
       x = "Buffer size around location point (km)",
       y = "Number of vessels",
       title = "3. Density of locations with given number of vessels for each buffer size",
       subtitle = "A smaller buffer size results on most locations containing very few vessels, and increasing
the buffer size increases both the maximum, average and spread of number of observations for 
each location.") +
  guides(legend.position = "right")
```

Now I create a few plots to visualise what the different buffer sizes
offer.

1.  plot\_circles shows the number of locations included for each buffer
    size. A larger buffer size will naturally capture more points, so we
    expect the more locations will be included (i.e. have overlapping
    ship observations) with increasing buffer size.
2.  plot\_obs is a violin plot showing the number of observations found
    at each location for each buffer size
3.  plot\_vessels is a violin plot showing the number of observations
    found at each location for each buffer size.

``` r
plot_circles
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/3-AntarcticLocations_files/figure-gfm/SUMMARISED%20PLOTs%20TO%20COMPARE%20BUFFER%20SIZE-1.png)<!-- -->

``` r
plot_obs
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/3-AntarcticLocations_files/figure-gfm/SUMMARISED%20PLOTs%20TO%20COMPARE%20BUFFER%20SIZE-2.png)<!-- -->

``` r
plot_vessels
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/3-AntarcticLocations_files/figure-gfm/SUMMARISED%20PLOTs%20TO%20COMPARE%20BUFFER%20SIZE-3.png)<!-- -->

Okay, so increasing the buffer size beyond 10km doesn’t really offer
much, but more information is added fairly rapidly up to about 5km. The
optimal buffer size is probably 5-10km. I have decided to go with 5km.
Given the close proximity (&lt;10km) of many locations (including those
listed as different places in the IAATO Antarctic Peninsula guide),
conversations with Kevin Hughes and looking at how well the point
location matches areas of high activity, it seemed the most sensible
option. Moreover, from a biological perspective, brooding benthic
invertebrates around Antarctica have shown population structure over
small distances (2-30kms) so distinguishing between places on this scale
seems relevant. With a 5km buffer zones,two locations could not be
closer than 10km without overlapping as each circle will a create 5km
radius.

## 5 Selecting the final locations

There are many overlapping circles, even with the 5km radius, and this
section explains the process of filtering out locations so that I have
locations all around Antarctica that the ships visited, without having
any overlapping circles (buffer zones). Given there are so many
locations so close together, it is an interative process removing first,
the unnecessary ones with 1km buffers, then 2km buffers, then 5km
buffers, and finally for the extremely busy areas around the Antarctic
peninsula, manually selecting non overlapping locations that best
represent the ship activity and known landing sites.

This process involves:  
a) Taking the visited circles with 1km buffer, creating a network and
using that to identify clusters of overlapping circles b) Then, for
clusters with fewer than 20 overlapping locations, select the top
location per cluster based on number of ships (and number of
observations if needed) c) For clusters with more than 20 overlapping
locations, all are kept

``` r
# First we take the
# clusters for 1km circles
circles_ant_1_all <- circles_ant_1
clusters_circles_1_all <- st_intersects(circles_ant_1, circles_ant_1)
circles_ant_1_all$circles_overlap <- clusters_circles_1_all
circles_ant_1_all <- circles_ant_1_all %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())

circles_ant_1_all <- circles_ant_1_all %>%
  left_join(so_count_1, by = "scar_common_id") %>%
  add_column(buffer_size = 1) %>%
   dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                -geometry,
                circle_id,
                lat_2,
                lon_2)

circles_overlap_1 <- as.data.frame(clusters_circles_1_all)

clusters_ant_1 <- graph_from_data_frame(d = circles_overlap_1, directed = FALSE)
igraph::write_graph(clusters_ant_1,
                    here("outputs", "clusters_1.graphml"), format = "graphml")

clu_1 <- components(clusters_ant_1)
circles_ant_1_all$cluster_id <- clu_1[[1]]

# Filtering for the 1km circles
circles_main_1 <- circles_ant_1_all %>%
  arrange(cluster_id, desc(n_vessels), desc(n_obs))

circles_main_1 <- circles_main_1 %>%
  group_by(cluster_id) %>%
  mutate(cluster_pos = 1:n())

circles_main_1 <- circles_main_1 %>%
  add_column("keep" = case_when(
    circles_main_1$cluster_pos == 1 ~ TRUE,
    TRUE ~ FALSE
  ))
large_clusters_1 <- circles_main_1 %>%
  group_by(cluster_id) %>%
  count() %>%
  filter(n > 10)
circles_main_check_1 <- circles_main_1 %>%
  filter(keep == TRUE | cluster_id %in% large_clusters_1$cluster_id)
```

Then do that again for 2km circles, filtering first to keep only the
points that remained after 1km filtering.

The initial joining between the check from the previous size (in this
case circles\_main\_check\_1), means that any locations excluded during
the 1m stage are excluded, but any locations in the 2km but not 1km list
are still included for evaluation.

``` r
# THis with the 2km circles that have first pass 1km overlapping circles removed
circles_ant_2_all <- circles_ant_2 %>%
  semi_join(as_tibble(circles_main_check_1), by = "scar_common_id")
clusters_circles_2_all <- st_intersects(circles_ant_2_all, circles_ant_2_all)
circles_ant_2_all$circles_overlap <- clusters_circles_2_all
circles_ant_2_all <- circles_ant_2_all %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())

circles_ant_2_all <- circles_ant_2_all %>%
  left_join(so_count_2, by = "scar_common_id") %>%
  add_column(buffer_size = 2) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                -geometry,
                circle_id,
                lat_2,
                lon_2,
                everything())

# Now for the clusters and filtering
# Now for the 2km area for finer scale stuff e.g. around the peninsula
circles_overlap_2 <- as.data.frame(clusters_circles_2_all)

clusters_ant_2 <- graph_from_data_frame(d = circles_overlap_2, directed = FALSE)
write_graph(clusters_ant_2, here("outputs", "clusters_2.graphml"), format = "graphml")

clu_2 <- components(clusters_ant_2)
circles_ant_2_all$cluster_id <- clu_2[[1]]

# Now for the 2km filtering
circles_main_2 <- circles_ant_2_all %>%
  arrange(cluster_id, desc(n_vessels), desc(n_obs))

circles_main_2 <- circles_main_2 %>%
  group_by(cluster_id) %>%
  mutate(cluster_pos = 1:n())

circles_main_2 <- circles_main_2 %>%
  add_column("keep" = case_when(
    circles_main_2$cluster_pos == 1 ~ TRUE,
    TRUE ~ FALSE
  ))
large_clusters_2 <- circles_main_2 %>%
  group_by(cluster_id) %>%
  count() %>%
  filter(n > 10)

circles_main_check_2 <- circles_main_2 %>%
  filter(keep == TRUE | cluster_id %in% large_clusters_2$cluster_id)
```

Now essentially the same again for the 5km buffer.

The circles\_ant\_v\_i, represents the circles around Antarctica that
were visited by ships, first pass at the filter. The manual stage
creates circles\_ant\_v\_ii (second pass).

``` r
# Starting with filtering from 1km and 2km discard locations
circles_ant_5_v_i <- circles_ant_5 %>%
  semi_join(as_tibble(trips_sf_5), by = "scar_common_id") %>%
  semi_join(as_tibble(circles_main_check_2), by = "scar_common_id")
clusters_circles_5 <- st_intersects(circles_ant_5_v_i, circles_ant_5_v_i)
circles_ant_5_v_i$circles_overlap <- clusters_circles_5
circles_ant_5_v_i <- circles_ant_5_v_i %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())

circles_ant_5_v_i <- circles_ant_5_v_i %>%
  left_join(so_count_5, by = "scar_common_id") %>%
  add_column(buffer_size = 5) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                -geometry,
                circle_id, lat_2, lon_2, everything())

# Now for the clusters
# Clusters for 5km circles after 2km filtering
circles_overlap_5 <- as.data.frame(clusters_circles_5)

clusters_ant_5 <- graph_from_data_frame(d = circles_overlap_5,
                                        directed = FALSE)
write_graph(clusters_ant_5,
            here("outputs", "clusters_5.graphml"), format = "graphml")

clu_5 <- components(clusters_ant_5)
circles_ant_5_v_i$cluster_id <- clu_5[[1]]

# Now for the filtering
# Now the 5km circles (after first filtering with 1km and 2km)
circles_main_5 <- circles_ant_5_v_i %>%
  arrange(cluster_id, desc(n_vessels), desc(n_obs))

circles_main_5 <- circles_main_5 %>%
  group_by(cluster_id) %>%
  mutate(cluster_pos = 1:n())

circles_main_5 <- circles_main_5 %>%
  add_column("keep" = case_when(
    circles_main_5$cluster_pos == 1 ~ TRUE,
    TRUE ~ FALSE
  ))
large_clusters_5 <- circles_main_5 %>%
  group_by(cluster_id) %>%
  count() %>%
  filter(n > 10)
circles_main_check_5 <- circles_main_5 %>%
  filter(keep == TRUE | cluster_id %in% large_clusters_5$cluster_id)
```

### Visualising and manually selecting final locations

To manually select the final locations I need to be able to visualise
them with all the ship observations from the Southern Ocean. This also
clearly demonstrates the clusters of locations and the problems that
overlapping circles create (on observation allocated to numerous
locations). Although I only have one map on display, the map was used
many times at all stages of the filtering process to check that it was
working as anticipated. I have opted for an interactive map because that
is the only way to reasonably visualise the locations and points at
different scales.

First step is to create an object of the observations from south of
-60°S to use in the map.

``` r
trips_so <- trips_sf %>%
  filter(lat_2 <= -60)
```

So I mapped all the remaining clusters (the same way that I created the
maps later on) and picked appropriate circles, still based on number of
ships and observations, but also ones that roughly matched the clumps of
observations. However there are still many locations that could, or
possibly should be included that aren’t. So, I remove all the circles
that overlap with the ones I have decided to keep. That should clear up
the map and reduce some of the giant clusters to more manageable ones.

``` r
circles_main_5_m <- circles_main_check_5 %>%
  filter(keep == TRUE | scar_common_id %in% c(14097, 492, 13615, 3029, 393, 16964, 5273, 15941, 15677, 13719, 5346, 9270, 3669, 12210, 10932, 11521, 9188, 1870, 14622, 13267, 3812, 10382, 6551, 17084, 1349, 12197, 8983, 10375, 3228, 5511, 337, 14097, 8672, 6621, 1193, 10637, 7582, 12681, 11516, 531, 16201, 3155, 1366, 1499, 6281, 11041, 3856, 9412, 9763, 9758, 3167, 9085, 103, 15275, 12294, 11485, 20, 19507, 5177, 4324, 14316, 16533, 16269, 1145, 12845, 14597, 11521, 1870, 9720, 4324, 14316, 16444)) %>% 
  mutate(keep = TRUE)
```

``` r
locs_to_keep <- circles_main_5_m$circle_id
locs_to_remove <-  circles_main_5_m$circles_overlap %>% 
  reduce(c) %>% 
  unique %>% 
  .[!. %in% locs_to_keep]

circles_main_check_5_2 <- circles_main_check_5 %>% 
  filter(!circle_id %in% locs_to_remove) %>% 
  mutate(keep = case_when(
    scar_common_id %in% circles_main_5_m$scar_common_id ~ TRUE,
    TRUE ~ as.logical(keep))) %>% 
  dplyr::select(-circle_id)
```

Now to do the clustering and filtering again with the reduced number of
locations

``` r
# Starting with filtering from 1km and 2km discard locations
clusters_circles_5_2 <- st_intersects(circles_main_check_5_2, circles_main_check_5_2)
circles_main_check_5_2$circles_overlap <- clusters_circles_5_2
circles_main_check_5_2 <- circles_main_check_5_2 %>%
  tibble::rowid_to_column("circle_id") %>%
  dplyr::select(circle_id, everything())

circles_main_check_5_2 <- circles_main_check_5_2 %>%
  left_join(so_count_5) %>%
  dplyr::select(scar_common_id,
                n_obs,
                n_vessels,
                buffer_size,
                circle_id, 
                lat_2, 
                lon_2, 
                everything())
```

    ## Joining, by = c("scar_common_id", "n_obs", "n_vessels")

``` r
# Now for the clusters
# Clusters for 5km circles after 2km filtering
circles_overlap_5_2 <- as.data.frame(clusters_circles_5_2)

clusters_circles_5_2 <- graph_from_data_frame(d = circles_overlap_5_2,
                                        directed = FALSE)
write_graph(clusters_circles_5_2,
            here("outputs", "clusters_5_2.graphml"), format = "graphml")

clu_5_2 <- components(clusters_circles_5_2)
circles_main_check_5_2$cluster_id <- clu_5_2[[1]]

# Now for the filtering
# Now the 5km circles (after first filtering with 1km and 2km)
circles_main_5_2 <- circles_main_check_5_2 %>%
  arrange(cluster_id, desc(n_vessels), desc(n_obs))

circles_main_5_2 <- circles_main_5_2 %>%
  group_by(cluster_id) %>%
  mutate(cluster_pos = 1:n())

circles_main_5_2 <- circles_main_5_2 %>%
  ungroup() %>% 
  mutate("keep" = case_when(
    circles_main_5_2$cluster_pos == 1 ~ TRUE, 
    TRUE ~ as.logical(keep)
  ))
large_clusters_5_2 <- circles_main_5_2 %>%
  group_by(cluster_id) %>%
  count() %>%
  filter(n > 1)
circles_main_check_5_3 <- circles_main_5_2 %>%
  filter(keep == TRUE | cluster_id %in% large_clusters_5_2$cluster_id)
```

Now, I map the remaining 621 locations and select the final few places.
11 clusters have more than 5 locations so I’ll look at those 11 and
manually select the final ones.

Next I create an object with all the information I want to include in
the map. Change the dataframe that is turned into ‘locs’ to
‘circles\_main\_check\_5\_3’ to see the locations at this stage or run
it later with circles\_main\_5\_3 to see final locations.

``` r
locs <- circles_main_5_3 %>%
  sf::st_transform(crs = 4326)
locs_geom <-circles_main_5_3 %>% 
  sf::st_transform(crs = 4326)
locs_geom <- st_coordinates(locs_geom$geometry)
locs_geom <- as_tibble(locs_geom) %>%
  rename(circle_id = L2) %>%
  left_join(circles_main_check_5_3, by = "circle_id")
```

Finally I create and then plot the map. This will appear in the plot
viewer (e.g. in RStudio) and will take a long time to load. Your
computer might struggle.

``` r
map_widget <- suppressWarnings(figure(width = 450,
                                height = 400,
                                padding_factor = 0) %>%
  ly_map(database = "world",
         regions = "antarctica",
         col = "gray") %>%
  ly_polygons(xs = X, ys = Y,
              data = locs_geom,
              group = circle_id,
              color = "powderblue") %>%
  ly_points(x = lon_2,
            y = lat_2,
            data = locs,
            color = as_factor(keep),
            hover = c(scar_common_id,
                      n_obs,
                      n_vessels,
                      cluster_id),
            size = 10) %>%
  ly_points(x = lon_2,
            y = lat_2,
            data = trips_so,
            size = 1,
            color = "teal")
    )
widgetframe::frameWidget(map_widget, width = 1200, height = 400)
```

That was helpful, from those 11 clusters, these are the location I want
to retain by SCAR ID: 7473, 2389, 3491, 405, 14920, 8295, 14631, 5099,
2960, 4508, 1933, 3038, 13658, 8720, 8502, 1196, 135588, 2477, 15127,
16979, 17895, 19079, 8870, 18515, 15500, 4165, 2302, 16134, 6018, 10917,
4518, 11728, 18741, 15993, 12675, 12663, 3308

``` r
locs_to_keep_2 <- c(7473, 2389, 3491, 405, 14920, 8295, 14631, 5099, 2960, 4508, 1933, 3038, 13658, 8720, 8502, 1196, 135588, 2477, 15127, 16979, 17895, 19079, 8870, 18515, 15500, 4165, 2302, 16134, 6018, 10917, 4518, 11728, 18741, 15993, 12675, 12663, 3308, 17400, 8770)

locs_to_remove_2 <- c(1499, 5177, 10932, 3669, 13267, 15941, 103, 1051, 1366, 16444, 16201, 13824, 7486, 15472, 5241, 12197, 9260, 7582)

circles_main_5_3 <- circles_main_check_5_3 %>% 
  mutate("keep" = case_when(
    scar_common_id %in% locs_to_keep_2 ~ TRUE, 
    scar_common_id %in% locs_to_remove_2 ~ FALSE,
    TRUE ~ as.logical(keep))) %>% 
  filter(keep == TRUE)

# See how many circles are STILL overlapping and
# if any more need to be removed
circles_check <- st_intersects(circles_main_5_3, circles_main_5_3)
circles_main_5_3$circles_overlap_2 <- circles_check
circles_main_5_3 <- circles_main_5_3 %>%
  dplyr::select(circles_overlap_2, -circles_overlap, everything())
#No overlapping circles remaining

circles_main <- st_difference(circles_main_5_3)
#Now I save a version that hasn't had ecoregions etc assigned
circles_ant_main <- circles_main %>%
  st_drop_geometry() %>%
  dplyr::select(-circles_overlap, -circles_overlap_2)
readr::write_csv(circles_ant_main,
                 here("cleaned_data", "circles_ant_main.csv"), col_names = TRUE)
# for very few circles there is an unavoidable, very small
# overlap but both circles should be kept so this removes that
```

Now assign observations to locations based on the final list of circles
(`circles_main`) and then also add the marine ecoregions of the world.

Due to issues with the data from Informa, I needed to manually sort all
the observations and put them in the correct order (many port calls were
not grouping together as they should). Because that had to be done
manually (outside R), I now need to import the verified observation file
and assign those observations to locations and ecoregions

``` r
# import the verified file and turn it into an sf object
trips_networks <- read_csv(here::here("data", "trips_networks_verified.csv")) %>% 
  clean_names() %>% 
  dplyr::select(-geometry)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   vessel_id = col_double(),
    ##   vessel_name = col_character(),
    ##   tms = col_datetime(format = ""),
    ##   date_time = col_datetime(format = ""),
    ##   move_id = col_double(),
    ##   estimated = col_character(),
    ##   qualifier = col_character(),
    ##   movement_type = col_character(),
    ##   move_sequence = col_double(),
    ##   port_stay = col_character(),
    ##   country = col_character(),
    ##   remove = col_character(),
    ##   lat_2 = col_double(),
    ##   lon_2 = col_double(),
    ##   geometry = col_character(),
    ##   place = col_character(),
    ##   date = col_character()
    ## )

``` r
trips_sf <- st_as_sf(trips_networks, 
                     coords = c("lon_2", "lat_2"),
                     crs = 4326,
                     remove = FALSE)
# check that the trips and circles have the same crs
st_crs(circles_main)
```

    ## Coordinate Reference System:
    ##   User input: EPSG:3031 
    ##   wkt:
    ## PROJCRS["WGS 84 / Antarctic Polar Stereographic",
    ##     BASEGEOGCRS["WGS 84",
    ##         DATUM["World Geodetic System 1984",
    ##             ELLIPSOID["WGS 84",6378137,298.257223563,
    ##                 LENGTHUNIT["metre",1]]],
    ##         PRIMEM["Greenwich",0,
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##         ID["EPSG",4326]],
    ##     CONVERSION["Antarctic Polar Stereographic",
    ##         METHOD["Polar Stereographic (variant B)",
    ##             ID["EPSG",9829]],
    ##         PARAMETER["Latitude of standard parallel",-71,
    ##             ANGLEUNIT["degree",0.0174532925199433],
    ##             ID["EPSG",8832]],
    ##         PARAMETER["Longitude of origin",0,
    ##             ANGLEUNIT["degree",0.0174532925199433],
    ##             ID["EPSG",8833]],
    ##         PARAMETER["False easting",0,
    ##             LENGTHUNIT["metre",1],
    ##             ID["EPSG",8806]],
    ##         PARAMETER["False northing",0,
    ##             LENGTHUNIT["metre",1],
    ##             ID["EPSG",8807]]],
    ##     CS[Cartesian,2],
    ##         AXIS["(E)",north,
    ##             MERIDIAN[90,
    ##                 ANGLEUNIT["degree",0.0174532925199433]],
    ##             ORDER[1],
    ##             LENGTHUNIT["metre",1]],
    ##         AXIS["(N)",north,
    ##             MERIDIAN[0,
    ##                 ANGLEUNIT["degree",0.0174532925199433]],
    ##             ORDER[2],
    ##             LENGTHUNIT["metre",1]],
    ##     USAGE[
    ##         SCOPE["Antarctic Digital Database and small scale topographic mapping."],
    ##         AREA["Antarctica."],
    ##         BBOX[-90,-180,-60,180]],
    ##     ID["EPSG",3031]]

``` r
st_crs(trips_sf)
```

    ## Coordinate Reference System:
    ##   User input: EPSG:4326 
    ##   wkt:
    ## GEOGCRS["WGS 84",
    ##     DATUM["World Geodetic System 1984",
    ##         ELLIPSOID["WGS 84",6378137,298.257223563,
    ##             LENGTHUNIT["metre",1]]],
    ##     PRIMEM["Greenwich",0,
    ##         ANGLEUNIT["degree",0.0174532925199433]],
    ##     CS[ellipsoidal,2],
    ##         AXIS["geodetic latitude (Lat)",north,
    ##             ORDER[1],
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##         AXIS["geodetic longitude (Lon)",east,
    ##             ORDER[2],
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##     USAGE[
    ##         SCOPE["Horizontal component of 3D system."],
    ##         AREA["World."],
    ##         BBOX[-90,-180,90,180]],
    ##     ID["EPSG",4326]]

``` r
trips_sf <- sf::st_transform(trips_sf, crs = 3031)

# then assign the observations a place (from the circles)
trips_sf_main <- st_join(trips_sf, circles_main["place"], left = TRUE) %>%
  mutate(place = coalesce(place.x, place.y)) %>%
  dplyr::select(-place.x, -place.y)

trips_sf_so <- trips_sf_main %>% filter(country == "Southern Ocean")

# check that the places selected (based on observations etc)
# are all present and allocated for in the trips object.
place_trips_sf_so <- as_tibble(unique(trips_sf_so$place))
place_circles <- as_tibble(unique(circles_main$place))
anti_join(place_circles, place_trips_sf_so) #all locations
```

    ## Joining, by = "value"

    ## # A tibble: 0 x 1
    ## # … with 1 variable: value <chr>

``` r
#selected do have observations

# The trips object actually has an extra location in
# the ATA because the port_locations_main has one 'port'
# in the ATA. Need to change 'Bahia Fildes' to 'Maxwell Bay'

trips_sf_main <- trips_sf_main %>% 
  mutate(place = case_when(place == "Bahia Fildes" ~ "Maxwell Bay", 
                           TRUE ~ as.character(place)))

#Now I allocate every southern ocean sighting to a MEOW region, if it overlaps with one

meow <- st_read(here("data", "MEOW", "meow_ecos.shp")) %>% 
  clean_names()
```

    ## Reading layer `meow_ecos' from data source `/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/data/MEOW/meow_ecos.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 232 features and 9 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -180 ymin: -89.9 xmax: 180 ymax: 86.9194
    ## geographic CRS: WGS 84

``` r
trips_sf_main <- sf::st_transform(trips_sf_main, crs = 4326)
trips_sf_main <- st_join(trips_sf_main, meow["ecoregion"])
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
trips_sf_main <- st_join(trips_sf_main, meow["province"])
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar
    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
trips_sf_main <- st_join(trips_sf_main, meow["realm"])
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar
    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
circles_main <- circles_main %>%
  st_drop_geometry() %>%
  dplyr::select(-circles_overlap, -circles_overlap_2)

readr::write_csv(circles_main,
                 here("cleaned_data", "circles_main.csv"), col_names = TRUE)
readr::write_csv(trips_sf_main,
                 here("cleaned_data", "trips_sf_main.csv"), col_names = TRUE)

trips_networks <-  trips_sf_main %>% 
  as_tibble() %>% 
  mutate(place = case_when(is.na(place) ~ "offshore",
         TRUE ~ as.character(place)),
    ecoregion = case_when(is.na(ecoregion) & lat_2 < -60 ~ "offshore",
                          TRUE ~ as.character(ecoregion)),
    province = case_when(is.na(province) & lat_2 < -60 ~ "offshore",
                         TRUE ~ as.character(province)),
    realm = case_when(place == "offshore" ~ "Southern Ocean",
                      TRUE ~ as.character(realm)))
readr::write_csv(trips_networks, here::here("cleaned_data", "trips_networks.csv") , col_names = TRUE)
#creating an "offshore" location for calculating Antarctic location stay times is really important and also for fishing vessels, but I also have some scenarios of NA and some of "offshore". Place == NA occurs when the location is in an ecoregion but not an Antarctic location.
```

Currently, I don’t have data for sub-Antarctic Islands unless there is a
port there, e.g. Grytviken. At a later point, if it seems necessary to
obtain the data for sub-ANtarctic islands, the following code produces a
shapefile with those regions.

``` r
extra_regions_for_informa <- meow %>%
  filter(ECO_CODE_X %in% c(189, 194, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 219, 230, 231, 232))
st_write(extra_regions_for_informa,
         dsn = "Extra port regions",
         layer = "Ecoregion",
         driver = "ESRI Shapefile")
```
