1 Importing Data and setting up master dataframes
================
Arlie McCarthy
17/06/2019

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(cowplot)
library(mapdata)
library(janitor)
library(here)
```

# Summary

The following code imports the data supplied by Informa and generates
‘master’ files (based on data cleaned to stage 3) that can be used for
later tages of cleaning.

# Importing data

Here I import the data for sightings (south of 60), movements (port
calls), vessels (vessel details). The cleaning was done manually in
excel and a ‘verified’ file of vessels (vessels\_verified) created with
a column stating whether or not to remove a ship. Ships were removed if
all of their sightings were on land (coastal/ambiguous cases were not
removed), or their port calls conflicted with their time in Antarctica.
Two cases that had many Antarctic sightings, with likely port calls at
either end of their journeys but a conflicting port call in the middle
were not removed during cleaning.

| Dataframe (raw data)   | Path                                                                                                    | Description                                                                                                                                                             |
|------------------------|---------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| vessels                | \~data/vessels\_verified.csv                                                                            | list of vessels from original data provided by Informa, with additional columns from manual cleaning process (no ships removed yet)                                     |
| sightings\_1st         | \~data/InformaDataRaw/ LLIDS3547/sightings.csv                                                          | location reads south of 60°S for all vessels (summarised raw data), from original data provided. All times in UTC                                                       |
| movements\_1st         | \~data/InformaDataRaw/ LLIDS3547\_movements/movements.csv                                               | worldwide port calls (raw data), from fifth set of data provided (includes date/time qualifiers, all times in UTC, and sequence id for sorting the port calls in order) |
| vessels\_2nd           | \~data/InformaDataRaw/ LLI\_Vessel\_positions\_recorded\_South\_60\_latitude\_2014-2018.xlsx            | vessel list from 2nd set of data provided by informa (not in exactly the same format as the first)                                                                      |
| vessels\_2nd\_verified | \~data/additional\_vessels\_verified.csv                                                                | verified list of vessels from 2nd set of data                                                                                                                           |
| port\_locations\_raw   | \~data/InformaDataRaw/ LLI\_Vessel\_positions\_recorded\_South\_60\_latitude\_2014-2018.xlsx            | coordinates for each of the ports mentioned in the movements dataframe                                                                                                  |
| sightings\_2nd\_201416 | \~data/InformaDataRaw/ LLI\_AIS\_Positions\_&\_Port\_Callings/ LLI\_AIS\_Positions\_S60\_2014\_2016.csv | location reads (not summarised) from 2014-16 for the 175 ships on the 2nd list of ships but not the first list                                                          |
| sightings\_2nd\_201718 | \~data/InformaDataRaw/ LLI\_AIS\_Positions\_&\_Port\_Callings /LLI\_AIS\_Positions\_S60\_2017\_2018.csv | location reads (not summarised) from 2017-18 for the 175 ships on the 2nd list of ships but not the first list                                                          |
| movements\_2nd         | \~data/InformaDataRaw/ LLI\_Port\_Callings\_2014\_2018.csv                                              | port call data for the additional 175 ships from 2014-2018. Provided in fourth issue of data.                                                                           |

``` r
sightings_1st <- read_csv(here::here("data", "InformaDataRaw", "LLIDS3547", "sightings.csv"),
  col_types = cols(
    `TO GMT` = col_datetime(format = "%d/%m/%Y %H:%M"),
    `FROM GMT` = col_datetime(format = "%d/%m/%Y %H:%M")
  )
)
vessels <- read_csv(here::here("data", "vessels_verified.csv")) %>% clean_names()

movements_1st <- read_csv(here::here("data", "InformaDataRaw", "2020-05-18-LLIDS3547_movements", "MOVEMENTS_ID_SEQ.csv"),
  col_types = cols(
    `ARRIVAL DATE` = col_datetime(format = "%d/%m/%Y %H:%M"),
    `SAILED DATE` = col_datetime(format = "%d/%m/%Y %H:%M")
  )
) %>% clean_names()
```

How many ships are there in the raw data first provided?

``` r
length(unique(vessels$vessel_id))
```

    ## [1] 865

After sending a list of questions to Informa about how the data are
generated and why so many ships seemed to be incorrectly included (see
description of cleaning and verifying ships in README file), I was sent
additional data: an excel spreadsheet with a list of ships based on the
raw (not summaraised) data orginially sent me (including position counts
per month, which was not included in original list).

The new sightings and port call files are:

``` r
vessels_2nd <- read_excel(here("data", "InformaDataRaw", "LLI_Vessel_positions_recorded_South_60_latitude_2014-2018.xlsx"),
  sheet = "Vessel List & Position Count"
)
vessels_2nd <- vessels_2nd %>% clean_names()
```

I then checked the new list against the vessels in the original list.
The second list of ships from Informa has 430 ships, but some of them
may overlap with the ships that were on the original list during QC. The
results of the next chunk were used to generate a list of ships that was
sent to Informa, who then provided the sightings and movement data for
the 175 ships that were on the new list but not on the original list.

``` r
test_vessels <- anti_join(vessels_2nd, vessels, by = "vessel_id")
length(unique(test_vessels$vessel_id))
```

    ## [1] 175

The next chunk generates and exports a list of the 175 vessels that are
not on the original list but are on the new list. test\_vessels has a
row for each ship in every month and year was active south of 60degS so
the following chunk creates a list of ships with only one row per ship
and exports it so that some manual QC (first pass at stages 1-3) can be
done and information added to FLAG and ACTIVITY\_TYPE variables, so that
it matches the information provided for the original list of ships.

``` r
extra_vessels_from_new_list <- test_vessels %>%
  group_by(vessel_id) %>%
  slice(1)
write.csv(extra_vessels_from_new_list,
  file = here("outputs", "additional_vessels.csv"), row.names = TRUE
)
rm(extra_vessels_from_new_list)
```

Once provided with the list of extra ships, Informa provided me with:

1.  a list of ports with lat/long coordinates; csv files with
    information on ships that were on the new list but not the original
    list of ships including
2.  raw position data south of 60 degrees in two files (2014-2016 and
    2017-2018),
3.  port call data in 5 files, one for each year.

So all that data needs importing as well as the new, verified data (some
ships removed).

``` r
# The list of vessels
vessels_2nd_verified <- read_csv(here("data", "additional_vessels_verified.csv"),
  col_types = cols(
    `#Positions reported South 60` = col_skip(),
    `Month/Year` = col_skip(), X1 = col_skip()
  )
)
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
# The list of ports/location and their lat/long coordinates
port_locations_raw <- read_excel(here(
  "data", "InformaDataRaw",
  "LLI_Vessel_positions_recorded_South_60_latitude_2014-2018.xlsx"
),
sheet = "LLI Places"
)

sightings_2nd_201416 <- read_csv(here("data", "InformaDataRaw", "LLI_AIS_Positions_&_Port_Callings", "LLI_AIS_Positions_S60_2014_2016.csv"),
  col_types = cols(`MESSAGE_TIMESTAMP` = col_datetime(format = "%d/%m/%Y %H:%M"))
)
sightings_2nd_201718 <- read_csv(here(
  "data", "InformaDataRaw", "LLI_AIS_Positions_&_Port_Callings",
  "LLI_AIS_Positions_S60_2017_2018.csv"
),
col_types = cols(`MESSAGE_TIMESTAMP` = col_datetime(format = "%d/%m/%Y %H:%M"))
)
# The list of observations
movements_2nd <- read_csv(here("data", "InformaDataRaw", "LLI_Port_Callings_2014_2018.csv"),
  col_types = cols(
    `ARRIVAL DATE` = col_datetime(format = "%d/%m/%Y %H:%M"),
    `SAILING DATE` = col_datetime(format = "%d/%m/%Y %H:%M")
  )
)
```

# Setting up master dataframes

This section creates the following list of data frames combining the
various different data I was provided with and filtering based on my
manual QC in excel.

| Dataframe (raw data)    | Description                                                                          |
|-------------------------|--------------------------------------------------------------------------------------|
| vessels\_master         | list of vessels from both sets of data                                               |
| sightings\_master       | location reads south of 60°S for QC vessels from both data sets                      |
| movements\_master       | worldwide port calls for QC vessels from both data sets, with lat/long for each port |
| port\_locations\_master | extra information about each port                                                    |

### Vessels

For most of the analysis, I am only interested in the ships on the
cleaned list so I need to make a ‘master’ list of ships from both the
old and new set of ships, and keep only the sightings and movements for
those ships.

``` r
vessels_2nd_verified <- vessels_2nd_verified %>% clean_names()
vessels_master <- bind_rows(vessels, vessels_2nd_verified) %>%
  filter(`remove` != "y")
# Number of ships after all the manual QC, including uncertain cases
length(unique(vessels_master$vessel_id))
```

    ## [1] 266

``` r
# Remove objects no longer needed to keep the environment tidy
rm(test_vessels, vessels, vessels_2nd, vessels_2nd_verified)
```

There are 266 ships (including uncertain cases) on the master list.

### Sightings (Southern Ocean Observations)

I am only interested in the sightings and movementments for the ships on
the master list so now I need to create master lists of sightings and
movements that combine the two sets of data I have been given and keep
only the ships on my master list.

``` r
sightings <- sightings_1st %>% clean_names()
sightings_2nd_201416 <- sightings_2nd_201416 %>% clean_names()
sightings_2nd_201718 <- sightings_2nd_201718 %>% clean_names()
# sightings only of ships from cleaned data
sightings <- semi_join(sightings, vessels_master, by = "vessel_id")
sightings_2nd <- bind_rows(sightings_2nd_201416, sightings_2nd_201718) %>%
  semi_join(vessels_master, by = "vessel_id")
# remove objects no longer needed
rm(sightings_1st, sightings_2nd_201416, sightings_2nd_201718)
```

Unfortunately, the original and second sets of sightings data are not in
the same format. The first has `FROM GMT` and `TO GMT` datetimes, which
are the start and end times for the ship at a given location rather than
potentially many reads at the same location. I will need to make a
separate row for each of these reads, so that I can use both. TO do this
I make two new dataframes, one for the TO and one for the FROM, keeping
only the essential information and renaming the TO and FROM variables as
date\_time. The new sightings data needs a date\_time column. Then they
are combined with the new sightings data.

``` r
sightings_to <- sightings %>%
  dplyr::select(vessel_id, to_gmt, latitude, longitude) %>%
  rename(date_time = to_gmt)

sightings_from <- sightings %>%
  dplyr::select(vessel_id, from_gmt, latitude, longitude) %>%
  rename(date_time = from_gmt)

sightings_2nd <- sightings_2nd %>%
  dplyr::select(-mmsi) %>%
  rename(date_time = message_timestamp)

sightings_master <- bind_rows(sightings_from, sightings_to, sightings_2nd) %>%
  distinct()

# remove objects no longer needed
rm(sightings, sightings_2nd, sightings_from, sightings_to)
```

Despite the cleaning already performed, I eyeballed the data for each
ship to identify additional ships that may have been missed, but should
have been removed based on my criteria above. This turned out to be
particularly true of ships from the second set of data because they
could have many Southern Ocean sighitings that are false positives. Due
to the iterative nature of the cleaning process, the next chunk of code
produces a pdf figure with the 265 ships. However, the first output
inluded 342 ships. Looking at the first revealed 131 vessels that need a
closer look due to unlikely-looking sighitngs, including 12 that had
sightings only on land. In the outputs folder roughmap\_ship is the
first output and the second \~(current) output is roughmap\_ship\_2. The
details of each ship examined and why they were removed or kept are in
the Scrivener file for this piece of work.

``` r
world <- map_data("world")
continents <- map_data("world", region = "antarctica")

figbase <- ggplot() +
  geom_polygon(
    data = continents,
    mapping = aes(x = long, y = lat, group = group),
    fill = "lightgrey"
  ) +
  theme_light() +
  coord_map("ortho", orientation = c(-90, 0, 0), ylim = c(-90, -60))

roughmap_ship <- figbase +
  geom_point(data = sightings_master, aes(x = longitude, y = latitude)) +
  facet_wrap(~vessel_id)
```

The figure is huge so the only practical way look at it was to export it
and view it as a pdf, but it did mean that the filtering was manual
(outside R). The next chunk is not run becuase it takes forever to save
the file, but can be run when opened in RStudio.

Save the output as a pdf and look at it in a pdf viewer rather than here
because it is such a large output that it looks terrible in the RStudio
window.

``` r
ggsave2(here("outputs", "roughmap_ship_2.pdf"),
  plot = roughmap_ship, width = 40, height = 60, units = "cm"
)
```

### Movements (port calls)

Now that I have a master vessels and sightings files, I need to create a
master file for the movement data. This will only keep movements for
ships identified to be kept or uncertain during cleaning, combine the
original and new movements data, and add the latitude and longitude for
each port.

I start by keeping only the movements for the ships I want, making sure
the variable names are the same, and combining the files to create a
master file.

``` r
movements <- movements_1st %>%
  clean_names() %>%
  semi_join(vessels_master, by = "vessel_id") %>%
  dplyr::select(-imo, -country, -move_id) %>%
  arrange(vessel_id)
movements_2 <- movements_2nd %>%
  clean_names() %>%
  rename(sailed_date = sailing_date) %>%
  dplyr::select(-move_id, -country) %>%
  semi_join(vessels_master, by = "vessel_id")
movements_master <- bind_rows(movements, movements_2)
# remove objects no longer needed
rm(movements_2nd, movements, movements_2)
```

So, we are very close to being able to combine the movements and
sightings data so that a trip can be created for each ship, that will
ultimately get turned into a beatiful network. But to do that, I need to
add the latitude and longitude from the port locations dataframe to the
movements\_master file. And then I need to create one dataframe that
combines both movements and sightings.

So I attach locations to the port calls, but many port calls actually
don’t have country information and these and some others also don’t have
coordinates. This needs to be fixed before I can move on.

### Ports

Again, this requires checking that the variable names are the same and
filtering the port locations so that I am left with only the ports
visited by my ships of interest. I also keep only the ports that my
ships actually went to.

``` r
port_locations <- port_locations_raw %>%
  clean_names() %>%
  rename(place = name)
movements_port_check <- left_join(movements_master, port_locations, "place")
port_locations <- semi_join(port_locations, movements_master, "place")
rm(port_locations_raw)
```

Next I identify to the ports that are missing information.

``` r
# which ports are missing coordinate or country information?
ports_missing <- movements_port_check %>%
  filter(is.na(latitude) | is.na(country)) %>%
  group_by(place) %>%
  slice(1)
# The original movements data had some country information so we'll
# add that to the ports_missing to make the next step easier
movements_ports <- movements_1st %>%
  dplyr::select(place, country) %>%
  group_by(place) %>%
  slice(1)

ports_missing <- left_join(ports_missing, movements_ports, by = "place")
```

Unfortunately, the 107 ports with missing information will need to be
manually checked against the exiting port dataand the locations will
need to be found. So I export this as a csv file which I’ll open in
another program to fill in.

``` r
write.csv(ports_missing, file = here("data", "ports_missing.csv"), row.names = FALSE)
```

All ports with missing information, except those that were listed only
as large countries, have had country, region and coordinate information
added. Most coordinates have come from searching MarineTraffic.com for
the port. When that was not available, a suitable anchorage or
lighthouse location was chosen in MarineTraffic.com or, failing that,
using Geohack. One Antarctic location was verified using the Antarctic
Place-names Gazette. This verified list of ports can now be uploaded and
combined with the existing list of ports.

``` r
# import the verified missing port data
ports_missing_verified <- read_csv(here("data", "ports_missing_verified.csv"),
  col_types = cols(
    LATITUDE = col_double(),
    LONGITUDE = col_double(),
    PLACE_ID = col_double()
  )
) %>%
  rename(country = COUNTRY.y) %>%
  dplyr::select(-COUNTRY.x) %>%
  clean_names()
# view the verified missing port data that are still missing coordinate or country information
ports_missing_verified %>%
  filter(is.na(latitude) | is.na(country)) %>%
  dplyr::select(
    place,
    country,
    area,
    movement_type,
    latitude,
    longitude,
    place_id
  ) %>%
  group_by(place)
```

    ## # A tibble: 7 x 7
    ## # Groups:   place [7]
    ##   place    country  area               movement_type latitude longitude place_id
    ##   <chr>    <chr>    <chr>              <chr>            <dbl>     <dbl>    <dbl>
    ## 1 China    China    <NA>               Port Call           NA        NA       NA
    ## 2 Greenla… Greenla… <NA>               Port Call           NA        NA       NA
    ## 3 Indones… Indones… <NA>               Port Call           NA        NA       NA
    ## 4 Newfoun… Canada   N.E. Canada and G… Port Call           NA        NA       NA
    ## 5 Queensl… Austral… Australia, New Ze… Port Call           NA        NA       NA
    ## 6 Venezue… Venezue… <NA>               Port Call           NA        NA       NA
    ## 7 Vietnam  Vietnam  <NA>               Port Call           NA        NA       NA

Ther are 7 places with missing information, but these are for areas at
such a large scale that they have no had information added. These
observations are be removed from further analyses later becuase they do
not have coordinate information.

Next, I add the new information for the missing ports to the
port\_locations file, and add rounded lat and lon variables so that the
coordinate accuracy is consistent.

``` r
port_locations_master <- full_join(
  port_locations, ports_missing_verified,
  by = "place"
) %>%
  mutate(
    country = coalesce(country.x, country.y),
    area = coalesce(area.x, area.y),
    latitude = coalesce(latitude.x, latitude.y),
    longitude = coalesce(longitude.x, longitude.y),
    place_id = coalesce(place_id.x, place_id.y)
  ) %>%
  dplyr::select(
    -country.x,
    -country.y,
    -area.x,
    -area.y,
    -latitude.x,
    -latitude.y,
    -longitude.x,
    -longitude.y,
    -place_id.x,
    -place_id.y
  )

port_locations_master <- port_locations_master %>%
  filter(!is.na(latitude))
```

Now, to make sure that th locations have been combined correctly I check
the number of distinct places based on coordinates and names.

``` r
length(unique(port_locations_master$place))
```

    ## [1] 1887

``` r
length(port_locations_master$place)
```

    ## [1] 1887

``` r
port_locations_master %>%
  arrange(latitude, longitude) %>%
  group_by(latitude, longitude) %>%
  filter(n() > 1)
```

    ## # A tibble: 18 x 9
    ## # Groups:   latitude, longitude [9]
    ##    place  movement_type source  notes  country area  latitude longitude place_id
    ##    <chr>  <chr>         <chr>   <chr>  <chr>   <chr>    <dbl>     <dbl>    <dbl>
    ##  1 Grytv… <NA>          <NA>    <NA>   Falkla… Sout…   -54.3     -36.5       574
    ##  2 South… Port Call     Inform… used … South … Sout…   -54.3     -36.5        NA
    ##  3 Noumea <NA>          <NA>    <NA>   New Ca… Aust…   -22.3     166.       7607
    ##  4 New C… Port Call     Marine… used … New Ca… Aust…   -22.3     166.         NA
    ##  5 Port … <NA>          <NA>    <NA>   Maurit… Sout…   -20.2      57.5      7603
    ##  6 Mauri… Port Call     Inform… used … Maurit… Sout…   -20.2      57.5        NA
    ##  7 Taraw… <NA>          <NA>    <NA>   Kiriba… Aust…     1.35    173.       2626
    ##  8 Betio  <NA>          <NA>    <NA>   Kiriba… Aust…     1.35    173.       8155
    ##  9 Panam… Port Call     Marine… used … Panama  Cent…     8.88    -79.5        NA
    ## 10 Panam… Port Call     Marine… <NA>   Panama  Cent…     8.88    -79.5        NA
    ## 11 St. V… <NA>          <NA>    <NA>   Cape V… Afri…    16.9     -25.0      1274
    ## 12 Minde… <NA>          <NA>    <NA>   Cape V… Afri…    16.9     -25.0      8154
    ## 13 Kerch… Passing       Marine… Used … Ukraine Blac…    45.3      36.5        NA
    ## 14 Kerch… Passing       Marine… Used … Ukraine Blac…    45.3      36.5        NA
    ## 15 Guern… <NA>          <NA>    <NA>   U.K.    Unit…    49.5      -2.54       49
    ## 16 St. P… <NA>          <NA>    <NA>   U.K.    Unit…    49.5      -2.54     8149
    ## 17 Torsh… <NA>          <NA>    <NA>   Faroe … Scan…    62.0      -6.77     1142
    ## 18 Faroes Port Call     GeoHack used … Faroe … Scan…    62.0      -6.77       NA

There are no duplicated rows, but 8 pairs of locations share
coordinates. This happened when I needed to use a main port or capital
city for the coordinates when a larger region was listed in the port
call inforamtion. All ports have unique names. Because of the way that
the port call data is generated, the names are the ‘gold standard’
rather than coordinates, and it is the names that are used to create the
network and in further analyses. The port list is left as is, for that
reason, the ports that have matching coordinates that I added manually
have their names changed in the movements files so that there is only
one name per set of coordinates.

Add the port location information to the master movements file,
replacing names where needed.

``` r
movements_master <- left_join(
  movements_master,
  port_locations_master, "place"
) %>%
  filter(!is.na("latitude")) %>%
  clean_names() %>%
  mutate(place = case_when(
    place == "South Georgia" ~ "Grytviken",
    place == "New Caledonia" ~ "Noumea",
    place == "Mauritius" ~ "Port Louis",
    place == "Panama Pacific light. area" ~ "Panama Pacific Anch.",
    place == "Mindelo" ~ "St. Vincent(CPV)",
    place == "St. Peter Port" ~ "Guernsey",
    place == "Faroes" ~ "Torshavn",
    place == "Kerch Strait" ~ "Kerchenskiy Strait",
    TRUE ~ as.character(place)
  ))
```

And now remove any dataframes that I don’t need anymore

``` r
rm(
  movements_1st,
  movements_ports,
  port_locations,
  ports_missing,
  ports_missing_verified
)
```

## Save master files

Now, so that I don’t need to run this code again, I now export all the
files in the data folder so that I can import the master files for any
of my next scripts.

``` r
write.csv(movements_master,
  file = here("data", "movements_master.csv"), row.names = FALSE
)
write.csv(sightings_master,
  file = here("data", "sightings_master.csv"), row.names = FALSE
)
write.csv(vessels_master,
  file = here("data", "vessels_master.csv"), row.names = FALSE
)
write.csv(port_locations_master,
  file = here("data", "port_locations_master.csv"), row.names = FALSE
)
```
