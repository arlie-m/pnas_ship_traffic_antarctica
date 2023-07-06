6-Network Analysis
================
Arlie McCarthy
25/03/2021

# Summary and set-up

The analysis of ship activity and use of networks stems from the
following aims to:

    •   Reveal the extent of the ship network connecting Antarctica to the rest of the world, in light of assumptions that activity is higher in certain locations (Antarctic Peninsula) and that 'Antarctic Gateway' ports are the primary conduits through which ships travel to the Southern Ocean.
        ⁃   For example, are official gateway cities really the "main international points of departure to and from the Antarctic region"? (https://www.circlesofsustainability.org/projects/antarctic-cities/)
        ⁃   Are the numbers of ships and voyages from Temperate South America (much) greater than to/from other areas of Antarctica and other parts of the world? (what magnitude of difference?)
    •   Quantify the intensity of ship activity in coastal Antarctic locations, to identify areas with higher pressure from human activity, including potential propagule pressure of non-native species. (AKA where do we need to start looking for invasive species?)
        ⁃   different spatial scales of maps showing number of visits to each place
        ⁃   table of most visited places/rank antarctic ports and ecoregions based on number of ships and also cumulative ship time spent in those places.
    •   Compare the activity of fishing, tourism and research/national operations spatially and temporally (risk of introduction for different industries)
        ⁃   essentially do all the above, but for different activity types and compare them. E.g. do the Gateway ports reflect all activity types?
    •   Identify key ports and ecological regions (Marine Ecoregions of the World as defined by [@Spalding2007]) around the world that are likely to contribute most to introductions, based on their connectivity to Antarctic locations

Packages and functions used in this script.

``` r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
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
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(here)
```

    ## here() starts at /Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis

``` r
library(mapview)
```

    ## GDAL version >= 3.1.0 | setting mapviewOptions(fgb = TRUE)

``` r
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     %--%, union

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
library(tidygraph)
```

    ## 
    ## Attaching package: 'tidygraph'

    ## The following object is masked from 'package:igraph':
    ## 
    ##     groups

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(ggmap)
```

    ## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.

    ## Please cite ggmap if you use it! See citation("ggmap") for details.

``` r
library(ggraph)
```

    ## 
    ## Attaching package: 'ggraph'

    ## The following object is masked from 'package:sp':
    ## 
    ##     geometry

``` r
library(mapdata)
```

    ## Loading required package: maps

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
library(maps)
library(rnaturalearth)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(ggsci)
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:ggmap':
    ## 
    ##     theme_nothing

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
source(here("scripts", "functions", "make_networks.R"))
source(here("scripts", "functions", "make_world_plots.R"))
source(here("scripts", "functions", "make_eco_plots.R"))
source(here("scripts", "functions", "gateway_importance.R"))
source(here("scripts", "functions", "gateway_regions.R"))
source(here("scripts", "functions", "make_antarctic_maps.R"))
```

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
source(here("scripts", "functions", "make_scotia_maps.R"))
```

    ## Loading required package: viridisLite

    ## although coordinates are longitude/latitude, st_intersection assumes that they are planar

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
source(here("scripts", "functions", "antarctic_risk.R"))
```

# Import master files for all ship networks

Files are imported as lists so that they can be easily used together.
Most analyses are performed on networks for all ships and for each
activity type so I wrote functions that allow me to do the analyses on
all networks in a list.

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

    ## Reading layer `meow_ecos' from data source `/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/data/MEOW/meow_ecos.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 232 features and 9 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -180 ymin: -89.9 xmax: 180 ymax: 86.9194
    ## geographic CRS: WGS 84

## Making the networks

I created a function (`make_networks`) that does the final preparation
for and creates directed networks using tidygraph for both the
port-to-port networks and ecoregion networks. The function also
calculates the centrality measures that will be used later on.

``` r
port_networks <- make_networks(edge_lists, nodes_lists, nodes_are_ports = TRUE)
ecoregion_networks <- make_networks(edge_eco_lists, nodes_eco_lists, nodes_are_ports = FALSE)
```

### First look at the port networks

``` r
port_networks$all
```

    ## # A tbl_graph: 1873 nodes and 12442 edges
    ## #
    ## # A directed multigraph with 2 components
    ## #
    ## # Node Data: 1,873 x 26 (active)
    ##   place country area  latitude longitude feature ecoregion province realm
    ##   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>    <chr>
    ## 1 Poin… U.S.A.  West…     71.3     -157. Port    Chukchi … Arctic   Arct…
    ## 2 Pevek Russia  Chin…     69.7      170. Port    East Sib… Arctic   Arct…
    ## 3 Anad… Russia  Chin…     64.7      178. Port    Eastern … Arctic   Arct…
    ## 4 Beri… Russia  Chin…     63.1      180. Port    Eastern … Arctic   Arct…
    ## 5 Egve… Russia  Chin…     66.3     -179. Port    Eastern … Arctic   Arct…
    ## 6 Fals… U.S.A.  West…     54.8     -163. Port    Eastern … Arctic   Arct…
    ## # … with 1,867 more rows, and 17 more variables: from_province <chr>,
    ## #   to_province <chr>, n_voyages <dbl>, n_ships <dbl>, n_trips <dbl>,
    ## #   total_time <dbl>, median_time <dbl>, mean_time <dbl>, n_time <dbl>,
    ## #   betweenness_centrality <dbl>, closeness_centrality <dbl>,
    ## #   strength_out <dbl>, strength_in <dbl>, strength_total <dbl>,
    ## #   centrality_eigen <dbl>, centrality_hub <dbl>, centrality_pagerank <dbl>
    ## #
    ## # Edge Data: 12,442 x 19
    ##    from    to move  from_place to_place from_ecoregion to_ecoregion
    ##   <int> <int> <chr> <chr>      <chr>    <chr>          <chr>       
    ## 1   154   155 'Eua… 'Eua Is.   Haapai   Tonga Islands  Tonga Islan…
    ## 2  1196  1204 12 M… 12 Mile A… Chalmet… Northern Gulf… Northern Gu…
    ## 3  1196  1215 12 M… 12 Mile A… New Orl… Northern Gulf… Northern Gu…
    ## # … with 12,439 more rows, and 12 more variables: from_province <chr>,
    ## #   to_province <chr>, from_realm <chr>, to_realm <chr>, n_voyages <dbl>,
    ## #   n_ships <dbl>, n_trips <dbl>, total_time <dbl>, median_time <dbl>,
    ## #   mean_time <dbl>, n_time <dbl>, internal <chr>

``` r
port_networks$fishing
```

    ## # A tbl_graph: 161 nodes and 552 edges
    ## #
    ## # A directed simple graph with 1 component
    ## #
    ## # Node Data: 161 x 26 (active)
    ##   place country area  latitude longitude feature ecoregion province realm
    ##   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>    <chr>
    ## 1 Anad… Russia  Chin…    64.7       178. Port    Eastern … Arctic   Arct…
    ## 2 Haik… China   Chin…    20.0       110. Port    Gulf of … South C… Cent…
    ## 3 Hong… China   Chin…    22.3       114. Port    Southern… South C… Cent…
    ## 4 Sing… Singap… Viet…     1.27      104. Port    Malacca … Sunda S… Cent…
    ## 5 Suva  Fiji    Aust…   -18.1       178. Port    Fiji Isl… Tropica… Cent…
    ## 6 Pape… French… Aust…   -17.5      -150. Port    Society … Southea… East…
    ## # … with 155 more rows, and 17 more variables: from_province <chr>,
    ## #   to_province <chr>, n_voyages <dbl>, n_ships <dbl>, n_trips <dbl>,
    ## #   total_time <dbl>, median_time <dbl>, mean_time <dbl>, n_time <dbl>,
    ## #   betweenness_centrality <dbl>, closeness_centrality <dbl>,
    ## #   strength_out <dbl>, strength_in <dbl>, strength_total <dbl>,
    ## #   centrality_eigen <dbl>, centrality_hub <dbl>, centrality_pagerank <dbl>
    ## #
    ## # Edge Data: 552 x 19
    ##    from    to move  from_place to_place from_ecoregion to_ecoregion
    ##   <int> <int> <chr> <chr>      <chr>    <chr>          <chr>       
    ## 1    13    58 Abov… Abovedada… Buen Ti… Antarctic Pen… South Shetl…
    ## 2    13    26 Abov… Abovedada… Delaite… Antarctic Pen… Antarctic P…
    ## 3    13    60 Abov… Abovedada… Dovizio… Antarctic Pen… South Shetl…
    ## # … with 549 more rows, and 12 more variables: from_province <chr>,
    ## #   to_province <chr>, from_realm <chr>, to_realm <chr>, n_voyages <dbl>,
    ## #   n_ships <dbl>, n_trips <dbl>, total_time <dbl>, median_time <dbl>,
    ## #   mean_time <dbl>, n_time <dbl>, internal <chr>

``` r
port_networks$tourism
```

    ## # A tbl_graph: 1284 nodes and 8206 edges
    ## #
    ## # A directed simple graph with 1 component
    ## #
    ## # Node Data: 1,284 x 26 (active)
    ##   place country area  latitude longitude feature ecoregion province realm
    ##   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>    <chr>
    ## 1 Anad… Russia  Chin…     64.7     178.  Port    Eastern … Arctic   Arct…
    ## 2 Egve… Russia  Chin…     66.3    -179.  Port    Eastern … Arctic   Arct…
    ## 3 Fals… U.S.A.  West…     54.8    -163.  Port    Eastern … Arctic   Arct…
    ## 4 Nome  U.S.A.  West…     64.5    -165.  Port    Eastern … Arctic   Arct…
    ## 5 St. … U.S.A.  West…     57.1    -170.  Port    Eastern … Arctic   Arct…
    ## 6 Bare… Svalba… Scan…     78.1      14.2 Port    North an… Arctic   Arct…
    ## # … with 1,278 more rows, and 17 more variables: from_province <chr>,
    ## #   to_province <chr>, n_voyages <dbl>, n_ships <dbl>, n_trips <dbl>,
    ## #   total_time <dbl>, median_time <dbl>, mean_time <dbl>, n_time <dbl>,
    ## #   betweenness_centrality <dbl>, closeness_centrality <dbl>,
    ## #   strength_out <dbl>, strength_in <dbl>, strength_total <dbl>,
    ## #   centrality_eigen <dbl>, centrality_hub <dbl>, centrality_pagerank <dbl>
    ## #
    ## # Edge Data: 8,206 x 19
    ##    from    to move  from_place to_place from_ecoregion to_ecoregion
    ##   <int> <int> <chr> <chr>      <chr>    <chr>          <chr>       
    ## 1    88    89 'Eua… 'Eua Is.   Haapai   Tonga Islands  Tonga Islan…
    ## 2   726   646 Aalb… Aalborg    Copenha… North Sea      Baltic Sea  
    ## 3   726   873 Aalb… Aalborg    Rosendal North Sea      Southern No…
    ## # … with 8,203 more rows, and 12 more variables: from_province <chr>,
    ## #   to_province <chr>, from_realm <chr>, to_realm <chr>, n_voyages <dbl>,
    ## #   n_ships <dbl>, n_trips <dbl>, total_time <dbl>, median_time <dbl>,
    ## #   mean_time <dbl>, n_time <dbl>, internal <chr>

``` r
port_networks$research
```

    ## # A tbl_graph: 441 nodes and 1503 edges
    ## #
    ## # A directed multigraph with 2 components
    ## #
    ## # Node Data: 441 x 26 (active)
    ##   place country area  latitude longitude feature ecoregion province realm
    ##   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>    <chr>
    ## 1 Poin… U.S.A.  West…     71.3    -157.  Port    Chukchi … Arctic   Arct…
    ## 2 Nome  U.S.A.  West…     64.5    -165.  Port    Eastern … Arctic   Arct…
    ## 3 Long… Svalba… Scan…     78.2      15.6 Port    North an… Arctic   Arct…
    ## 4 St. … Canada  N.E.…     47.6     -52.7 Port    Northern… Arctic   Arct…
    ## 5 Mani… Greenl… Scan…     65.4     -52.9 Port    West Gre… Arctic   Arct…
    ## 6 Nano… Greenl… Scan…     60.1     -45.2 Port    West Gre… Arctic   Arct…
    ## # … with 435 more rows, and 17 more variables: from_province <chr>,
    ## #   to_province <chr>, n_voyages <dbl>, n_ships <dbl>, n_trips <dbl>,
    ## #   total_time <dbl>, median_time <dbl>, mean_time <dbl>, n_time <dbl>,
    ## #   betweenness_centrality <dbl>, closeness_centrality <dbl>,
    ## #   strength_out <dbl>, strength_in <dbl>, strength_total <dbl>,
    ## #   centrality_eigen <dbl>, centrality_hub <dbl>, centrality_pagerank <dbl>
    ## #
    ## # Edge Data: 1,503 x 19
    ##    from    to move  from_place to_place from_ecoregion to_ecoregion
    ##   <int> <int> <chr> <chr>      <chr>    <chr>          <chr>       
    ## 1   286   288 Aalb… Aalborg    Dover S… North Sea      North Sea   
    ## 2   286   290 Aalb… Aalborg    Frederi… North Sea      North Sea   
    ## 3   286   300 Aalb… Aalborg    Lindo    North Sea      North Sea   
    ## # … with 1,500 more rows, and 12 more variables: from_province <chr>,
    ## #   to_province <chr>, from_realm <chr>, to_realm <chr>, n_voyages <dbl>,
    ## #   n_ships <dbl>, n_trips <dbl>, total_time <dbl>, median_time <dbl>,
    ## #   mean_time <dbl>, n_time <dbl>, internal <chr>

``` r
port_networks$supply
```

    ## # A tbl_graph: 827 nodes and 2923 edges
    ## #
    ## # A directed simple graph with 1 component
    ## #
    ## # Node Data: 827 x 26 (active)
    ##   place country area  latitude longitude feature ecoregion province realm
    ##   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>    <chr>
    ## 1 Pevek Russia  Chin…     69.7     170.  Port    East Sib… Arctic   Arct…
    ## 2 Anad… Russia  Chin…     64.7     178.  Port    Eastern … Arctic   Arct…
    ## 3 Beri… Russia  Chin…     63.1     180.  Port    Eastern … Arctic   Arct…
    ## 4 Egve… Russia  Chin…     66.3    -179.  Port    Eastern … Arctic   Arct…
    ## 5 Sabe… Russia  Scan…     71.3      72.1 Port    Kara Sea  Arctic   Arct…
    ## 6 Bare… Svalba… Scan…     78.1      14.2 Port    North an… Arctic   Arct…
    ## # … with 821 more rows, and 17 more variables: from_province <chr>,
    ## #   to_province <chr>, n_voyages <dbl>, n_ships <dbl>, n_trips <dbl>,
    ## #   total_time <dbl>, median_time <dbl>, mean_time <dbl>, n_time <dbl>,
    ## #   betweenness_centrality <dbl>, closeness_centrality <dbl>,
    ## #   strength_out <dbl>, strength_in <dbl>, strength_total <dbl>,
    ## #   centrality_eigen <dbl>, centrality_hub <dbl>, centrality_pagerank <dbl>
    ## #
    ## # Edge Data: 2,923 x 19
    ##    from    to move  from_place to_place from_ecoregion to_ecoregion
    ##   <int> <int> <chr> <chr>      <chr>    <chr>          <chr>       
    ## 1   413   421 12 M… 12 Mile A… Chalmet… Northern Gulf… Northern Gu…
    ## 2   413   432 12 M… 12 Mile A… New Orl… Northern Gulf… Northern Gu…
    ## 3   414   432 9 Mi… 9 Mile An… New Orl… Northern Gulf… Northern Gu…
    ## # … with 2,920 more rows, and 12 more variables: from_province <chr>,
    ## #   to_province <chr>, from_realm <chr>, to_realm <chr>, n_voyages <dbl>,
    ## #   n_ships <dbl>, n_trips <dbl>, total_time <dbl>, median_time <dbl>,
    ## #   mean_time <dbl>, n_time <dbl>, internal <chr>

``` r
port_networks$other
```

    ## # A tbl_graph: 126 nodes and 196 edges
    ## #
    ## # A directed simple graph with 1 component
    ## #
    ## # Node Data: 126 x 26 (active)
    ##   place country area  latitude longitude feature ecoregion province realm
    ##   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>    <chr>
    ## 1 Pevek Russia  Chin…     69.7     170.  Port    East Sib… Arctic   Arct…
    ## 2 Sabe… Russia  Scan…     71.3      72.1 Port    Kara Sea  Arctic   Arct…
    ## 3 Long… Svalba… Scan…     78.2      15.6 Port    North an… Arctic   Arct…
    ## 4 Vara… Russia  Scan…     68.8      58   Port    North an… Arctic   Arct…
    ## 5 St. … Canada  N.E.…     47.6     -52.7 Port    Northern… Arctic   Arct…
    ## 6 Nuuk  Greenl… Scan…     64.2     -51.7 Port    West Gre… Arctic   Arct…
    ## # … with 120 more rows, and 17 more variables: from_province <chr>,
    ## #   to_province <chr>, n_voyages <dbl>, n_ships <dbl>, n_trips <dbl>,
    ## #   total_time <dbl>, median_time <dbl>, mean_time <dbl>, n_time <dbl>,
    ## #   betweenness_centrality <dbl>, closeness_centrality <dbl>,
    ## #   strength_out <dbl>, strength_in <dbl>, strength_total <dbl>,
    ## #   centrality_eigen <dbl>, centrality_hub <dbl>, centrality_pagerank <dbl>
    ## #
    ## # Edge Data: 196 x 19
    ##    from    to move  from_place to_place from_ecoregion to_ecoregion
    ##   <int> <int> <chr> <chr>      <chr>    <chr>          <chr>       
    ## 1    58    63 Amst… Amsterdam  Delfzijl North Sea      North Sea   
    ## 2    58    64 Amst… Amsterdam  Den Hel… North Sea      North Sea   
    ## 3    58    65 Amst… Amsterdam  Dover S… North Sea      North Sea   
    ## # … with 193 more rows, and 12 more variables: from_province <chr>,
    ## #   to_province <chr>, from_realm <chr>, to_realm <chr>, n_voyages <dbl>,
    ## #   n_ships <dbl>, n_trips <dbl>, total_time <dbl>, median_time <dbl>,
    ## #   mean_time <dbl>, n_time <dbl>, internal <chr>

# Port networks

## Port network metrics

Average Degree

``` r
# Average degree
port_networks$all %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(strength_total))
```

    ## # A tibble: 1 x 1
    ##   `mean(strength_total)`
    ##                    <dbl>
    ## 1                   13.3

``` r
port_networks$fishing %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(strength_total))
```

    ## # A tibble: 1 x 1
    ##   `mean(strength_total)`
    ##                    <dbl>
    ## 1                   6.86

``` r
port_networks$tourism %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(strength_total))
```

    ## # A tibble: 1 x 1
    ##   `mean(strength_total)`
    ##                    <dbl>
    ## 1                   12.8

``` r
port_networks$research %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(strength_total))
```

    ## # A tibble: 1 x 1
    ##   `mean(strength_total)`
    ##                    <dbl>
    ## 1                   6.82

``` r
port_networks$supply %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(strength_total))
```

    ## # A tibble: 1 x 1
    ##   `mean(strength_total)`
    ##                    <dbl>
    ## 1                   7.07

``` r
port_networks$other %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(strength_total))
```

    ## # A tibble: 1 x 1
    ##   `mean(strength_total)`
    ##                    <dbl>
    ## 1                   3.11

Average betweenness centrality

``` r
# Average betweenness centrality
port_networks$all %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(betweenness_centrality))
```

    ## # A tibble: 1 x 1
    ##   `mean(betweenness_centrality)`
    ##                            <dbl>
    ## 1                        0.00181

``` r
port_networks$fishing %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(betweenness_centrality))
```

    ## # A tibble: 1 x 1
    ##   `mean(betweenness_centrality)`
    ##                            <dbl>
    ## 1                         0.0158

``` r
port_networks$tourism %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(betweenness_centrality))
```

    ## # A tibble: 1 x 1
    ##   `mean(betweenness_centrality)`
    ##                            <dbl>
    ## 1                        0.00320

``` r
port_networks$research %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(betweenness_centrality))
```

    ## # A tibble: 1 x 1
    ##   `mean(betweenness_centrality)`
    ##                            <dbl>
    ## 1                        0.00811

``` r
port_networks$supply %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(betweenness_centrality))
```

    ## # A tibble: 1 x 1
    ##   `mean(betweenness_centrality)`
    ##                            <dbl>
    ## 1                        0.00450

``` r
port_networks$other %>%
  activate(nodes) %>%
  as_tibble() %>%
  summarise(mean(betweenness_centrality))
```

    ## # A tibble: 1 x 1
    ##   `mean(betweenness_centrality)`
    ##                            <dbl>
    ## 1                         0.0462

Edge density

``` r
edge_density(port_networks$all, loops = FALSE)
```

    ## [1] 0.003548514

``` r
edge_density(port_networks$fishing, loops = FALSE)
```

    ## [1] 0.02142857

``` r
edge_density(port_networks$tourism, loops = FALSE)
```

    ## [1] 0.004981267

``` r
edge_density(port_networks$research, loops = FALSE)
```

    ## [1] 0.007745826

``` r
edge_density(port_networks$supply, loops = FALSE)
```

    ## [1] 0.00427901

``` r
edge_density(port_networks$other, loops = FALSE)
```

    ## [1] 0.01244444

number of cliques (fully connected clusters)

``` r
count_components(port_networks$all, mode = "strong")
```

    ## [1] 10

``` r
count_components(port_networks$all, mode = "weak")
```

    ## [1] 2

``` r
count_components(port_networks$fishing, mode = "strong")
```

    ## [1] 10

``` r
count_components(port_networks$fishing, mode = "weak")
```

    ## [1] 1

``` r
count_components(port_networks$tourism, mode = "strong")
```

    ## [1] 3

``` r
count_components(port_networks$tourism, mode = "weak")
```

    ## [1] 1

``` r
count_components(port_networks$research, mode = "strong")
```

    ## [1] 13

``` r
count_components(port_networks$research, mode = "weak")
```

    ## [1] 2

``` r
count_components(port_networks$supply, mode = "strong")
```

    ## [1] 11

``` r
count_components(port_networks$supply, mode = "weak")
```

    ## [1] 1

``` r
count_components(port_networks$other, mode = "strong")
```

    ## [1] 15

``` r
count_components(port_networks$other, mode = "weak")
```

    ## [1] 1

Closer look at clusters and membership in those clusters, for each
activity types.

``` r
port_networks$all <- port_networks$all %>%
  activate(nodes) %>%
  mutate(cluster = membership(cluster_infomap(port_networks$all)))

port_networks$all %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  distinct() %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    75

``` r
port_networks$all %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  group_by(cluster) %>%
  count()
```

    ## # A tibble: 75 x 2
    ## # Groups:   cluster [75]
    ##    cluster        n
    ##    <membrshp> <int>
    ##  1  1           226
    ##  2  2           137
    ##  3  3           108
    ##  4  4            92
    ##  5  5            83
    ##  6  6            49
    ##  7  7            49
    ##  8  8            47
    ##  9  9            61
    ## 10 10            59
    ## # … with 65 more rows

``` r
port_networks$fishing <- port_networks$fishing %>%
  activate(nodes) %>%
  mutate(cluster = membership(cluster_infomap(port_networks$fishing)))
port_networks$fishing %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  distinct() %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    14

``` r
port_networks$fishing %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  group_by(cluster) %>%
  count()
```

    ## # A tibble: 14 x 2
    ## # Groups:   cluster [14]
    ##    cluster        n
    ##    <membrshp> <int>
    ##  1  1            41
    ##  2  2            25
    ##  3  3            15
    ##  4  4            17
    ##  5  5            17
    ##  6  6            10
    ##  7  7             5
    ##  8  8             8
    ##  9  9             7
    ## 10 10             6
    ## 11 11             4
    ## 12 12             2
    ## 13 13             2
    ## 14 14             2

``` r
port_networks$tourism <- port_networks$tourism %>%
  activate(nodes) %>%
  mutate(cluster = membership(cluster_infomap(port_networks$tourism)))
port_networks$tourism %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  distinct() %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    50

``` r
port_networks$tourism %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  group_by(cluster) %>%
  count()
```

    ## # A tibble: 50 x 2
    ## # Groups:   cluster [50]
    ##    cluster        n
    ##    <membrshp> <int>
    ##  1  1           162
    ##  2  2            97
    ##  3  3            86
    ##  4  4            65
    ##  5  5            62
    ##  6  6            61
    ##  7  7            45
    ##  8  8            42
    ##  9  9            39
    ## 10 10            33
    ## # … with 40 more rows

``` r
port_networks$research <- port_networks$research %>%
  activate(nodes) %>%
  mutate(cluster = port_networks$all <- membership(cluster_infomap(port_networks$research)))
port_networks$research %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  distinct() %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    39

``` r
port_networks$research %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  group_by(cluster) %>%
  count()
```

    ## # A tibble: 39 x 2
    ## # Groups:   cluster [39]
    ##    cluster        n
    ##    <membrshp> <int>
    ##  1  1            56
    ##  2  2            50
    ##  3  3            35
    ##  4  4            25
    ##  5  5            20
    ##  6  6            23
    ##  7  7            17
    ##  8  8            13
    ##  9  9            10
    ## 10 10            14
    ## # … with 29 more rows

``` r
port_networks$supply <- port_networks$supply %>%
  activate(nodes) %>%
  mutate(cluster = membership(cluster_infomap(port_networks$supply)))
port_networks$supply %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  distinct() %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    75

``` r
port_networks$supply %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  group_by(cluster) %>%
  count()
```

    ## # A tibble: 75 x 2
    ## # Groups:   cluster [75]
    ##    cluster        n
    ##    <membrshp> <int>
    ##  1  1            59
    ##  2  2            48
    ##  3  3            38
    ##  4  4            42
    ##  5  5            35
    ##  6  6            41
    ##  7  7            38
    ##  8  8            24
    ##  9  9            30
    ## 10 10            27
    ## # … with 65 more rows

``` r
port_networks$other <- port_networks$other %>%
  activate(nodes) %>%
  mutate(cluster = membership(cluster_infomap(port_networks$other)))
port_networks$other %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  distinct() %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    21

``` r
port_networks$other %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(cluster) %>%
  group_by(cluster) %>%
  count()
```

    ## # A tibble: 21 x 2
    ## # Groups:   cluster [21]
    ##    cluster        n
    ##    <membrshp> <int>
    ##  1  1            14
    ##  2  2            10
    ##  3  3             9
    ##  4  4             7
    ##  5  5             8
    ##  6  6             8
    ##  7  7             6
    ##  8  8             8
    ##  9  9             7
    ## 10 10             6
    ## # … with 11 more rows

Number of ports outside Antarctica.

``` r
port_networks$all %>%
  activate(nodes) %>% 
  as_tibble() %>% 
  filter(realm != "Southern Ocean") %>% 
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  1581

``` r
port_networks$all %>%
  activate(nodes) %>% 
  as_tibble() %>% 
  filter(realm == "Southern Ocean") %>% 
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   286

## World maps of all the port networks

It is clear that Antarctic vessels really do travel the world and that
activity types do differ a little. There also appears to be substantial
traffic between Europe and South America through the Atlantic

``` r
world_port_maps <- make_plots(list_of_networks = port_networks)
plot(world_port_maps$all)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

And for each activity type

``` r
plot(world_port_maps$fishing)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
plot(world_port_maps$tourism)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
plot(world_port_maps$research)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
plot(world_port_maps$supply)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r
plot(world_port_maps$other)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->

# Ecoregion graphs

First, I generate points to connect ecoregions, rather than plotting
polygons. To do this I need to take the polygon geometry from the MEOW
objects and find the central point of the polygon

``` r
eco_points <- st_point_on_surface(MEOW) %>% 
      st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
eco_points_variables <- do.call(rbind, st_geometry(eco_points)) %>%
  as_tibble() %>%
  setNames(c("x_node", "y_node"))
eco_points_coords <- st_point_on_surface(MEOW) %>% 
      st_transform(crs = 4326)
eco_points_variables_coords <- do.call(rbind, st_geometry(eco_points_coords)) %>%
  as_tibble() %>%
  setNames(c("longitude", "latitude"))
eco_points <- eco_points %>%
  add_column(
    x_node = eco_points_variables$x_node,
    y_node = eco_points_variables$y_node,
    longitude = eco_points_variables_coords$longitude,
    latitude = eco_points_variables_coords$latitude
  )
eco_points$geometry <- NULL
```

Then I create the networks and plots for them. Just as I did for the
port network. I am less interested in the density, centrality etc
measure of these networks so I haven’t calculated them for the ecoregion
networks.

``` r
world_eco_maps <- make_eco_plots(
  list_of_networks = ecoregion_networks,
  points_for_layout = eco_points
)
plot(world_eco_maps$all)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
plot(world_eco_maps$fishing)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
plot(world_eco_maps$tourism)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
plot(world_eco_maps$research)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
plot(world_eco_maps$supply)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-12-5.png)<!-- -->

``` r
plot(world_eco_maps$other)
```

![](/Users/arlie/Documents/PhD_work/Chapters_thesis_papers/2_Ship_pathway_analysis/pathway_analysis/workflow/6-NetworkAnalyses_files/figure-gfm/unnamed-chunk-12-6.png)<!-- -->

# Aim 1: testing assumptions on activity and importance of ports

## Gateway Cities

First, are official Gateway cities the main ports of departure for ships
going to Antarctica? To find this out I need to count the number of
ships going to Antarctica from ANY ports with direct links to Antarctic
locations.

I created a function to a subset the port\_network graph so that it only
has edges TO the Southern Ocean and used the subsetted graph
re-calculate out-strength. Out-strength now reflects the number of
voyages from that port to the Southern Ocean within my 5-year time
period. This was repeated for each of the different activity types. The
function then creates a table of the gateway ports ranked from highest
to lowest by number of departures.

``` r
gateway_ports_results <- gateway_importance(port_networks)
gateway_ports_results$all
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
place
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
country
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
area
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
latitude
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
longitude
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
feature
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
betweenness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
closeness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_total
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_eigen
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_hub
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_pagerank
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
cluster
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
.tidygraph\_node\_index
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
Ushuaia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
718
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-54.801944
</td>
<td style="text-align:right;">
-68.3027778
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
1132
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
92028900
</td>
<td style="text-align:right;">
61860
</td>
<td style="text-align:right;">
135137.89
</td>
<td style="text-align:right;">
681
</td>
<td style="text-align:right;">
0.0633296
</td>
<td style="text-align:right;">
0.1254608
</td>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
243
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.0065769
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1447
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
Stanley Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
212
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.683333
</td>
<td style="text-align:right;">
-57.8333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
461
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
93807060
</td>
<td style="text-align:right;">
37860
</td>
<td style="text-align:right;">
252849.22
</td>
<td style="text-align:right;">
371
</td>
<td style="text-align:right;">
0.0445123
</td>
<td style="text-align:right;">
0.1249416
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:right;">
175
</td>
<td style="text-align:right;">
0.7232937
</td>
<td style="text-align:right;">
0.8478881
</td>
<td style="text-align:right;">
0.0040189
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1459
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Punta Arenas
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
163
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-53.166667
</td>
<td style="text-align:right;">
-70.9000000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
483
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:right;">
1040288820
</td>
<td style="text-align:right;">
345330
</td>
<td style="text-align:right;">
2626991.97
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.0923236
</td>
<td style="text-align:right;">
0.1255870
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
200
</td>
<td style="text-align:right;">
0.6967879
</td>
<td style="text-align:right;">
0.6109293
</td>
<td style="text-align:right;">
0.0066130
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1446
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Puerto Williams
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
124
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-54.933333
</td>
<td style="text-align:right;">
-67.6166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
226
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
20728620
</td>
<td style="text-align:right;">
50100
</td>
<td style="text-align:right;">
265751.54
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
0.0045090
</td>
<td style="text-align:right;">
0.1197850
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:right;">
0.4322165
</td>
<td style="text-align:right;">
0.6671451
</td>
<td style="text-align:right;">
0.0017021
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1445
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
Cape Town
</td>
<td style="text-align:left;">
South Africa
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-33.916667
</td>
<td style="text-align:right;">
18.4333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:right;">
179
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
56
</td>
<td style="text-align:right;">
339895500
</td>
<td style="text-align:right;">
423930
</td>
<td style="text-align:right;">
2393630.28
</td>
<td style="text-align:right;">
142
</td>
<td style="text-align:right;">
0.0652400
</td>
<td style="text-align:right;">
0.1270876
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
119
</td>
<td style="text-align:right;">
0.2415621
</td>
<td style="text-align:right;">
0.2231377
</td>
<td style="text-align:right;">
0.0038608
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
1539
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
Port William
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.666667
</td>
<td style="text-align:right;">
-57.7833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
262
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
33725400
</td>
<td style="text-align:right;">
7200
</td>
<td style="text-align:right;">
188410.06
</td>
<td style="text-align:right;">
179
</td>
<td style="text-align:right;">
0.0159196
</td>
<td style="text-align:right;">
0.1249750
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
0.3805145
</td>
<td style="text-align:right;">
0.3473966
</td>
<td style="text-align:right;">
0.0022437
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1458
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
Montevideo
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.900000
</td>
<td style="text-align:right;">
-56.2666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
481
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:right;">
98
</td>
<td style="text-align:right;">
286114560
</td>
<td style="text-align:right;">
84270
</td>
<td style="text-align:right;">
650260.36
</td>
<td style="text-align:right;">
440
</td>
<td style="text-align:right;">
0.0421971
</td>
<td style="text-align:right;">
0.1260946
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
0.4361499
</td>
<td style="text-align:right;">
0.4522382
</td>
<td style="text-align:right;">
0.0030350
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1498
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
Hobart
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-42.880000
</td>
<td style="text-align:right;">
147.3347222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
173044800
</td>
<td style="text-align:right;">
531840
</td>
<td style="text-align:right;">
3845440.00
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.0239954
</td>
<td style="text-align:right;">
0.1183013
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
0.0344683
</td>
<td style="text-align:right;">
0.0325634
</td>
<td style="text-align:right;">
0.0024109
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
523
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
Lyttelton
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-43.606389
</td>
<td style="text-align:right;">
172.7280556
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
70
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
36260340
</td>
<td style="text-align:right;">
227940
</td>
<td style="text-align:right;">
824098.64
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
0.0453610
</td>
<td style="text-align:right;">
0.1227300
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
0.0285876
</td>
<td style="text-align:right;">
0.0569459
</td>
<td style="text-align:right;">
0.0029303
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
528
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
Berkeley Sound
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.568500
</td>
<td style="text-align:right;">
-57.9350000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
217
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
56171160
</td>
<td style="text-align:right;">
180000
</td>
<td style="text-align:right;">
371994.44
</td>
<td style="text-align:right;">
151
</td>
<td style="text-align:right;">
0.0172043
</td>
<td style="text-align:right;">
0.1238013
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
0.2079869
</td>
<td style="text-align:right;">
0.1962151
</td>
<td style="text-align:right;">
0.0021764
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1455
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
Bluff
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-46.616667
</td>
<td style="text-align:right;">
168.3666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
20787240
</td>
<td style="text-align:right;">
126030
</td>
<td style="text-align:right;">
866135.00
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0047416
</td>
<td style="text-align:right;">
0.1117545
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
0.0032323
</td>
<td style="text-align:right;">
0.0022358
</td>
<td style="text-align:right;">
0.0017689
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
535
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
Mare Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.916667
</td>
<td style="text-align:right;">
-58.4666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2947080
</td>
<td style="text-align:right;">
213900
</td>
<td style="text-align:right;">
245590.00
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.0032631
</td>
<td style="text-align:right;">
0.1180924
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.1491757
</td>
<td style="text-align:right;">
0.1808846
</td>
<td style="text-align:right;">
0.0006459
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1457
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
Montevideo Alpha Zone
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-35.044610
</td>
<td style="text-align:right;">
-56.0587700
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
325
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
181217940
</td>
<td style="text-align:right;">
119190
</td>
<td style="text-align:right;">
604059.80
</td>
<td style="text-align:right;">
300
</td>
<td style="text-align:right;">
0.0569748
</td>
<td style="text-align:right;">
0.1265292
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:right;">
0.2875728
</td>
<td style="text-align:right;">
0.2414593
</td>
<td style="text-align:right;">
0.0037786
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1499
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
Buenos Aires
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.590278
</td>
<td style="text-align:right;">
-58.3775000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
147
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
97445220
</td>
<td style="text-align:right;">
141030
</td>
<td style="text-align:right;">
798731.31
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
0.0053304
</td>
<td style="text-align:right;">
0.1216058
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
0.2511624
</td>
<td style="text-align:right;">
0.2781807
</td>
<td style="text-align:right;">
0.0015365
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1492
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
Cabo Negro
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-52.950000
</td>
<td style="text-align:right;">
-70.7833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
299220
</td>
<td style="text-align:right;">
69510
</td>
<td style="text-align:right;">
74805.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0008553
</td>
<td style="text-align:right;">
0.1133446
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.0535043
</td>
<td style="text-align:right;">
0.0756199
</td>
<td style="text-align:right;">
0.0003732
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1441
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
Bintulu
</td>
<td style="text-align:left;">
Malaysia
</td>
<td style="text-align:left;">
Sunda Shelf/Java Sea
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Vietnam, Thailand, Malaysia and Indonesia
</td>
<td style="text-align:right;">
3.166667
</td>
<td style="text-align:right;">
113.0333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
39660
</td>
<td style="text-align:right;">
39660
</td>
<td style="text-align:right;">
39660.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0000773
</td>
<td style="text-align:right;">
0.1102084
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0193285
</td>
<td style="text-align:right;">
0.0269509
</td>
<td style="text-align:right;">
0.0002661
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
125
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
Las Palmas
</td>
<td style="text-align:left;">
Canary Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Spain / Portugal inc Atlantic Islands
</td>
<td style="text-align:right;">
28.133056
</td>
<td style="text-align:right;">
-15.4319444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:right;">
202
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
143319180
</td>
<td style="text-align:right;">
118200
</td>
<td style="text-align:right;">
770533.23
</td>
<td style="text-align:right;">
186
</td>
<td style="text-align:right;">
0.0657576
</td>
<td style="text-align:right;">
0.1258826
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:right;">
149
</td>
<td style="text-align:right;">
0.2178712
</td>
<td style="text-align:right;">
0.1638702
</td>
<td style="text-align:right;">
0.0050938
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
649
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
Busan
</td>
<td style="text-align:left;">
South Korea
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
35.117222
</td>
<td style="text-align:right;">
129.0444444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
276
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
201356640
</td>
<td style="text-align:right;">
308700
</td>
<td style="text-align:right;">
864191.59
</td>
<td style="text-align:right;">
233
</td>
<td style="text-align:right;">
0.0433371
</td>
<td style="text-align:right;">
0.1217640
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
95
</td>
<td style="text-align:right;">
0.0563119
</td>
<td style="text-align:right;">
0.0710715
</td>
<td style="text-align:right;">
0.0034927
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1408
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
Mar del Plata
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-38.033056
</td>
<td style="text-align:right;">
-57.5447222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1447200
</td>
<td style="text-align:right;">
94020
</td>
<td style="text-align:right;">
131563.64
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0019870
</td>
<td style="text-align:right;">
0.1157342
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
0.2204177
</td>
<td style="text-align:right;">
0.0835426
</td>
<td style="text-align:right;">
0.0010527
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1527
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
Talcahuano
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-36.693056
</td>
<td style="text-align:right;">
-73.0986111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
58672860
</td>
<td style="text-align:right;">
1037010
</td>
<td style="text-align:right;">
2444702.50
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0026803
</td>
<td style="text-align:right;">
0.1169488
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.0617176
</td>
<td style="text-align:right;">
0.1345736
</td>
<td style="text-align:right;">
0.0009437
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1468
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
Callao
</td>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-12.048056
</td>
<td style="text-align:right;">
-77.1425000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
34151640
</td>
<td style="text-align:right;">
107400
</td>
<td style="text-align:right;">
443527.79
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
0.0049358
</td>
<td style="text-align:right;">
0.1196931
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
0.0232497
</td>
<td style="text-align:right;">
0.0716267
</td>
<td style="text-align:right;">
0.0011213
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1477
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Vietnam, Thailand, Malaysia and Indonesia
</td>
<td style="text-align:right;">
1.268889
</td>
<td style="text-align:right;">
103.8330556
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:right;">
176
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
25993500
</td>
<td style="text-align:right;">
52110
</td>
<td style="text-align:right;">
158496.95
</td>
<td style="text-align:right;">
164
</td>
<td style="text-align:right;">
0.1586519
</td>
<td style="text-align:right;">
0.1247085
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
171
</td>
<td style="text-align:right;">
0.1083365
</td>
<td style="text-align:right;">
0.1183744
</td>
<td style="text-align:right;">
0.0074888
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
117
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
Sydney
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Manning-Hawkesbury
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-33.857500
</td>
<td style="text-align:right;">
151.2063889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
7848900
</td>
<td style="text-align:right;">
176220
</td>
<td style="text-align:right;">
413100.00
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0.0051545
</td>
<td style="text-align:right;">
0.1099689
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
0.0031308
</td>
<td style="text-align:right;">
0.0026404
</td>
<td style="text-align:right;">
0.0012889
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
511
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
Fremantle
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-32.059167
</td>
<td style="text-align:right;">
115.7508333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
5321040
</td>
<td style="text-align:right;">
285060
</td>
<td style="text-align:right;">
295613.33
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.0127652
</td>
<td style="text-align:right;">
0.1190915
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.0071753
</td>
<td style="text-align:right;">
0.0345285
</td>
<td style="text-align:right;">
0.0014415
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
542
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
La Plata Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.745000
</td>
<td style="text-align:right;">
-57.8311111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
2295840
</td>
<td style="text-align:right;">
44400
</td>
<td style="text-align:right;">
85031.11
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.0037545
</td>
<td style="text-align:right;">
0.1172492
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.0878967
</td>
<td style="text-align:right;">
0.0920901
</td>
<td style="text-align:right;">
0.0009020
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1497
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
Timaru
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-44.383333
</td>
<td style="text-align:right;">
171.2500000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
91295580
</td>
<td style="text-align:right;">
4660620
</td>
<td style="text-align:right;">
8299598.18
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0007215
</td>
<td style="text-align:right;">
0.1099947
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.0023053
</td>
<td style="text-align:right;">
0.0024840
</td>
<td style="text-align:right;">
0.0006401
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
533
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
Dunedin
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-45.883333
</td>
<td style="text-align:right;">
170.5166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
2277420
</td>
<td style="text-align:right;">
92130
</td>
<td style="text-align:right;">
103519.09
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0013273
</td>
<td style="text-align:right;">
0.1120086
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.0025789
</td>
<td style="text-align:right;">
0.0030994
</td>
<td style="text-align:right;">
0.0008474
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
536
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
Dover Strait
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.020920
</td>
<td style="text-align:right;">
1.3979300
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
300
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
53940
</td>
<td style="text-align:right;">
26970
</td>
<td style="text-align:right;">
26970.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.1063554
</td>
<td style="text-align:right;">
0.1280613
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:right;">
195
</td>
<td style="text-align:right;">
0.1287739
</td>
<td style="text-align:right;">
0.2108743
</td>
<td style="text-align:right;">
0.0064365
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1034
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
Nakhodka
</td>
<td style="text-align:left;">
Russia
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
42.798611
</td>
<td style="text-align:right;">
132.8769444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
5085240
</td>
<td style="text-align:right;">
266400
</td>
<td style="text-align:right;">
462294.55
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0015546
</td>
<td style="text-align:right;">
0.1160570
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0068342
</td>
<td style="text-align:right;">
0.0387870
</td>
<td style="text-align:right;">
0.0008189
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1302
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
Zhoushan
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
30.000000
</td>
<td style="text-align:right;">
122.1000000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
132916500
</td>
<td style="text-align:right;">
1630440
</td>
<td style="text-align:right;">
2712581.63
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
0.0083196
</td>
<td style="text-align:right;">
0.1167446
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.0323581
</td>
<td style="text-align:right;">
0.0401773
</td>
<td style="text-align:right;">
0.0020230
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1438
</td>
</tr>
<tr>
<td style="text-align:left;">
31
</td>
<td style="text-align:left;">
San Antonio(CHL)
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-33.600000
</td>
<td style="text-align:right;">
-71.6166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3844800
</td>
<td style="text-align:right;">
50040
</td>
<td style="text-align:right;">
213600.00
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.0021536
</td>
<td style="text-align:right;">
0.1150372
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0628266
</td>
<td style="text-align:right;">
0.0869422
</td>
<td style="text-align:right;">
0.0007813
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1467
</td>
</tr>
<tr>
<td style="text-align:left;">
32
</td>
<td style="text-align:left;">
Rio Grande(BRA)
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-32.166667
</td>
<td style="text-align:right;">
-52.0833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
4447680
</td>
<td style="text-align:right;">
97680
</td>
<td style="text-align:right;">
103434.42
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.0024300
</td>
<td style="text-align:right;">
0.1187968
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0.0791623
</td>
<td style="text-align:right;">
0.1386731
</td>
<td style="text-align:right;">
0.0014001
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1505
</td>
</tr>
<tr>
<td style="text-align:left;">
33
</td>
<td style="text-align:left;">
Recalada Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-39.000000
</td>
<td style="text-align:right;">
-61.2666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
144
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
28050240
</td>
<td style="text-align:right;">
97380
</td>
<td style="text-align:right;">
252704.86
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:right;">
0.0016961
</td>
<td style="text-align:right;">
0.1167301
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0.1956967
</td>
<td style="text-align:right;">
0.1008033
</td>
<td style="text-align:right;">
0.0014139
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1529
</td>
</tr>
<tr>
<td style="text-align:left;">
34
</td>
<td style="text-align:left;">
Hong Kong
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
22.286111
</td>
<td style="text-align:right;">
114.1575000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
9694860
</td>
<td style="text-align:right;">
62280
</td>
<td style="text-align:right;">
122719.75
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:right;">
0.0226553
</td>
<td style="text-align:right;">
0.1202467
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
0.0147290
</td>
<td style="text-align:right;">
0.0464588
</td>
<td style="text-align:right;">
0.0033722
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
80
</td>
</tr>
<tr>
<td style="text-align:left;">
35
</td>
<td style="text-align:left;">
Huangpu
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
23.097500
</td>
<td style="text-align:right;">
113.4241667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
38059800
</td>
<td style="text-align:right;">
1371060
</td>
<td style="text-align:right;">
1654773.91
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.0042219
</td>
<td style="text-align:right;">
0.1134752
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0297128
</td>
<td style="text-align:right;">
0.0135357
</td>
<td style="text-align:right;">
0.0009803
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
81
</td>
</tr>
<tr>
<td style="text-align:left;">
36
</td>
<td style="text-align:left;">
Xiamen
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
24.457500
</td>
<td style="text-align:right;">
118.0713889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
7158960
</td>
<td style="text-align:right;">
58680
</td>
<td style="text-align:right;">
650814.55
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0031432
</td>
<td style="text-align:right;">
0.1136957
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
0.0010678
</td>
<td style="text-align:right;">
0.0102104
</td>
<td style="text-align:right;">
0.0009309
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
90
</td>
</tr>
<tr>
<td style="text-align:left;">
37
</td>
<td style="text-align:left;">
Delfzijl
</td>
<td style="text-align:left;">
Netherlands
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
North European Atlantic coast
</td>
<td style="text-align:right;">
53.333333
</td>
<td style="text-align:right;">
6.9333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
5265780
</td>
<td style="text-align:right;">
684090
</td>
<td style="text-align:right;">
877630.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0034653
</td>
<td style="text-align:right;">
0.1134821
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0.0236109
</td>
<td style="text-align:right;">
0.0248191
</td>
<td style="text-align:right;">
0.0007415
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1028
</td>
</tr>
<tr>
<td style="text-align:left;">
38
</td>
<td style="text-align:left;">
Rochester(GBR)
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.387500
</td>
<td style="text-align:right;">
0.5097222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
93600
</td>
<td style="text-align:right;">
93600
</td>
<td style="text-align:right;">
93600.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0001640
</td>
<td style="text-align:right;">
0.1104360
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0264197
</td>
<td style="text-align:right;">
0.0234085
</td>
<td style="text-align:right;">
0.0002168
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
1097
</td>
</tr>
<tr>
<td style="text-align:left;">
39
</td>
<td style="text-align:left;">
Sheerness
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.437778
</td>
<td style="text-align:right;">
0.7438889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
397500
</td>
<td style="text-align:right;">
198750
</td>
<td style="text-align:right;">
198750.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0016633
</td>
<td style="text-align:right;">
0.1178916
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0070646
</td>
<td style="text-align:right;">
0.0272848
</td>
<td style="text-align:right;">
0.0004125
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
1105
</td>
</tr>
<tr>
<td style="text-align:left;">
40
</td>
<td style="text-align:left;">
Thamesport
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.434167
</td>
<td style="text-align:right;">
0.6863889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0000919
</td>
<td style="text-align:right;">
0.1058105
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0041600
</td>
<td style="text-align:right;">
0.0099785
</td>
<td style="text-align:right;">
0.0001325
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1119
</td>
</tr>
<tr>
<td style="text-align:left;">
41
</td>
<td style="text-align:left;">
Busan Anch.
</td>
<td style="text-align:left;">
South Korea
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
35.038333
</td>
<td style="text-align:right;">
129.0538889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
199
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
129115980
</td>
<td style="text-align:right;">
103860
</td>
<td style="text-align:right;">
690459.79
</td>
<td style="text-align:right;">
187
</td>
<td style="text-align:right;">
0.0259333
</td>
<td style="text-align:right;">
0.1213929
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:right;">
0.0337546
</td>
<td style="text-align:right;">
0.0753713
</td>
<td style="text-align:right;">
0.0033894
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1409
</td>
</tr>
<tr>
<td style="text-align:left;">
42
</td>
<td style="text-align:left;">
Puerto Natales
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-51.716667
</td>
<td style="text-align:right;">
-72.5166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
1497600
</td>
<td style="text-align:right;">
108300
</td>
<td style="text-align:right;">
106971.43
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.0011227
</td>
<td style="text-align:right;">
0.1156341
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0.1016801
</td>
<td style="text-align:right;">
0.0997736
</td>
<td style="text-align:right;">
0.0006475
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1444
</td>
</tr>
<tr>
<td style="text-align:left;">
43
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.683330
</td>
<td style="text-align:right;">
-57.8333300
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
354420
</td>
<td style="text-align:right;">
177210
</td>
<td style="text-align:right;">
177210.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0000060
</td>
<td style="text-align:right;">
0.1139033
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.0465468
</td>
<td style="text-align:right;">
0.0357669
</td>
<td style="text-align:right;">
0.0002488
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1456
</td>
</tr>
<tr>
<td style="text-align:left;">
44
</td>
<td style="text-align:left;">
Comodoro Rivadavia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-45.856944
</td>
<td style="text-align:right;">
-67.4822222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1383360
</td>
<td style="text-align:right;">
691680
</td>
<td style="text-align:right;">
691680.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0000009
</td>
<td style="text-align:right;">
0.1120086
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0535153
</td>
<td style="text-align:right;">
0.0639131
</td>
<td style="text-align:right;">
0.0002435
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1460
</td>
</tr>
<tr>
<td style="text-align:left;">
45
</td>
<td style="text-align:left;">
Puerto Madryn
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-42.738611
</td>
<td style="text-align:right;">
-65.0472222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
2803380
</td>
<td style="text-align:right;">
44040
</td>
<td style="text-align:right;">
116807.50
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0003642
</td>
<td style="text-align:right;">
0.1179956
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.2025929
</td>
<td style="text-align:right;">
0.1431527
</td>
<td style="text-align:right;">
0.0008338
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1461
</td>
</tr>
<tr>
<td style="text-align:left;">
46
</td>
<td style="text-align:left;">
Valparaiso
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-33.016667
</td>
<td style="text-align:right;">
-71.6333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
141
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
118742400
</td>
<td style="text-align:right;">
121140
</td>
<td style="text-align:right;">
997835.29
</td>
<td style="text-align:right;">
119
</td>
<td style="text-align:right;">
0.0116789
</td>
<td style="text-align:right;">
0.1180924
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
0.0805636
</td>
<td style="text-align:right;">
0.0902217
</td>
<td style="text-align:right;">
0.0022891
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1476
</td>
</tr>
<tr>
<td style="text-align:left;">
47
</td>
<td style="text-align:left;">
Callao Anch.
</td>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-12.017778
</td>
<td style="text-align:right;">
-77.1872222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
44193720
</td>
<td style="text-align:right;">
419160
</td>
<td style="text-align:right;">
1339203.64
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0.0015702
</td>
<td style="text-align:right;">
0.1183986
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0632082
</td>
<td style="text-align:right;">
0.0815135
</td>
<td style="text-align:right;">
0.0010637
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1478
</td>
</tr>
<tr>
<td style="text-align:left;">
48
</td>
<td style="text-align:left;">
Punta del Este
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.966667
</td>
<td style="text-align:right;">
-54.9500000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
394560
</td>
<td style="text-align:right;">
63840
</td>
<td style="text-align:right;">
98640.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0000031
</td>
<td style="text-align:right;">
0.1150442
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.0314936
</td>
<td style="text-align:right;">
0.0428980
</td>
<td style="text-align:right;">
0.0002193
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1501
</td>
</tr>
<tr>
<td style="text-align:left;">
49
</td>
<td style="text-align:left;">
Rio de Janeiro
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-22.916667
</td>
<td style="text-align:right;">
-43.2000000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
54419640
</td>
<td style="text-align:right;">
143430
</td>
<td style="text-align:right;">
777423.43
</td>
<td style="text-align:right;">
70
</td>
<td style="text-align:right;">
0.0181477
</td>
<td style="text-align:right;">
0.1217086
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
0.1453109
</td>
<td style="text-align:right;">
0.1416753
</td>
<td style="text-align:right;">
0.0022071
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1516
</td>
</tr>
<tr>
<td style="text-align:left;">
50
</td>
<td style="text-align:left;">
Bahia Blanca
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-38.776111
</td>
<td style="text-align:right;">
-62.2869444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
42699480
</td>
<td style="text-align:right;">
2081220
</td>
<td style="text-align:right;">
3881770.91
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0028381
</td>
<td style="text-align:right;">
0.1156912
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
0.1226273
</td>
<td style="text-align:right;">
0.0954990
</td>
<td style="text-align:right;">
0.0008106
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
1525
</td>
</tr>
<tr>
<td style="text-align:left;">
51
</td>
<td style="text-align:left;">
Apapa-Lagos
</td>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
6.438333
</td>
<td style="text-align:right;">
3.3886111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
12280380
</td>
<td style="text-align:right;">
455610
</td>
<td style="text-align:right;">
511682.50
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0042668
</td>
<td style="text-align:right;">
0.1168831
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.0588670
</td>
<td style="text-align:right;">
0.0365989
</td>
<td style="text-align:right;">
0.0011743
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
1550
</td>
</tr>
<tr>
<td style="text-align:left;">
52
</td>
<td style="text-align:left;">
Bonny
</td>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
4.421667
</td>
<td style="text-align:right;">
7.1502778
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
347880
</td>
<td style="text-align:right;">
80100
</td>
<td style="text-align:right;">
69576.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0011173
</td>
<td style="text-align:right;">
0.1080519
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.0051158
</td>
<td style="text-align:right;">
0.0078717
</td>
<td style="text-align:right;">
0.0007463
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
1554
</td>
</tr>
<tr>
<td style="text-align:left;">
53
</td>
<td style="text-align:left;">
Matadi
</td>
<td style="text-align:left;">
Congo
</td>
<td style="text-align:left;">
Gulf of Guinea South
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
-5.816667
</td>
<td style="text-align:right;">
13.4500000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
10624080
</td>
<td style="text-align:right;">
578700
</td>
<td style="text-align:right;">
664005.00
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.0008298
</td>
<td style="text-align:right;">
0.1177655
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.0227315
</td>
<td style="text-align:right;">
0.0451822
</td>
<td style="text-align:right;">
0.0006504
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
1575
</td>
</tr>
<tr>
<td style="text-align:left;">
54
</td>
<td style="text-align:left;">
Isla de Providencia
</td>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Central America inc Mexico to Panama
</td>
<td style="text-align:right;">
13.383333
</td>
<td style="text-align:right;">
-81.3666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
201480
</td>
<td style="text-align:right;">
38640
</td>
<td style="text-align:right;">
33580.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0002454
</td>
<td style="text-align:right;">
0.1098849
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.0014076
</td>
<td style="text-align:right;">
0.0073070
</td>
<td style="text-align:right;">
0.0004179
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1690
</td>
</tr>
<tr>
<td style="text-align:left;">
55
</td>
<td style="text-align:left;">
San Andres
</td>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Central America inc Mexico to Panama
</td>
<td style="text-align:right;">
12.550000
</td>
<td style="text-align:right;">
-81.6833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
156420
</td>
<td style="text-align:right;">
41400
</td>
<td style="text-align:right;">
39105.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0007270
</td>
<td style="text-align:right;">
0.1147973
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0033883
</td>
<td style="text-align:right;">
0.0199736
</td>
<td style="text-align:right;">
0.0004138
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1695
</td>
</tr>
<tr>
<td style="text-align:left;">
56
</td>
<td style="text-align:left;">
Dakar
</td>
<td style="text-align:left;">
Senegal
</td>
<td style="text-align:left;">
Sahelian Upwelling
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
14.688056
</td>
<td style="text-align:right;">
-17.4347222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
1476900
</td>
<td style="text-align:right;">
49260
</td>
<td style="text-align:right;">
92306.25
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.0037294
</td>
<td style="text-align:right;">
0.1186312
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.0375337
</td>
<td style="text-align:right;">
0.0406981
</td>
<td style="text-align:right;">
0.0007127
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
1729
</td>
</tr>
<tr>
<td style="text-align:left;">
57
</td>
<td style="text-align:left;">
Kakinada
</td>
<td style="text-align:left;">
India
</td>
<td style="text-align:left;">
Eastern India
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
India, Pakistan and Burma
</td>
<td style="text-align:right;">
17.000000
</td>
<td style="text-align:right;">
82.2833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1044000
</td>
<td style="text-align:right;">
1044000
</td>
<td style="text-align:right;">
1044000.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0006868
</td>
<td style="text-align:right;">
0.1070938
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0102628
</td>
<td style="text-align:right;">
0.0102817
</td>
<td style="text-align:right;">
0.0003122
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
1763
</td>
</tr>
<tr>
<td style="text-align:left;">
58
</td>
<td style="text-align:left;">
Port Louis
</td>
<td style="text-align:left;">
Mauritius
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-20.150000
</td>
<td style="text-align:right;">
57.4833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
16627140
</td>
<td style="text-align:right;">
41460
</td>
<td style="text-align:right;">
369492.00
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.0107212
</td>
<td style="text-align:right;">
0.1202930
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
0.0991341
</td>
<td style="text-align:right;">
0.0268845
</td>
<td style="text-align:right;">
0.0014368
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
1858
</td>
</tr>
</tbody>
</table>

</div>

In fact, 58 ports within the port-based network were the ‘last port of
call’ for ships travelling to the Southern Ocean. 33 of those ports had
more than one ship depart for the Southern Ocean during the 5-year
period. The official gateway cities are 1st (Ushuaia), 3rd (Punta
Arenas), 5th (Cape Town), 8th (Hobart) and 9th (Lyttleton/Christchurch)
on the list of ‘last ports of call.’ Each gateway city is the highest
rank city for its country and they are typically cultural and logistical
hubs that arrange Antarctic travel. For example, a tourist itinerary may
have passengers embark on their cruise in Ushuaia (Argentina), travel to
Puerto Williams (Chile), on to the Falkland Islands/Islas Malvinas where
they make a number of stops before heading to Antarctica. In this
scenario, the last port of call might be Stanley Harbour, but the point
of departure for passengers was Ushuaia. Nonetheless, from an impact and
biosecurity perspective, ensuring that measures are in place at all
major ports of departure is essential.

75 ports have direct links to locations in Antarctica, 17 acting only as
return ports and 15 acting only as departure ports. As shown in table
produced below.

``` r
port_networks$all %>% 
  activate(edges) %>% 
  convert(to_subgraph, to_realm == "Southern Ocean" | from_realm == "Southern Ocean", subset_by = "edges") %>% 
  activate(nodes) %>% 
  mutate(isolated = node_is_isolated()) %>% 
  convert(to_subgraph, isolated == FALSE, subset_by = "nodes") %>% 
  mutate(strength_out = centrality_degree(mode='out', weights = n_voyages, normalized = T)) %>%
  mutate(strength_in = centrality_degree(mode='in', weights = n_voyages, normalized = T)) %>% 
  as_tibble() %>% 
  filter(realm != "Southern Ocean") %>% 
  kable(format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = F,
                    fixed_thead = TRUE,
                    font_size = 11,
                    row_label_position = "l") %>%
  scroll_box(height = "300px")
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
place
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
country
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
area
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
latitude
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
longitude
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
feature
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
betweenness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
closeness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_total
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_eigen
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_hub
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_pagerank
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
cluster
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
.tidygraph\_node\_index
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
isolated
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
Hong Kong
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
22.286111
</td>
<td style="text-align:right;">
114.1575000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
9694860
</td>
<td style="text-align:right;">
62280
</td>
<td style="text-align:right;">
122719.75
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:right;">
0.0226553
</td>
<td style="text-align:right;">
0.1202467
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
0.0147290
</td>
<td style="text-align:right;">
0.0464588
</td>
<td style="text-align:right;">
0.0033722
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
Huangpu
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
23.097500
</td>
<td style="text-align:right;">
113.4241667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
38059800
</td>
<td style="text-align:right;">
1371060
</td>
<td style="text-align:right;">
1654773.91
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.0042219
</td>
<td style="text-align:right;">
0.1134752
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0297128
</td>
<td style="text-align:right;">
0.0135357
</td>
<td style="text-align:right;">
0.0009803
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Xiamen
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
24.457500
</td>
<td style="text-align:right;">
118.0713889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
7158960
</td>
<td style="text-align:right;">
58680
</td>
<td style="text-align:right;">
650814.55
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0031432
</td>
<td style="text-align:right;">
0.1136957
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
0.0010678
</td>
<td style="text-align:right;">
0.0102104
</td>
<td style="text-align:right;">
0.0009309
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:left;">
Vietnam, Thailand, Malaysia and Indonesia
</td>
<td style="text-align:right;">
1.268889
</td>
<td style="text-align:right;">
103.8330556
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:right;">
176
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
25993500
</td>
<td style="text-align:right;">
52110
</td>
<td style="text-align:right;">
158496.95
</td>
<td style="text-align:right;">
164
</td>
<td style="text-align:right;">
0.1586519
</td>
<td style="text-align:right;">
0.1247085
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
171
</td>
<td style="text-align:right;">
0.1083365
</td>
<td style="text-align:right;">
0.1183744
</td>
<td style="text-align:right;">
0.0074888
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
Bintulu
</td>
<td style="text-align:left;">
Malaysia
</td>
<td style="text-align:left;">
Vietnam, Thailand, Malaysia and Indonesia
</td>
<td style="text-align:right;">
3.166667
</td>
<td style="text-align:right;">
113.0333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Sunda Shelf/Java Sea
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
39660
</td>
<td style="text-align:right;">
39660
</td>
<td style="text-align:right;">
39660.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0000773
</td>
<td style="text-align:right;">
0.1102084
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0193285
</td>
<td style="text-align:right;">
0.0269509
</td>
<td style="text-align:right;">
0.0002661
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
Papeete
</td>
<td style="text-align:left;">
French Polynesia
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-17.533333
</td>
<td style="text-align:right;">
-149.5833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Society Islands
</td>
<td style="text-align:left;">
Southeast Polynesia
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:left;">
Southeast Polynesia
</td>
<td style="text-align:left;">
Southeast Polynesia
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
52010340
</td>
<td style="text-align:right;">
271140
</td>
<td style="text-align:right;">
753773.04
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
0.0279027
</td>
<td style="text-align:right;">
0.1192965
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
0.0429945
</td>
<td style="text-align:right;">
0.0671618
</td>
<td style="text-align:right;">
0.0024621
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
215
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
Sydney
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-33.857500
</td>
<td style="text-align:right;">
151.2063889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Manning-Hawkesbury
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
7848900
</td>
<td style="text-align:right;">
176220
</td>
<td style="text-align:right;">
413100.00
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0.0051545
</td>
<td style="text-align:right;">
0.1099689
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
0.0031308
</td>
<td style="text-align:right;">
0.0026404
</td>
<td style="text-align:right;">
0.0012889
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
511
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
Hobart
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-42.880000
</td>
<td style="text-align:right;">
147.3347222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
173044800
</td>
<td style="text-align:right;">
531840
</td>
<td style="text-align:right;">
3845440.00
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.0239954
</td>
<td style="text-align:right;">
0.1183013
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
0.0344683
</td>
<td style="text-align:right;">
0.0325634
</td>
<td style="text-align:right;">
0.0024109
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
523
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
Lyttelton
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-43.606389
</td>
<td style="text-align:right;">
172.7280556
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
70
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
36260340
</td>
<td style="text-align:right;">
227940
</td>
<td style="text-align:right;">
824098.64
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
0.0453610
</td>
<td style="text-align:right;">
0.1227300
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
0.0285876
</td>
<td style="text-align:right;">
0.0569459
</td>
<td style="text-align:right;">
0.0029303
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
528
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
Timaru
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-44.383333
</td>
<td style="text-align:right;">
171.2500000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
91295580
</td>
<td style="text-align:right;">
4660620
</td>
<td style="text-align:right;">
8299598.18
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0007215
</td>
<td style="text-align:right;">
0.1099947
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.0023053
</td>
<td style="text-align:right;">
0.0024840
</td>
<td style="text-align:right;">
0.0006401
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
533
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
Bluff
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-46.616667
</td>
<td style="text-align:right;">
168.3666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
20787240
</td>
<td style="text-align:right;">
126030
</td>
<td style="text-align:right;">
866135.00
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0047416
</td>
<td style="text-align:right;">
0.1117545
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
0.0032323
</td>
<td style="text-align:right;">
0.0022358
</td>
<td style="text-align:right;">
0.0017689
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
535
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
Dunedin
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-45.883333
</td>
<td style="text-align:right;">
170.5166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
2277420
</td>
<td style="text-align:right;">
92130
</td>
<td style="text-align:right;">
103519.09
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0013273
</td>
<td style="text-align:right;">
0.1120086
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.0025789
</td>
<td style="text-align:right;">
0.0030994
</td>
<td style="text-align:right;">
0.0008474
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
536
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
Fremantle
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-32.059167
</td>
<td style="text-align:right;">
115.7508333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
5321040
</td>
<td style="text-align:right;">
285060
</td>
<td style="text-align:right;">
295613.33
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.0127652
</td>
<td style="text-align:right;">
0.1190915
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.0071753
</td>
<td style="text-align:right;">
0.0345285
</td>
<td style="text-align:right;">
0.0014415
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
542
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
Fremantle Anch.
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-32.035833
</td>
<td style="text-align:right;">
115.6955556
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1385820
</td>
<td style="text-align:right;">
103050
</td>
<td style="text-align:right;">
173227.50
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0024659
</td>
<td style="text-align:right;">
0.1102084
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0244535
</td>
<td style="text-align:right;">
0.0005621
</td>
<td style="text-align:right;">
0.0008744
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
543
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
Las Palmas
</td>
<td style="text-align:left;">
Canary Islands
</td>
<td style="text-align:left;">
Spain / Portugal inc Atlantic Islands
</td>
<td style="text-align:right;">
28.133056
</td>
<td style="text-align:right;">
-15.4319444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:right;">
202
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
143319180
</td>
<td style="text-align:right;">
118200
</td>
<td style="text-align:right;">
770533.23
</td>
<td style="text-align:right;">
186
</td>
<td style="text-align:right;">
0.0657576
</td>
<td style="text-align:right;">
0.1258826
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
149
</td>
<td style="text-align:right;">
0.2178712
</td>
<td style="text-align:right;">
0.1638702
</td>
<td style="text-align:right;">
0.0050938
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
649
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
Gdansk
</td>
<td style="text-align:left;">
Poland
</td>
<td style="text-align:left;">
Scandinavia inc Baltic, Greenland, Iceland etc
</td>
<td style="text-align:right;">
54.361111
</td>
<td style="text-align:right;">
18.6566667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
45540480
</td>
<td style="text-align:right;">
39150
</td>
<td style="text-align:right;">
690007.27
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
0.0038011
</td>
<td style="text-align:right;">
0.1168758
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
0.0256666
</td>
<td style="text-align:right;">
0.0088746
</td>
<td style="text-align:right;">
0.0012671
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
910
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
Delfzijl
</td>
<td style="text-align:left;">
Netherlands
</td>
<td style="text-align:left;">
North European Atlantic coast
</td>
<td style="text-align:right;">
53.333333
</td>
<td style="text-align:right;">
6.9333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
5265780
</td>
<td style="text-align:right;">
684090
</td>
<td style="text-align:right;">
877630.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0034653
</td>
<td style="text-align:right;">
0.1134821
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0.0236109
</td>
<td style="text-align:right;">
0.0248191
</td>
<td style="text-align:right;">
0.0007415
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1028
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
Dover Strait
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.020920
</td>
<td style="text-align:right;">
1.3979300
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
300
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
53940
</td>
<td style="text-align:right;">
26970
</td>
<td style="text-align:right;">
26970.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.1063554
</td>
<td style="text-align:right;">
0.1280613
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
195
</td>
<td style="text-align:right;">
0.1287739
</td>
<td style="text-align:right;">
0.2108743
</td>
<td style="text-align:right;">
0.0064365
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1034
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
Hansweert
</td>
<td style="text-align:left;">
Netherlands
</td>
<td style="text-align:left;">
North European Atlantic coast
</td>
<td style="text-align:right;">
51.450000
</td>
<td style="text-align:right;">
4.0000000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11762820
</td>
<td style="text-align:right;">
1119600
</td>
<td style="text-align:right;">
1306980.00
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.0010170
</td>
<td style="text-align:right;">
0.1178470
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.0641853
</td>
<td style="text-align:right;">
0.0366657
</td>
<td style="text-align:right;">
0.0005647
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1058
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
Immingham
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
53.630556
</td>
<td style="text-align:right;">
-0.1902778
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
12020580
</td>
<td style="text-align:right;">
435540
</td>
<td style="text-align:right;">
480823.20
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
0.0028392
</td>
<td style="text-align:right;">
0.1165556
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.0160842
</td>
<td style="text-align:right;">
0.0146820
</td>
<td style="text-align:right;">
0.0011140
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1066
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
Rochester(GBR)
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.387500
</td>
<td style="text-align:right;">
0.5097222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
93600
</td>
<td style="text-align:right;">
93600
</td>
<td style="text-align:right;">
93600.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0001640
</td>
<td style="text-align:right;">
0.1104360
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0264197
</td>
<td style="text-align:right;">
0.0234085
</td>
<td style="text-align:right;">
0.0002168
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
1097
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
Sheerness
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.437778
</td>
<td style="text-align:right;">
0.7438889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
397500
</td>
<td style="text-align:right;">
198750
</td>
<td style="text-align:right;">
198750.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0016633
</td>
<td style="text-align:right;">
0.1178916
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0070646
</td>
<td style="text-align:right;">
0.0272848
</td>
<td style="text-align:right;">
0.0004125
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
1105
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
Thamesport
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.434167
</td>
<td style="text-align:right;">
0.6863889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0000919
</td>
<td style="text-align:right;">
0.1058105
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0041600
</td>
<td style="text-align:right;">
0.0099785
</td>
<td style="text-align:right;">
0.0001325
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1119
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
Seattle
</td>
<td style="text-align:left;">
U.S.A.
</td>
<td style="text-align:left;">
West coast North America inc USA, Canada & Alaska
</td>
<td style="text-align:right;">
47.633333
</td>
<td style="text-align:right;">
-122.3333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Puget Trough/Georgia Basin
</td>
<td style="text-align:left;">
Cold Temperate Northeast Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Cold Temperate Northeast Pacific
</td>
<td style="text-align:left;">
Cold Temperate Northeast Pacific
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
50576460
</td>
<td style="text-align:right;">
49440
</td>
<td style="text-align:right;">
1123921.33
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.0111626
</td>
<td style="text-align:right;">
0.1078838
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
0.0023915
</td>
<td style="text-align:right;">
0.0005734
</td>
<td style="text-align:right;">
0.0021924
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
1273
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
Nakhodka
</td>
<td style="text-align:left;">
Russia
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
42.798611
</td>
<td style="text-align:right;">
132.8769444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
5085240
</td>
<td style="text-align:right;">
266400
</td>
<td style="text-align:right;">
462294.55
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0015546
</td>
<td style="text-align:right;">
0.1160570
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0068342
</td>
<td style="text-align:right;">
0.0387870
</td>
<td style="text-align:right;">
0.0008189
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1302
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
Shidao
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
36.879722
</td>
<td style="text-align:right;">
122.4288889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Yellow Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
15024300
</td>
<td style="text-align:right;">
356430
</td>
<td style="text-align:right;">
441891.18
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
0.0014049
</td>
<td style="text-align:right;">
0.1109531
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0756169
</td>
<td style="text-align:right;">
0.0068480
</td>
<td style="text-align:right;">
0.0010576
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1349
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
Busan
</td>
<td style="text-align:left;">
South Korea
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
35.117222
</td>
<td style="text-align:right;">
129.0444444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
276
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
201356640
</td>
<td style="text-align:right;">
308700
</td>
<td style="text-align:right;">
864191.59
</td>
<td style="text-align:right;">
233
</td>
<td style="text-align:right;">
0.0433371
</td>
<td style="text-align:right;">
0.1217640
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
95
</td>
<td style="text-align:right;">
0.0563119
</td>
<td style="text-align:right;">
0.0710715
</td>
<td style="text-align:right;">
0.0034927
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1408
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
Busan Anch.
</td>
<td style="text-align:left;">
South Korea
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
35.038333
</td>
<td style="text-align:right;">
129.0538889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
199
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
129115980
</td>
<td style="text-align:right;">
103860
</td>
<td style="text-align:right;">
690459.79
</td>
<td style="text-align:right;">
187
</td>
<td style="text-align:right;">
0.0259333
</td>
<td style="text-align:right;">
0.1213929
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:right;">
0.0337546
</td>
<td style="text-align:right;">
0.0753713
</td>
<td style="text-align:right;">
0.0033894
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1409
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
Zhoushan
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
30.000000
</td>
<td style="text-align:right;">
122.1000000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
132916500
</td>
<td style="text-align:right;">
1630440
</td>
<td style="text-align:right;">
2712581.63
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
0.0083196
</td>
<td style="text-align:right;">
0.1167446
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.0323581
</td>
<td style="text-align:right;">
0.0401773
</td>
<td style="text-align:right;">
0.0020230
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1438
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
Cabo Negro
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-52.950000
</td>
<td style="text-align:right;">
-70.7833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
299220
</td>
<td style="text-align:right;">
69510
</td>
<td style="text-align:right;">
74805.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0008553
</td>
<td style="text-align:right;">
0.1133446
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.0535043
</td>
<td style="text-align:right;">
0.0756199
</td>
<td style="text-align:right;">
0.0003732
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1441
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
31
</td>
<td style="text-align:left;">
Puerto Natales
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-51.716667
</td>
<td style="text-align:right;">
-72.5166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
1497600
</td>
<td style="text-align:right;">
108300
</td>
<td style="text-align:right;">
106971.43
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.0011227
</td>
<td style="text-align:right;">
0.1156341
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0.1016801
</td>
<td style="text-align:right;">
0.0997736
</td>
<td style="text-align:right;">
0.0006475
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1444
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
32
</td>
<td style="text-align:left;">
Puerto Williams
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-54.933333
</td>
<td style="text-align:right;">
-67.6166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
226
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
20728620
</td>
<td style="text-align:right;">
50100
</td>
<td style="text-align:right;">
265751.54
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
0.0045090
</td>
<td style="text-align:right;">
0.1197850
</td>
<td style="text-align:right;">
124
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:right;">
0.4322165
</td>
<td style="text-align:right;">
0.6671451
</td>
<td style="text-align:right;">
0.0017021
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1445
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
33
</td>
<td style="text-align:left;">
Punta Arenas
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-53.166667
</td>
<td style="text-align:right;">
-70.9000000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
483
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:right;">
1040288820
</td>
<td style="text-align:right;">
345330
</td>
<td style="text-align:right;">
2626991.97
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.0923236
</td>
<td style="text-align:right;">
0.1255870
</td>
<td style="text-align:right;">
163
</td>
<td style="text-align:right;">
180
</td>
<td style="text-align:right;">
200
</td>
<td style="text-align:right;">
0.6967879
</td>
<td style="text-align:right;">
0.6109293
</td>
<td style="text-align:right;">
0.0066130
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1446
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
34
</td>
<td style="text-align:left;">
Ushuaia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-54.801944
</td>
<td style="text-align:right;">
-68.3027778
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
1132
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
92028900
</td>
<td style="text-align:right;">
61860
</td>
<td style="text-align:right;">
135137.89
</td>
<td style="text-align:right;">
681
</td>
<td style="text-align:right;">
0.0633296
</td>
<td style="text-align:right;">
0.1254608
</td>
<td style="text-align:right;">
718
</td>
<td style="text-align:right;">
874
</td>
<td style="text-align:right;">
243
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.0065769
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1447
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
35
</td>
<td style="text-align:left;">
Berkeley Sound
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.568500
</td>
<td style="text-align:right;">
-57.9350000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
217
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
56171160
</td>
<td style="text-align:right;">
180000
</td>
<td style="text-align:right;">
371994.44
</td>
<td style="text-align:right;">
151
</td>
<td style="text-align:right;">
0.0172043
</td>
<td style="text-align:right;">
0.1238013
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
0.2079869
</td>
<td style="text-align:right;">
0.1962151
</td>
<td style="text-align:right;">
0.0021764
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1455
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
36
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.683330
</td>
<td style="text-align:right;">
-57.8333300
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
354420
</td>
<td style="text-align:right;">
177210
</td>
<td style="text-align:right;">
177210.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0000060
</td>
<td style="text-align:right;">
0.1139033
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.0465468
</td>
<td style="text-align:right;">
0.0357669
</td>
<td style="text-align:right;">
0.0002488
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1456
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
37
</td>
<td style="text-align:left;">
Mare Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.916667
</td>
<td style="text-align:right;">
-58.4666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2947080
</td>
<td style="text-align:right;">
213900
</td>
<td style="text-align:right;">
245590.00
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.0032631
</td>
<td style="text-align:right;">
0.1180924
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.1491757
</td>
<td style="text-align:right;">
0.1808846
</td>
<td style="text-align:right;">
0.0006459
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1457
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
38
</td>
<td style="text-align:left;">
Port William
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.666667
</td>
<td style="text-align:right;">
-57.7833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
262
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
33725400
</td>
<td style="text-align:right;">
7200
</td>
<td style="text-align:right;">
188410.06
</td>
<td style="text-align:right;">
179
</td>
<td style="text-align:right;">
0.0159196
</td>
<td style="text-align:right;">
0.1249750
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
0.3805145
</td>
<td style="text-align:right;">
0.3473966
</td>
<td style="text-align:right;">
0.0022437
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1458
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
39
</td>
<td style="text-align:left;">
Stanley Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.683333
</td>
<td style="text-align:right;">
-57.8333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
461
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
93807060
</td>
<td style="text-align:right;">
37860
</td>
<td style="text-align:right;">
252849.22
</td>
<td style="text-align:right;">
371
</td>
<td style="text-align:right;">
0.0445123
</td>
<td style="text-align:right;">
0.1249416
</td>
<td style="text-align:right;">
212
</td>
<td style="text-align:right;">
103
</td>
<td style="text-align:right;">
175
</td>
<td style="text-align:right;">
0.7232937
</td>
<td style="text-align:right;">
0.8478881
</td>
<td style="text-align:right;">
0.0040189
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1459
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
40
</td>
<td style="text-align:left;">
Comodoro Rivadavia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-45.856944
</td>
<td style="text-align:right;">
-67.4822222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1383360
</td>
<td style="text-align:right;">
691680
</td>
<td style="text-align:right;">
691680.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0000009
</td>
<td style="text-align:right;">
0.1120086
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0535153
</td>
<td style="text-align:right;">
0.0639131
</td>
<td style="text-align:right;">
0.0002435
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1460
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
41
</td>
<td style="text-align:left;">
Puerto Madryn
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-42.738611
</td>
<td style="text-align:right;">
-65.0472222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
2803380
</td>
<td style="text-align:right;">
44040
</td>
<td style="text-align:right;">
116807.50
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0003642
</td>
<td style="text-align:right;">
0.1179956
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.2025929
</td>
<td style="text-align:right;">
0.1431527
</td>
<td style="text-align:right;">
0.0008338
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1461
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
42
</td>
<td style="text-align:left;">
San Antonio(CHL)
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-33.600000
</td>
<td style="text-align:right;">
-71.6166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3844800
</td>
<td style="text-align:right;">
50040
</td>
<td style="text-align:right;">
213600.00
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.0021536
</td>
<td style="text-align:right;">
0.1150372
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0628266
</td>
<td style="text-align:right;">
0.0869422
</td>
<td style="text-align:right;">
0.0007813
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1467
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
43
</td>
<td style="text-align:left;">
Talcahuano
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-36.693056
</td>
<td style="text-align:right;">
-73.0986111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
58672860
</td>
<td style="text-align:right;">
1037010
</td>
<td style="text-align:right;">
2444702.50
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0026803
</td>
<td style="text-align:right;">
0.1169488
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.0617176
</td>
<td style="text-align:right;">
0.1345736
</td>
<td style="text-align:right;">
0.0009437
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1468
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
44
</td>
<td style="text-align:left;">
Valparaiso
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-33.016667
</td>
<td style="text-align:right;">
-71.6333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
141
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
118742400
</td>
<td style="text-align:right;">
121140
</td>
<td style="text-align:right;">
997835.29
</td>
<td style="text-align:right;">
119
</td>
<td style="text-align:right;">
0.0116789
</td>
<td style="text-align:right;">
0.1180924
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
0.0805636
</td>
<td style="text-align:right;">
0.0902217
</td>
<td style="text-align:right;">
0.0022891
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1476
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
45
</td>
<td style="text-align:left;">
Callao
</td>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-12.048056
</td>
<td style="text-align:right;">
-77.1425000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
34151640
</td>
<td style="text-align:right;">
107400
</td>
<td style="text-align:right;">
443527.79
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
0.0049358
</td>
<td style="text-align:right;">
0.1196931
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
0.0232497
</td>
<td style="text-align:right;">
0.0716267
</td>
<td style="text-align:right;">
0.0011213
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1477
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
46
</td>
<td style="text-align:left;">
Callao Anch.
</td>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-12.017778
</td>
<td style="text-align:right;">
-77.1872222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
44193720
</td>
<td style="text-align:right;">
419160
</td>
<td style="text-align:right;">
1339203.64
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0.0015702
</td>
<td style="text-align:right;">
0.1183986
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0632082
</td>
<td style="text-align:right;">
0.0815135
</td>
<td style="text-align:right;">
0.0010637
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1478
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
47
</td>
<td style="text-align:left;">
Buenos Aires
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.590278
</td>
<td style="text-align:right;">
-58.3775000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
147
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
97445220
</td>
<td style="text-align:right;">
141030
</td>
<td style="text-align:right;">
798731.31
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
0.0053304
</td>
<td style="text-align:right;">
0.1216058
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
0.2511624
</td>
<td style="text-align:right;">
0.2781807
</td>
<td style="text-align:right;">
0.0015365
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1492
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
48
</td>
<td style="text-align:left;">
La Plata Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.745000
</td>
<td style="text-align:right;">
-57.8311111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
2295840
</td>
<td style="text-align:right;">
44400
</td>
<td style="text-align:right;">
85031.11
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.0037545
</td>
<td style="text-align:right;">
0.1172492
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.0878967
</td>
<td style="text-align:right;">
0.0920901
</td>
<td style="text-align:right;">
0.0009020
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1497
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
49
</td>
<td style="text-align:left;">
Montevideo
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.900000
</td>
<td style="text-align:right;">
-56.2666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
481
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:right;">
98
</td>
<td style="text-align:right;">
286114560
</td>
<td style="text-align:right;">
84270
</td>
<td style="text-align:right;">
650260.36
</td>
<td style="text-align:right;">
440
</td>
<td style="text-align:right;">
0.0421971
</td>
<td style="text-align:right;">
0.1260946
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
0.4361499
</td>
<td style="text-align:right;">
0.4522382
</td>
<td style="text-align:right;">
0.0030350
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1498
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
50
</td>
<td style="text-align:left;">
Montevideo Alpha Zone
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-35.044610
</td>
<td style="text-align:right;">
-56.0587700
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
325
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
181217940
</td>
<td style="text-align:right;">
119190
</td>
<td style="text-align:right;">
604059.80
</td>
<td style="text-align:right;">
300
</td>
<td style="text-align:right;">
0.0569748
</td>
<td style="text-align:right;">
0.1265292
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:right;">
0.2875728
</td>
<td style="text-align:right;">
0.2414593
</td>
<td style="text-align:right;">
0.0037786
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1499
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
51
</td>
<td style="text-align:left;">
Punta del Este
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.966667
</td>
<td style="text-align:right;">
-54.9500000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
394560
</td>
<td style="text-align:right;">
63840
</td>
<td style="text-align:right;">
98640.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0000031
</td>
<td style="text-align:right;">
0.1150442
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.0314936
</td>
<td style="text-align:right;">
0.0428980
</td>
<td style="text-align:right;">
0.0002193
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1501
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
52
</td>
<td style="text-align:left;">
Rio Grande(BRA)
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-32.166667
</td>
<td style="text-align:right;">
-52.0833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
4447680
</td>
<td style="text-align:right;">
97680
</td>
<td style="text-align:right;">
103434.42
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.0024300
</td>
<td style="text-align:right;">
0.1187968
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0.0791623
</td>
<td style="text-align:right;">
0.1386731
</td>
<td style="text-align:right;">
0.0014001
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1505
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
53
</td>
<td style="text-align:left;">
Rio Grande(BRA) Anch.
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-32.264444
</td>
<td style="text-align:right;">
-51.8447222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
3220140
</td>
<td style="text-align:right;">
169860
</td>
<td style="text-align:right;">
230010.00
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.0000155
</td>
<td style="text-align:right;">
0.1062188
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.0856861
</td>
<td style="text-align:right;">
0.0024873
</td>
<td style="text-align:right;">
0.0008458
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1506
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
54
</td>
<td style="text-align:left;">
Niteroi
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-22.883333
</td>
<td style="text-align:right;">
-43.1166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
25092900
</td>
<td style="text-align:right;">
82800
</td>
<td style="text-align:right;">
1194900.00
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0.0018120
</td>
<td style="text-align:right;">
0.1185786
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.0859600
</td>
<td style="text-align:right;">
0.0243109
</td>
<td style="text-align:right;">
0.0008676
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1513
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
55
</td>
<td style="text-align:left;">
Rio de Janeiro
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-22.916667
</td>
<td style="text-align:right;">
-43.2000000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
54419640
</td>
<td style="text-align:right;">
143430
</td>
<td style="text-align:right;">
777423.43
</td>
<td style="text-align:right;">
70
</td>
<td style="text-align:right;">
0.0181477
</td>
<td style="text-align:right;">
0.1217086
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
0.1453109
</td>
<td style="text-align:right;">
0.1416753
</td>
<td style="text-align:right;">
0.0022071
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1516
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
56
</td>
<td style="text-align:left;">
Santos
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-23.933333
</td>
<td style="text-align:right;">
-46.3333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
1793340
</td>
<td style="text-align:right;">
47940
</td>
<td style="text-align:right;">
99630.00
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.0048856
</td>
<td style="text-align:right;">
0.1149312
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0762244
</td>
<td style="text-align:right;">
0.0387876
</td>
<td style="text-align:right;">
0.0009975
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
1518
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
57
</td>
<td style="text-align:left;">
Bahia Blanca
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-38.776111
</td>
<td style="text-align:right;">
-62.2869444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
42699480
</td>
<td style="text-align:right;">
2081220
</td>
<td style="text-align:right;">
3881770.91
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0028381
</td>
<td style="text-align:right;">
0.1156912
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
0.1226273
</td>
<td style="text-align:right;">
0.0954990
</td>
<td style="text-align:right;">
0.0008106
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
1525
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
58
</td>
<td style="text-align:left;">
Bahia Blanca Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-39.343889
</td>
<td style="text-align:right;">
-61.5036111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
302700
</td>
<td style="text-align:right;">
151350
</td>
<td style="text-align:right;">
151350.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0007294
</td>
<td style="text-align:right;">
0.1149312
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.1094162
</td>
<td style="text-align:right;">
0.0229492
</td>
<td style="text-align:right;">
0.0003355
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
1526
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
59
</td>
<td style="text-align:left;">
Mar del Plata
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-38.033056
</td>
<td style="text-align:right;">
-57.5447222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1447200
</td>
<td style="text-align:right;">
94020
</td>
<td style="text-align:right;">
131563.64
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0019870
</td>
<td style="text-align:right;">
0.1157342
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
0.2204177
</td>
<td style="text-align:right;">
0.0835426
</td>
<td style="text-align:right;">
0.0010527
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1527
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
60
</td>
<td style="text-align:left;">
Recalada Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-39.000000
</td>
<td style="text-align:right;">
-61.2666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
144
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
28050240
</td>
<td style="text-align:right;">
97380
</td>
<td style="text-align:right;">
252704.86
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:right;">
0.0016961
</td>
<td style="text-align:right;">
0.1167301
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0.1956967
</td>
<td style="text-align:right;">
0.1008033
</td>
<td style="text-align:right;">
0.0014139
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1529
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
61
</td>
<td style="text-align:left;">
Durban
</td>
<td style="text-align:left;">
South Africa
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-29.866667
</td>
<td style="text-align:right;">
31.0333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Natal
</td>
<td style="text-align:left;">
Agulhas
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Agulhas
</td>
<td style="text-align:left;">
Agulhas
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
9365160
</td>
<td style="text-align:right;">
58320
</td>
<td style="text-align:right;">
170275.64
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
0.0124810
</td>
<td style="text-align:right;">
0.1186914
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
0.0617869
</td>
<td style="text-align:right;">
0.0325299
</td>
<td style="text-align:right;">
0.0018225
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
1534
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
62
</td>
<td style="text-align:left;">
Cape Town
</td>
<td style="text-align:left;">
South Africa
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-33.916667
</td>
<td style="text-align:right;">
18.4333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:right;">
179
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
56
</td>
<td style="text-align:right;">
339895500
</td>
<td style="text-align:right;">
423930
</td>
<td style="text-align:right;">
2393630.28
</td>
<td style="text-align:right;">
142
</td>
<td style="text-align:right;">
0.0652400
</td>
<td style="text-align:right;">
0.1270876
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
119
</td>
<td style="text-align:right;">
0.2415621
</td>
<td style="text-align:right;">
0.2231377
</td>
<td style="text-align:right;">
0.0038608
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
1539
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
63
</td>
<td style="text-align:left;">
Cape Town Anch.
</td>
<td style="text-align:left;">
South Africa
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-33.854444
</td>
<td style="text-align:right;">
18.4380556
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
23566620
</td>
<td style="text-align:right;">
54360
</td>
<td style="text-align:right;">
380106.77
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
0.0057208
</td>
<td style="text-align:right;">
0.1148325
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
0.0900158
</td>
<td style="text-align:right;">
0.0180104
</td>
<td style="text-align:right;">
0.0018719
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
1540
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
64
</td>
<td style="text-align:left;">
Apapa-Lagos
</td>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
6.438333
</td>
<td style="text-align:right;">
3.3886111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
12280380
</td>
<td style="text-align:right;">
455610
</td>
<td style="text-align:right;">
511682.50
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0042668
</td>
<td style="text-align:right;">
0.1168831
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.0588670
</td>
<td style="text-align:right;">
0.0365989
</td>
<td style="text-align:right;">
0.0011743
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
1550
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
65
</td>
<td style="text-align:left;">
Bonny
</td>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
4.421667
</td>
<td style="text-align:right;">
7.1502778
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
347880
</td>
<td style="text-align:right;">
80100
</td>
<td style="text-align:right;">
69576.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0011173
</td>
<td style="text-align:right;">
0.1080519
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.0051158
</td>
<td style="text-align:right;">
0.0078717
</td>
<td style="text-align:right;">
0.0007463
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
1554
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
66
</td>
<td style="text-align:left;">
Matadi
</td>
<td style="text-align:left;">
Congo
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
-5.816667
</td>
<td style="text-align:right;">
13.4500000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Gulf of Guinea South
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
10624080
</td>
<td style="text-align:right;">
578700
</td>
<td style="text-align:right;">
664005.00
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.0008298
</td>
<td style="text-align:right;">
0.1177655
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.0227315
</td>
<td style="text-align:right;">
0.0451822
</td>
<td style="text-align:right;">
0.0006504
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
1575
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
67
</td>
<td style="text-align:left;">
St. Helena
</td>
<td style="text-align:left;">
St. Helena
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
-15.927410
</td>
<td style="text-align:right;">
-5.7170960
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1605600
</td>
<td style="text-align:right;">
190800
</td>
<td style="text-align:right;">
229371.43
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.0006632
</td>
<td style="text-align:right;">
0.1175510
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.0779246
</td>
<td style="text-align:right;">
0.0163551
</td>
<td style="text-align:right;">
0.0004018
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
1603
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
68
</td>
<td style="text-align:left;">
Isla de Providencia
</td>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:left;">
Central America inc Mexico to Panama
</td>
<td style="text-align:right;">
13.383333
</td>
<td style="text-align:right;">
-81.3666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
201480
</td>
<td style="text-align:right;">
38640
</td>
<td style="text-align:right;">
33580.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0002454
</td>
<td style="text-align:right;">
0.1098849
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.0014076
</td>
<td style="text-align:right;">
0.0073070
</td>
<td style="text-align:right;">
0.0004179
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1690
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
69
</td>
<td style="text-align:left;">
San Andres
</td>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:left;">
Central America inc Mexico to Panama
</td>
<td style="text-align:right;">
12.550000
</td>
<td style="text-align:right;">
-81.6833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
156420
</td>
<td style="text-align:right;">
41400
</td>
<td style="text-align:right;">
39105.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0007270
</td>
<td style="text-align:right;">
0.1147973
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0033883
</td>
<td style="text-align:right;">
0.0199736
</td>
<td style="text-align:right;">
0.0004138
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1695
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
70
</td>
<td style="text-align:left;">
Salvador Anch.
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-12.956389
</td>
<td style="text-align:right;">
-38.5444444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Eastern Brazil
</td>
<td style="text-align:left;">
Tropical Southwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Tropical Southwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Southwestern Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
160680
</td>
<td style="text-align:right;">
80340
</td>
<td style="text-align:right;">
80340.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0000224
</td>
<td style="text-align:right;">
0.1125135
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0131811
</td>
<td style="text-align:right;">
0.0083201
</td>
<td style="text-align:right;">
0.0001799
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
1711
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
71
</td>
<td style="text-align:left;">
Praia(CPV)
</td>
<td style="text-align:left;">
Cape Verde
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
14.900000
</td>
<td style="text-align:right;">
-23.5166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Cape Verde
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
1426140
</td>
<td style="text-align:right;">
40320
</td>
<td style="text-align:right;">
64824.55
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0048626
</td>
<td style="text-align:right;">
0.1191067
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.1067331
</td>
<td style="text-align:right;">
0.0738775
</td>
<td style="text-align:right;">
0.0011964
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
1724
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
72
</td>
<td style="text-align:left;">
Dakar
</td>
<td style="text-align:left;">
Senegal
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
14.688056
</td>
<td style="text-align:right;">
-17.4347222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Sahelian Upwelling
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
1476900
</td>
<td style="text-align:right;">
49260
</td>
<td style="text-align:right;">
92306.25
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.0037294
</td>
<td style="text-align:right;">
0.1186312
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.0375337
</td>
<td style="text-align:right;">
0.0406981
</td>
<td style="text-align:right;">
0.0007127
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
1729
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
73
</td>
<td style="text-align:left;">
La Libertad(ECU)
</td>
<td style="text-align:left;">
Ecuador
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-2.216667
</td>
<td style="text-align:right;">
-80.9166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Guayaquil
</td>
<td style="text-align:left;">
Tropical East Pacific
</td>
<td style="text-align:left;">
Tropical Eastern Pacific
</td>
<td style="text-align:left;">
Tropical East Pacific
</td>
<td style="text-align:left;">
Tropical East Pacific
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
3084600
</td>
<td style="text-align:right;">
63450
</td>
<td style="text-align:right;">
385575.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0019233
</td>
<td style="text-align:right;">
0.1162372
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0.0155724
</td>
<td style="text-align:right;">
0.0161900
</td>
<td style="text-align:right;">
0.0005717
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1739
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
74
</td>
<td style="text-align:left;">
Kakinada
</td>
<td style="text-align:left;">
India
</td>
<td style="text-align:left;">
India, Pakistan and Burma
</td>
<td style="text-align:right;">
17.000000
</td>
<td style="text-align:right;">
82.2833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Eastern India
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1044000
</td>
<td style="text-align:right;">
1044000
</td>
<td style="text-align:right;">
1044000.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0006868
</td>
<td style="text-align:right;">
0.1070938
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0102628
</td>
<td style="text-align:right;">
0.0102817
</td>
<td style="text-align:right;">
0.0003122
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
1763
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
75
</td>
<td style="text-align:left;">
Port Louis
</td>
<td style="text-align:left;">
Mauritius
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-20.150000
</td>
<td style="text-align:right;">
57.4833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
16627140
</td>
<td style="text-align:right;">
41460
</td>
<td style="text-align:right;">
369492.00
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.0107212
</td>
<td style="text-align:right;">
0.1202930
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
0.0991341
</td>
<td style="text-align:right;">
0.0268845
</td>
<td style="text-align:right;">
0.0014368
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
1858
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
</tbody>
</table>

</div>

Let’s take a closer look at the 58 Gateway ports (summary information in
the paragraph above).

``` r
#I filter the port network to only include ports that were the origin for had
#at least one voyage to the Southern Ocean
gateway_test <- port_networks$all %>%
  activate(edges) %>%
  filter(to_realm == "Southern Ocean") %>%
  activate(nodes) %>%
  mutate(strength_out = centrality_degree(mode = "out", weights = n_voyages)) %>%
  filter(strength_out > 0)
#I create a subgraph, converted to a tibble showing those ports and their attributes
gateway_nodes <- gateway_test %>%
  activate(nodes) %>%
  filter(realm != "Southern Ocean") %>%
  arrange(desc(strength_out)) %>%
  dplyr::select(place, country, ecoregion, province, realm, strength_out, everything()) %>%
  ungroup() %>%
  as_tibble() %>%
  mutate(total_voyages_to = sum(strength_out)) %>%
  mutate(pcent = strength_out / total_voyages_to * 100)

gateway_nodes %>%
  dplyr::select(place, country, strength_out, total_voyages_to, pcent, ecoregion, province, realm) %>% 
  kable(format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = F,
                    fixed_thead = TRUE,
                    font_size = 11,
                    row_label_position = "l") %>%
  scroll_box(height = "500px")
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:500px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
place
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
country
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_voyages\_to
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
pcent
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
Ushuaia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
718
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
46.8362688
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
Stanley Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:right;">
212
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
13.8290933
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Punta Arenas
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
163
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
10.6327462
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Puerto Williams
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
124
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
8.0887149
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
Cape Town
</td>
<td style="text-align:left;">
South Africa
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
2.6744945
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
Port William
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
2.6092629
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
Montevideo
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
2.0874103
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
Hobart
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
1.6960209
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
Lyttelton
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
1.1089367
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
Berkeley Sound
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
1.0437052
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
Bluff
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.9784736
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
Mare Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.9784736
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
Montevideo Alpha Zone
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.9784736
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
Buenos Aires
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.7175473
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
Cabo Negro
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.4566210
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
Bintulu
</td>
<td style="text-align:left;">
Malaysia
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.3261579
</td>
<td style="text-align:left;">
Sunda Shelf/Java Sea
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
Las Palmas
</td>
<td style="text-align:left;">
Canary Islands
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.3261579
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
Busan
</td>
<td style="text-align:left;">
South Korea
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.3261579
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
Mar del Plata
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.3261579
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
Talcahuano
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.2609263
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
Callao
</td>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.2609263
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1956947
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
Sydney
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1956947
</td>
<td style="text-align:left;">
Manning-Hawkesbury
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
Fremantle
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1956947
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
La Plata Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1956947
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
Timaru
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1304631
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
Dunedin
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1304631
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
Dover Strait
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1304631
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
Nakhodka
</td>
<td style="text-align:left;">
Russia
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1304631
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
Zhoushan
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1304631
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
31
</td>
<td style="text-align:left;">
San Antonio(CHL)
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1304631
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
32
</td>
<td style="text-align:left;">
Rio Grande(BRA)
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1304631
</td>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
33
</td>
<td style="text-align:left;">
Recalada Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.1304631
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
34
</td>
<td style="text-align:left;">
Hong Kong
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
35
</td>
<td style="text-align:left;">
Huangpu
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
36
</td>
<td style="text-align:left;">
Xiamen
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
37
</td>
<td style="text-align:left;">
Delfzijl
</td>
<td style="text-align:left;">
Netherlands
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
38
</td>
<td style="text-align:left;">
Rochester(GBR)
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
39
</td>
<td style="text-align:left;">
Sheerness
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
40
</td>
<td style="text-align:left;">
Thamesport
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
41
</td>
<td style="text-align:left;">
Busan Anch.
</td>
<td style="text-align:left;">
South Korea
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
42
</td>
<td style="text-align:left;">
Puerto Natales
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
43
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
44
</td>
<td style="text-align:left;">
Comodoro Rivadavia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
45
</td>
<td style="text-align:left;">
Puerto Madryn
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
46
</td>
<td style="text-align:left;">
Valparaiso
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
47
</td>
<td style="text-align:left;">
Callao Anch.
</td>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
48
</td>
<td style="text-align:left;">
Punta del Este
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
49
</td>
<td style="text-align:left;">
Rio de Janeiro
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
50
</td>
<td style="text-align:left;">
Bahia Blanca
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
</tr>
<tr>
<td style="text-align:left;">
51
</td>
<td style="text-align:left;">
Apapa-Lagos
</td>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
52
</td>
<td style="text-align:left;">
Bonny
</td>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
53
</td>
<td style="text-align:left;">
Matadi
</td>
<td style="text-align:left;">
Congo
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Gulf of Guinea South
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
54
</td>
<td style="text-align:left;">
Isla de Providencia
</td>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
55
</td>
<td style="text-align:left;">
San Andres
</td>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
56
</td>
<td style="text-align:left;">
Dakar
</td>
<td style="text-align:left;">
Senegal
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Sahelian Upwelling
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
</tr>
<tr>
<td style="text-align:left;">
57
</td>
<td style="text-align:left;">
Kakinada
</td>
<td style="text-align:left;">
India
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Eastern India
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
</tr>
<tr>
<td style="text-align:left;">
58
</td>
<td style="text-align:left;">
Port Louis
</td>
<td style="text-align:left;">
Mauritius
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1533
</td>
<td style="text-align:right;">
0.0652316
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
</tr>
</tbody>
</table>

</div>

### What about the recognised Gateway cities?

Here I just look at the 5 ‘official’ cities.

``` r
#percentage of activity through all official gateways
gateway_nodes %>%
  dplyr::select(place, country, strength_out, total_voyages_to, pcent, ecoregion, province, realm) %>%
  filter(place == "Ushuaia" |
           place == "Punta Arenas" |
           place == "Cape Town" |
           place == "Hobart" |
           place == "Lyttelton") %>%
  summarise(gateway_cities_pcent = sum(pcent))
```

    ## # A tibble: 1 x 1
    ##   gateway_cities_pcent
    ##                  <dbl>
    ## 1                 62.9

``` r
#percentage of activity through official each gateway
gateway_nodes %>%
  dplyr::select(place, country, strength_out, total_voyages_to, pcent, ecoregion, province, realm) %>%
  filter(place == "Ushuaia" |
           place == "Punta Arenas" |
           place == "Cape Town" |
           place == "Hobart" |
           place == "Lyttelton")
```

    ## # A tibble: 5 x 8
    ##   place  country  strength_out total_voyages_to pcent ecoregion  province realm 
    ##   <chr>  <chr>           <dbl>            <dbl> <dbl> <chr>      <chr>    <chr> 
    ## 1 Ushua… Argenti…          718             1533 46.8  Channels … Magella… Tempe…
    ## 2 Punta… Chile             163             1533 10.6  Channels … Magella… Tempe…
    ## 3 Cape … South A…           41             1533  2.67 Namaqua    Benguela Tempe…
    ## 4 Hobart Austral…           26             1533  1.70 Bassian    Southea… Tempe…
    ## 5 Lytte… New Zea…           17             1533  1.11 Central N… Souther… Tempe…

``` r
#percentage of activity through Punta Arenas and Ushuaia
gateway_nodes %>%
  dplyr::select(place, country, strength_out, total_voyages_to, pcent, ecoregion, province, realm) %>%
  filter(place == "Ushuaia" | place == "Punta Arenas") %>%
  summarise(gateway_cities_pcent = sum(pcent))
```

    ## # A tibble: 1 x 1
    ##   gateway_cities_pcent
    ##                  <dbl>
    ## 1                 57.5

#### Summary information about gateway ports by country.

Relevant to the consideration of where biosecurity measures could be
implemented to effectively reduce transfer of non-native species.

``` r
#number of gateway ports per country
gateway_nodes %>%
  group_by(country) %>%
  count() %>% 
  arrange(desc(n))
```

    ## # A tibble: 23 x 2
    ## # Groups:   country [23]
    ##    country              n
    ##    <chr>            <int>
    ##  1 Argentina            8
    ##  2 Chile                7
    ##  3 Falkland Islands     5
    ##  4 China                4
    ##  5 New Zealand          4
    ##  6 U.K.                 4
    ##  7 Australia            3
    ##  8 Uruguay              3
    ##  9 Brazil               2
    ## 10 Colombia             2
    ## # … with 13 more rows

``` r
#Number of gateway ports per country with strength out greater than 1
gateway_nodes %>%
  filter(strength_out > 1) %>%
  group_by(country) %>%
  count() %>% 
  arrange(desc(n))
```

    ## # A tibble: 16 x 2
    ## # Groups:   country [16]
    ##    country              n
    ##    <chr>            <int>
    ##  1 Argentina            5
    ##  2 Chile                5
    ##  3 Falkland Islands     4
    ##  4 New Zealand          4
    ##  5 Australia            3
    ##  6 Uruguay              2
    ##  7 Brazil               1
    ##  8 Canary Islands       1
    ##  9 China                1
    ## 10 Malaysia             1
    ## 11 Peru                 1
    ## 12 Russia               1
    ## 13 Singapore            1
    ## 14 South Africa         1
    ## 15 South Korea          1
    ## 16 U.K.                 1

``` r
#Top 10 together
gateway_nodes %>%
  arrange(desc(pcent)) %>%
  dplyr::select(place, country, strength_out, total_voyages_to, pcent, ecoregion, province, realm) %>%
  slice(1:10) %>%
  summarise(gateway_cities_pcent = sum(pcent))
```

    ## # A tibble: 1 x 1
    ##   gateway_cities_pcent
    ##                  <dbl>
    ## 1                 90.6

``` r
#Top 10 ports
gateway_nodes %>%
  arrange(desc(pcent)) %>%
  dplyr::select(place, country, strength_out, total_voyages_to, pcent, ecoregion, province, realm) %>%
  slice(1:10)
```

    ## # A tibble: 10 x 8
    ##    place  country  strength_out total_voyages_to pcent ecoregion  province realm
    ##    <chr>  <chr>           <dbl>            <dbl> <dbl> <chr>      <chr>    <chr>
    ##  1 Ushua… Argenti…          718             1533 46.8  Channels … Magella… Temp…
    ##  2 Stanl… Falklan…          212             1533 13.8  Malvinas/… Magella… Temp…
    ##  3 Punta… Chile             163             1533 10.6  Channels … Magella… Temp…
    ##  4 Puert… Chile             124             1533  8.09 Channels … Magella… Temp…
    ##  5 Cape … South A…           41             1533  2.67 Namaqua    Benguela Temp…
    ##  6 Port … Falklan…           40             1533  2.61 Malvinas/… Magella… Temp…
    ##  7 Monte… Uruguay            32             1533  2.09 Rio de la… Warm Te… Temp…
    ##  8 Hobart Austral…           26             1533  1.70 Bassian    Southea… Temp…
    ##  9 Lytte… New Zea…           17             1533  1.11 Central N… Souther… Temp…
    ## 10 Berke… Falklan…           16             1533  1.04 Malvinas/… Magella… Temp…

#### For what proportion of voyages do they act as ‘last ports of call’?

``` r
gateway_nodes %>%
  arrange(desc(pcent)) %>%
  dplyr::select(place, country, strength_out, total_voyages_to, pcent, ecoregion, province, realm) %>%
  filter(country == "Argentina" |
           country == "Chile" |
           country == "Falkland Islands" |
           country == "South Africa" |
           country == "Australia" |
           country == "New Zealand" |
           country == "Uruguay") %>%
  summarise(gateway_cities_pcent = sum(pcent))
```

    ## # A tibble: 1 x 1
    ##   gateway_cities_pcent
    ##                  <dbl>
    ## 1                 96.9

``` r
gateway_nodes %>%
  dplyr::select(place, country, strength_out, pcent) %>%
  mutate(pcent = sprintf("%0.2f", pcent))
```

    ## # A tibble: 58 x 4
    ##    place           country          strength_out pcent
    ##    <chr>           <chr>                   <dbl> <chr>
    ##  1 Ushuaia         Argentina                 718 46.84
    ##  2 Stanley Harbour Falkland Islands          212 13.83
    ##  3 Punta Arenas    Chile                     163 10.63
    ##  4 Puerto Williams Chile                     124 8.09 
    ##  5 Cape Town       South Africa               41 2.67 
    ##  6 Port William    Falkland Islands           40 2.61 
    ##  7 Montevideo      Uruguay                    32 2.09 
    ##  8 Hobart          Australia                  26 1.70 
    ##  9 Lyttelton       New Zealand                17 1.11 
    ## 10 Berkeley Sound  Falkland Islands           16 1.04 
    ## # … with 48 more rows

## Gateway Ports for different activities

### Fishing

The two most important departure ports for fishing vessels that visit
select coastal Antarctic locations are Punta Arenas in Chile and Cape
Town in South Africa, both recognised as Gateway Cities. However, no
other Gateway City was a departure point for fishing vessesl that
visited the Antarctic nodes. Many fishing vessesl, and much fishing
activity, occurrs offshore so the importance of ports that launch
vessels to the Southern Ocean that did not visit the Antarctic ‘ports’
could be quite different.

``` r
gateway_ports_results$fishing
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
place
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
country
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
area
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
latitude
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
longitude
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
feature
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
betweenness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
closeness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_total
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_eigen
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_hub
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_pagerank
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
cluster
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
.tidygraph\_node\_index
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
Punta Arenas
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-53.166667
</td>
<td style="text-align:right;">
-70.90000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
722117340
</td>
<td style="text-align:right;">
4958130
</td>
<td style="text-align:right;">
12894952.5
</td>
<td style="text-align:right;">
56
</td>
<td style="text-align:right;">
0.2826824
</td>
<td style="text-align:right;">
0.1390096
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
0.6269361
</td>
<td style="text-align:right;">
0.6342124
</td>
<td style="text-align:right;">
0.0528741
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
124
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
Cape Town
</td>
<td style="text-align:left;">
South Africa
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-33.916667
</td>
<td style="text-align:right;">
18.43333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
101208660
</td>
<td style="text-align:right;">
7577040
</td>
<td style="text-align:right;">
7229190.0
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.0400364
</td>
<td style="text-align:right;">
0.1285141
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.1941207
</td>
<td style="text-align:right;">
0.2998742
</td>
<td style="text-align:right;">
0.0180839
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
145
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Montevideo
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.900000
</td>
<td style="text-align:right;">
-56.26667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
106071120
</td>
<td style="text-align:right;">
1106490
</td>
<td style="text-align:right;">
3119738.8
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
0.1447931
</td>
<td style="text-align:right;">
0.1364024
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
0.3369112
</td>
<td style="text-align:right;">
0.5264605
</td>
<td style="text-align:right;">
0.0179155
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
140
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Las Palmas
</td>
<td style="text-align:left;">
Canary Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Spain / Portugal inc Atlantic Islands
</td>
<td style="text-align:right;">
28.133056
</td>
<td style="text-align:right;">
-15.43194
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
31886400
</td>
<td style="text-align:right;">
3490260
</td>
<td style="text-align:right;">
3188640.0
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0.0364850
</td>
<td style="text-align:right;">
0.1236476
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.2105891
</td>
<td style="text-align:right;">
0.1210776
</td>
<td style="text-align:right;">
0.0100419
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
87
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
Port William
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.666667
</td>
<td style="text-align:right;">
-57.78333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
11578620
</td>
<td style="text-align:right;">
7200
</td>
<td style="text-align:right;">
503418.3
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.0426637
</td>
<td style="text-align:right;">
0.1323408
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.4349254
</td>
<td style="text-align:right;">
0.4007132
</td>
<td style="text-align:right;">
0.0164768
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
130
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
Stanley Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.683333
</td>
<td style="text-align:right;">
-57.83333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
22370940
</td>
<td style="text-align:right;">
250560
</td>
<td style="text-align:right;">
1491396.0
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.0155890
</td>
<td style="text-align:right;">
0.1298701
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.4115656
</td>
<td style="text-align:right;">
0.3040485
</td>
<td style="text-align:right;">
0.0165443
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
131
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
Callao
</td>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-12.048056
</td>
<td style="text-align:right;">
-77.14250
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
15153900
</td>
<td style="text-align:right;">
331980
</td>
<td style="text-align:right;">
797573.7
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0.0026119
</td>
<td style="text-align:right;">
0.1291364
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.0744415
</td>
<td style="text-align:right;">
0.1999322
</td>
<td style="text-align:right;">
0.0049951
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
137
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
Montevideo Alpha Zone
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-35.044610
</td>
<td style="text-align:right;">
-56.05877
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
22483980
</td>
<td style="text-align:right;">
174060
</td>
<td style="text-align:right;">
642399.4
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
0.0501846
</td>
<td style="text-align:right;">
0.1246106
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.3754771
</td>
<td style="text-align:right;">
0.1275074
</td>
<td style="text-align:right;">
0.0210257
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
141
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
Puerto Williams
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-54.933333
</td>
<td style="text-align:right;">
-67.61667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
61920
</td>
<td style="text-align:right;">
61920
</td>
<td style="text-align:right;">
61920.0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0007056
</td>
<td style="text-align:right;">
0.1246106
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.1493826
</td>
<td style="text-align:right;">
0.1327960
</td>
<td style="text-align:right;">
0.0053559
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
123
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
Berkeley Sound
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.568500
</td>
<td style="text-align:right;">
-57.93500
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
3093600
</td>
<td style="text-align:right;">
215880
</td>
<td style="text-align:right;">
206240.0
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.0228011
</td>
<td style="text-align:right;">
0.1296596
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.3184068
</td>
<td style="text-align:right;">
0.3109590
</td>
<td style="text-align:right;">
0.0114823
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
128
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
Talcahuano
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-36.693056
</td>
<td style="text-align:right;">
-73.09861
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
22471080
</td>
<td style="text-align:right;">
3974280
</td>
<td style="text-align:right;">
4494216.0
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0213624
</td>
<td style="text-align:right;">
0.1285141
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.1606554
</td>
<td style="text-align:right;">
0.2332094
</td>
<td style="text-align:right;">
0.0096382
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
134
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
Timaru
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-44.383333
</td>
<td style="text-align:right;">
171.25000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
91295580
</td>
<td style="text-align:right;">
4660620
</td>
<td style="text-align:right;">
8299598.2
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0604429
</td>
<td style="text-align:right;">
0.1133948
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.0116987
</td>
<td style="text-align:right;">
0.0127981
</td>
<td style="text-align:right;">
0.0147080
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
83
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
Cabo Negro
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-52.950000
</td>
<td style="text-align:right;">
-70.78333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
48960
</td>
<td style="text-align:right;">
48960
</td>
<td style="text-align:right;">
48960.0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0025732
</td>
<td style="text-align:right;">
0.1235521
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.1243398
</td>
<td style="text-align:right;">
0.0948173
</td>
<td style="text-align:right;">
0.0063883
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Vietnam, Thailand, Malaysia and Indonesia
</td>
<td style="text-align:right;">
1.268889
</td>
<td style="text-align:right;">
103.83306
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
68280
</td>
<td style="text-align:right;">
68280
</td>
<td style="text-align:right;">
68280.0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0103263
</td>
<td style="text-align:right;">
0.1247077
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0260644
</td>
<td style="text-align:right;">
0.1283872
</td>
<td style="text-align:right;">
0.0053464
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
Busan
</td>
<td style="text-align:left;">
South Korea
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
35.117222
</td>
<td style="text-align:right;">
129.04444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
96671640
</td>
<td style="text-align:right;">
763140
</td>
<td style="text-align:right;">
3118440.0
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
0.2550591
</td>
<td style="text-align:right;">
0.1323408
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
0.2062726
</td>
<td style="text-align:right;">
0.1687841
</td>
<td style="text-align:right;">
0.0238696
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
115
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
Busan Anch.
</td>
<td style="text-align:left;">
South Korea
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
35.038333
</td>
<td style="text-align:right;">
129.05389
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
57861180
</td>
<td style="text-align:right;">
61500
</td>
<td style="text-align:right;">
2225430.0
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
0.0807204
</td>
<td style="text-align:right;">
0.1297648
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.0653662
</td>
<td style="text-align:right;">
0.1394829
</td>
<td style="text-align:right;">
0.0141316
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
116
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
Callao Anch.
</td>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-12.017778
</td>
<td style="text-align:right;">
-77.18722
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
37954860
</td>
<td style="text-align:right;">
689100
</td>
<td style="text-align:right;">
1650211.3
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.0175856
</td>
<td style="text-align:right;">
0.1278977
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0.2613968
</td>
<td style="text-align:right;">
0.1710035
</td>
<td style="text-align:right;">
0.0097068
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
138
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
Punta del Este
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.966667
</td>
<td style="text-align:right;">
-54.95000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
72240
</td>
<td style="text-align:right;">
72240
</td>
<td style="text-align:right;">
72240.0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0001594
</td>
<td style="text-align:right;">
0.1114206
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0360181
</td>
<td style="text-align:right;">
0.0249240
</td>
<td style="text-align:right;">
0.0017330
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
142
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
Recalada Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-39.000000
</td>
<td style="text-align:right;">
-61.26667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
74580
</td>
<td style="text-align:right;">
74580
</td>
<td style="text-align:right;">
74580.0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0015779
</td>
<td style="text-align:right;">
0.1250977
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.2328555
</td>
<td style="text-align:right;">
0.1406662
</td>
<td style="text-align:right;">
0.0069490
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
144
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
Isla de Providencia
</td>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Central America inc Mexico to Panama
</td>
<td style="text-align:right;">
13.383333
</td>
<td style="text-align:right;">
-81.36667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0003491
</td>
<td style="text-align:right;">
0.1139601
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0053229
</td>
<td style="text-align:right;">
0.0212270
</td>
<td style="text-align:right;">
0.0019872
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
153
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
San Andres
</td>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Central America inc Mexico to Panama
</td>
<td style="text-align:right;">
12.550000
</td>
<td style="text-align:right;">
-81.68333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.1243201
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0144606
</td>
<td style="text-align:right;">
0.1030789
</td>
<td style="text-align:right;">
0.0021133
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
154
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
Dakar
</td>
<td style="text-align:left;">
Senegal
</td>
<td style="text-align:left;">
Sahelian Upwelling
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
14.688056
</td>
<td style="text-align:right;">
-17.43472
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
245580
</td>
<td style="text-align:right;">
245580
</td>
<td style="text-align:right;">
245580.0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0069984
</td>
<td style="text-align:right;">
0.1170446
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0690098
</td>
<td style="text-align:right;">
0.0482089
</td>
<td style="text-align:right;">
0.0034642
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
155
</td>
</tr>
</tbody>
</table>

</div>

``` r
gateway_fishing <- port_networks$fishing %>%
  activate(edges) %>%
  filter(to_realm == "Southern Ocean") %>%
  activate(nodes) %>%
  mutate(strength_out = centrality_degree(mode = "out", weights = n_voyages)) %>%
  filter(strength_out > 0)
gateway_nodes_fishing <- gateway_fishing %>%
  activate(nodes) %>%
  filter(realm != "Southern Ocean") %>%
  arrange(desc(strength_out)) %>%
  dplyr::select(place, country, ecoregion, province, realm, strength_out, everything()) %>%
  ungroup() %>%
  as_tibble() %>%
  mutate(total_voyages_to = sum(strength_out)) %>%
  mutate(pcent = strength_out / total_voyages_to * 100)

gateway_nodes_fishing %>%
  dplyr::select(place, country, strength_out, pcent) %>%
  mutate(pcent = sprintf("%0.2f", pcent))
```

    ## # A tibble: 22 x 4
    ##    place                 country          strength_out pcent
    ##    <chr>                 <chr>                   <dbl> <chr>
    ##  1 Punta Arenas          Chile                      21 25.30
    ##  2 Cape Town             South Africa               13 15.66
    ##  3 Montevideo            Uruguay                     7 8.43 
    ##  4 Las Palmas            Canary Islands              4 4.82 
    ##  5 Port William          Falkland Islands            4 4.82 
    ##  6 Stanley Harbour       Falkland Islands            4 4.82 
    ##  7 Callao                Peru                        4 4.82 
    ##  8 Montevideo Alpha Zone Uruguay                     4 4.82 
    ##  9 Puerto Williams       Chile                       3 3.61 
    ## 10 Berkeley Sound        Falkland Islands            3 3.61 
    ## # … with 12 more rows

### Tourism

Tourism tends to visit islands and continents which are included as
Antarctic nodes. Ushuaia, Argentina is a recognised Gateway City and had
691 voyages direct to Antarctic locations, 3.5 times more than the next
most important port. Stanley Harbour in the Falkland Islands/Islas
Malvinas and Puerto Williams in Chile were the next most important
ports, with almost an order of mangitude more departures than the the
next ports; Bluff in New Zealand, Punta Arenas in Chile, Montevideo in
Uruguay and Port William in the Falkland Islands/Islas Malvinas. Tourist
intineraries, in particlar, involve frequent stops at places of
interest. So while the official Gateway Cities are relatively poorly
represented in this list (with the exception of Ushuaia), tourists are
still likely to embark or travel through Gatway Cities to reach
Antarctica. For example, a cruise might stop briefly at Puerto Williams
for sightseeing but the passengers may have joined the cruise in Punta
Arenas.

``` r
gateway_ports_results$tourism
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
place
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
country
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
area
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
latitude
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
longitude
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
feature
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
betweenness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
closeness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_total
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_eigen
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_hub
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_pagerank
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
cluster
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
.tidygraph\_node\_index
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
Ushuaia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
691
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-54.80194
</td>
<td style="text-align:right;">
-68.302778
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
1085
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
69901680
</td>
<td style="text-align:right;">
56040
</td>
<td style="text-align:right;">
106883.30
</td>
<td style="text-align:right;">
654
</td>
<td style="text-align:right;">
0.2209495
</td>
<td style="text-align:right;">
0.2275630
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:right;">
221
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.0096004
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1007
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
Stanley Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
197
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.68333
</td>
<td style="text-align:right;">
-57.833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
323
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
30551220
</td>
<td style="text-align:right;">
32820
</td>
<td style="text-align:right;">
117958.38
</td>
<td style="text-align:right;">
259
</td>
<td style="text-align:right;">
0.0494739
</td>
<td style="text-align:right;">
0.2200309
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
135
</td>
<td style="text-align:right;">
0.6551418
</td>
<td style="text-align:right;">
0.8216825
</td>
<td style="text-align:right;">
0.0041284
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1018
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Puerto Williams
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-54.93333
</td>
<td style="text-align:right;">
-67.616667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
160
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
5567520
</td>
<td style="text-align:right;">
28920
</td>
<td style="text-align:right;">
118457.87
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0.0065933
</td>
<td style="text-align:right;">
0.2105004
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
0.4281797
</td>
<td style="text-align:right;">
0.6793609
</td>
<td style="text-align:right;">
0.0016757
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1005
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Bluff
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-46.61667
</td>
<td style="text-align:right;">
168.366667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
20750940
</td>
<td style="text-align:right;">
146280
</td>
<td style="text-align:right;">
943224.55
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0159975
</td>
<td style="text-align:right;">
0.1653137
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.0002139
</td>
<td style="text-align:right;">
0.0001151
</td>
<td style="text-align:right;">
0.0031447
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
335
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
Punta Arenas
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-53.16667
</td>
<td style="text-align:right;">
-70.900000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
164
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
13754040
</td>
<td style="text-align:right;">
45840
</td>
<td style="text-align:right;">
110032.32
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
0.0438585
</td>
<td style="text-align:right;">
0.2107078
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:right;">
0.4097125
</td>
<td style="text-align:right;">
0.3221660
</td>
<td style="text-align:right;">
0.0034471
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1006
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
Montevideo
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.90000
</td>
<td style="text-align:right;">
-56.266667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
237
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
18665160
</td>
<td style="text-align:right;">
40620
</td>
<td style="text-align:right;">
85620.00
</td>
<td style="text-align:right;">
218
</td>
<td style="text-align:right;">
0.0468497
</td>
<td style="text-align:right;">
0.2191664
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:right;">
0.2822081
</td>
<td style="text-align:right;">
0.3009242
</td>
<td style="text-align:right;">
0.0031067
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
1043
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
Port William
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.66667
</td>
<td style="text-align:right;">
-57.783333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
1026120
</td>
<td style="text-align:right;">
7200
</td>
<td style="text-align:right;">
25027.32
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
0.0072139
</td>
<td style="text-align:right;">
0.2109850
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
0.2560826
</td>
<td style="text-align:right;">
0.2209660
</td>
<td style="text-align:right;">
0.0013488
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1017
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
Mar del Plata
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-38.03306
</td>
<td style="text-align:right;">
-57.544722
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1163460
</td>
<td style="text-align:right;">
92550
</td>
<td style="text-align:right;">
116346.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0.0050580
</td>
<td style="text-align:right;">
0.2006255
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.1787261
</td>
<td style="text-align:right;">
0.0537030
</td>
<td style="text-align:right;">
0.0011094
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1061
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
Hobart
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-42.88000
</td>
<td style="text-align:right;">
147.334722
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5245380
</td>
<td style="text-align:right;">
95100
</td>
<td style="text-align:right;">
476852.73
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0100563
</td>
<td style="text-align:right;">
0.1590036
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.0000576
</td>
<td style="text-align:right;">
0.0001087
</td>
<td style="text-align:right;">
0.0014345
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
325
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
Dunedin
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-45.88333
</td>
<td style="text-align:right;">
170.516667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2250240
</td>
<td style="text-align:right;">
95040
</td>
<td style="text-align:right;">
107154.29
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0.0054986
</td>
<td style="text-align:right;">
0.1785665
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.0001144
</td>
<td style="text-align:right;">
0.0002650
</td>
<td style="text-align:right;">
0.0014809
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
336
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
Buenos Aires
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.59028
</td>
<td style="text-align:right;">
-58.377500
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
102
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
11151360
</td>
<td style="text-align:right;">
127020
</td>
<td style="text-align:right;">
122542.42
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
0.0032472
</td>
<td style="text-align:right;">
0.2036508
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.1018624
</td>
<td style="text-align:right;">
0.1738242
</td>
<td style="text-align:right;">
0.0010422
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
1040
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
Delfzijl
</td>
<td style="text-align:left;">
Netherlands
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
North European Atlantic coast
</td>
<td style="text-align:right;">
53.33333
</td>
<td style="text-align:right;">
6.933333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
151380
</td>
<td style="text-align:right;">
151380
</td>
<td style="text-align:right;">
151380.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0033493
</td>
<td style="text-align:right;">
0.1895406
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.0171595
</td>
<td style="text-align:right;">
0.0198849
</td>
<td style="text-align:right;">
0.0006480
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
747
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
Puerto Natales
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-51.71667
</td>
<td style="text-align:right;">
-72.516667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
1236480
</td>
<td style="text-align:right;">
99240
</td>
<td style="text-align:right;">
95113.85
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.0031134
</td>
<td style="text-align:right;">
0.1934268
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0.1023001
</td>
<td style="text-align:right;">
0.0990973
</td>
<td style="text-align:right;">
0.0011498
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1004
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
Berkeley Sound
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.56850
</td>
<td style="text-align:right;">
-57.935000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
21600
</td>
<td style="text-align:right;">
7200
</td>
<td style="text-align:right;">
7200.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.0008630
</td>
<td style="text-align:right;">
0.2004374
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0687109
</td>
<td style="text-align:right;">
0.0756054
</td>
<td style="text-align:right;">
0.0003388
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1015
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
Comodoro Rivadavia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-45.85694
</td>
<td style="text-align:right;">
-67.482222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.1654203
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0103166
</td>
<td style="text-align:right;">
0.0279624
</td>
<td style="text-align:right;">
0.0001798
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1019
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
Puerto Madryn
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-42.73861
</td>
<td style="text-align:right;">
-65.047222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
2490780
</td>
<td style="text-align:right;">
42720
</td>
<td style="text-align:right;">
108294.78
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.0021021
</td>
<td style="text-align:right;">
0.2006255
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.1695074
</td>
<td style="text-align:right;">
0.1123615
</td>
<td style="text-align:right;">
0.0009795
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1020
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
San Antonio(CHL)
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-33.60000
</td>
<td style="text-align:right;">
-71.616667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2971020
</td>
<td style="text-align:right;">
47460
</td>
<td style="text-align:right;">
198068.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.0055657
</td>
<td style="text-align:right;">
0.1927005
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
0.0403163
</td>
<td style="text-align:right;">
0.0706162
</td>
<td style="text-align:right;">
0.0008837
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1023
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
Rio de Janeiro
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-22.91667
</td>
<td style="text-align:right;">
-43.200000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
10575480
</td>
<td style="text-align:right;">
127500
</td>
<td style="text-align:right;">
240351.82
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
0.0124843
</td>
<td style="text-align:right;">
0.1975366
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.0875050
</td>
<td style="text-align:right;">
0.0921614
</td>
<td style="text-align:right;">
0.0021482
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
1055
</td>
</tr>
</tbody>
</table>

</div>

``` r
gateway_tourism <- port_networks$tourism %>%
  activate(edges) %>%
  filter(to_realm == "Southern Ocean") %>%
  activate(nodes) %>%
  mutate(strength_out = centrality_degree(mode = "out", weights = n_voyages)) %>%
  filter(strength_out > 0)
gateway_nodes_tourism <- gateway_tourism %>%
  activate(nodes) %>%
  filter(realm != "Southern Ocean") %>%
  arrange(desc(strength_out)) %>%
  dplyr::select(place, country, ecoregion, province, realm, strength_out, everything()) %>%
  ungroup() %>%
  as_tibble() %>%
  mutate(total_voyages_to = sum(strength_out)) %>%
  mutate(pcent = strength_out / total_voyages_to * 100)

gateway_nodes_tourism %>%
  dplyr::select(place, country, strength_out, pcent) %>%
  mutate(pcent = sprintf("%0.2f", pcent))
```

    ## # A tibble: 18 x 4
    ##    place              country          strength_out pcent
    ##    <chr>              <chr>                   <dbl> <chr>
    ##  1 Ushuaia            Argentina                 691 65.00
    ##  2 Stanley Harbour    Falkland Islands          197 18.53
    ##  3 Puerto Williams    Chile                     106 9.97 
    ##  4 Bluff              New Zealand                15 1.41 
    ##  5 Punta Arenas       Chile                      14 1.32 
    ##  6 Montevideo         Uruguay                    13 1.22 
    ##  7 Port William       Falkland Islands            9 0.85 
    ##  8 Mar del Plata      Argentina                   5 0.47 
    ##  9 Hobart             Australia                   2 0.19 
    ## 10 Dunedin            New Zealand                 2 0.19 
    ## 11 Buenos Aires       Argentina                   2 0.19 
    ## 12 Delfzijl           Netherlands                 1 0.09 
    ## 13 Puerto Natales     Chile                       1 0.09 
    ## 14 Berkeley Sound     Falkland Islands            1 0.09 
    ## 15 Comodoro Rivadavia Argentina                   1 0.09 
    ## 16 Puerto Madryn      Argentina                   1 0.09 
    ## 17 San Antonio(CHL)   Chile                       1 0.09 
    ## 18 Rio de Janeiro     Brazil                      1 0.09

### Research

Official Gateway Cities quite accurately reflect the most important
departure ports for research vessels and national operations. This is
perhaps unsurprising, since it is the nations active in Antarctic
reserach that have put forward their cities as suggested Gateways.
Gateway Cities make up 5 of the top 6 ports for research activity,
joined by Mare Harbour in the Falkland Islands/Islas Malvinas.

``` r
gateway_ports_results$research
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
place
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
country
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
area
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
latitude
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
longitude
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
feature
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
betweenness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
closeness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_total
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_eigen
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_hub
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_pagerank
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
cluster
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
.tidygraph\_node\_index
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
Punta Arenas
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-53.16667
</td>
<td style="text-align:right;">
-70.9000000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
184
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
147883680
</td>
<td style="text-align:right;">
411660
</td>
<td style="text-align:right;">
954088.3
</td>
<td style="text-align:right;">
155
</td>
<td style="text-align:right;">
0.4280655
</td>
<td style="text-align:right;">
0.1044634
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
113
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.0308209
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
361
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
Hobart
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-42.88000
</td>
<td style="text-align:right;">
147.3347222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
167799420
</td>
<td style="text-align:right;">
693150
</td>
<td style="text-align:right;">
4935277.1
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
0.1524685
</td>
<td style="text-align:right;">
0.0980392
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
0.1459639
</td>
<td style="text-align:right;">
0.1684131
</td>
<td style="text-align:right;">
0.0126288
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
235
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Cape Town
</td>
<td style="text-align:left;">
South Africa
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-33.91667
</td>
<td style="text-align:right;">
18.4333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
179675520
</td>
<td style="text-align:right;">
526680
</td>
<td style="text-align:right;">
3327324.4
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
0.2396748
</td>
<td style="text-align:right;">
0.1021593
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
0.2978264
</td>
<td style="text-align:right;">
0.3787997
</td>
<td style="text-align:right;">
0.0175210
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
404
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Ushuaia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-54.80194
</td>
<td style="text-align:right;">
-68.3027778
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
5720340
</td>
<td style="text-align:right;">
196890
</td>
<td style="text-align:right;">
408595.7
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.0272186
</td>
<td style="text-align:right;">
0.0985222
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
0.3843061
</td>
<td style="text-align:right;">
0.5532232
</td>
<td style="text-align:right;">
0.0076592
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
362
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
Mare Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.91667
</td>
<td style="text-align:right;">
-58.4666667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2532480
</td>
<td style="text-align:right;">
173040
</td>
<td style="text-align:right;">
230225.5
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0362364
</td>
<td style="text-align:right;">
0.0994575
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.2370361
</td>
<td style="text-align:right;">
0.4964796
</td>
<td style="text-align:right;">
0.0053282
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
367
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
Lyttelton
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-43.60639
</td>
<td style="text-align:right;">
172.7280556
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
13817160
</td>
<td style="text-align:right;">
504510
</td>
<td style="text-align:right;">
1381716.0
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0.1059206
</td>
<td style="text-align:right;">
0.0988320
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
0.1170236
</td>
<td style="text-align:right;">
0.2409546
</td>
<td style="text-align:right;">
0.0111478
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
237
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
Puerto Williams
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-54.93333
</td>
<td style="text-align:right;">
-67.6166667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1764480
</td>
<td style="text-align:right;">
110760
</td>
<td style="text-align:right;">
220560.0
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0038924
</td>
<td style="text-align:right;">
0.0969377
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.1925112
</td>
<td style="text-align:right;">
0.3648550
</td>
<td style="text-align:right;">
0.0024122
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
360
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
Stanley Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.68333
</td>
<td style="text-align:right;">
-57.8333333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
25283280
</td>
<td style="text-align:right;">
371730
</td>
<td style="text-align:right;">
902974.3
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.0325410
</td>
<td style="text-align:right;">
0.0975177
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.2022560
</td>
<td style="text-align:right;">
0.2408295
</td>
<td style="text-align:right;">
0.0077180
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
369
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
Sydney
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Manning-Hawkesbury
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-33.85750
</td>
<td style="text-align:right;">
151.2063889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1906320
</td>
<td style="text-align:right;">
319110
</td>
<td style="text-align:right;">
317720.0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0100316
</td>
<td style="text-align:right;">
0.0907591
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.0136155
</td>
<td style="text-align:right;">
0.0257493
</td>
<td style="text-align:right;">
0.0023492
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
233
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
Fremantle
</td>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-32.05917
</td>
<td style="text-align:right;">
115.7508333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4606320
</td>
<td style="text-align:right;">
352740
</td>
<td style="text-align:right;">
329022.9
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.0724214
</td>
<td style="text-align:right;">
0.0955068
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
0.0218564
</td>
<td style="text-align:right;">
0.1126278
</td>
<td style="text-align:right;">
0.0075032
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
238
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
Cabo Negro
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-52.95000
</td>
<td style="text-align:right;">
-70.7833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
250260
</td>
<td style="text-align:right;">
90060
</td>
<td style="text-align:right;">
83420.0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.0030590
</td>
<td style="text-align:right;">
0.0952175
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0910324
</td>
<td style="text-align:right;">
0.1445782
</td>
<td style="text-align:right;">
0.0013317
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
359
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
Rio Grande(BRA)
</td>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-32.16667
</td>
<td style="text-align:right;">
-52.0833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
2104980
</td>
<td style="text-align:right;">
153720
</td>
<td style="text-align:right;">
150355.7
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.0057851
</td>
<td style="text-align:right;">
0.0963644
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.0598570
</td>
<td style="text-align:right;">
0.2401649
</td>
<td style="text-align:right;">
0.0035508
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
385
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
Huangpu
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
23.09750
</td>
<td style="text-align:right;">
113.4241667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
37630620
</td>
<td style="text-align:right;">
2165970
</td>
<td style="text-align:right;">
2090590.0
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.0405746
</td>
<td style="text-align:right;">
0.0888171
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.0681315
</td>
<td style="text-align:right;">
0.0126834
</td>
<td style="text-align:right;">
0.0057916
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
Dover Strait
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.02092
</td>
<td style="text-align:right;">
1.3979300
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0831312
</td>
<td style="text-align:right;">
0.0971302
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0.0858716
</td>
<td style="text-align:right;">
0.1348892
</td>
<td style="text-align:right;">
0.0097551
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
Rochester(GBR)
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.38750
</td>
<td style="text-align:right;">
0.5097222
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
93600
</td>
<td style="text-align:right;">
93600
</td>
<td style="text-align:right;">
93600.0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0080017
</td>
<td style="text-align:right;">
0.0911162
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0692031
</td>
<td style="text-align:right;">
0.0339041
</td>
<td style="text-align:right;">
0.0022316
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
303
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
Sheerness
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.43778
</td>
<td style="text-align:right;">
0.7438889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0079569
</td>
<td style="text-align:right;">
0.0920695
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0274553
</td>
<td style="text-align:right;">
0.0315746
</td>
<td style="text-align:right;">
0.0014667
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
304
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
Thamesport
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.43417
</td>
<td style="text-align:right;">
0.6863889
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0003210
</td>
<td style="text-align:right;">
0.0837776
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0074338
</td>
<td style="text-align:right;">
0.0021470
</td>
<td style="text-align:right;">
0.0008325
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
308
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
Port William
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.66667
</td>
<td style="text-align:right;">
-57.7833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
216720
</td>
<td style="text-align:right;">
15120
</td>
<td style="text-align:right;">
43344.0
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0025999
</td>
<td style="text-align:right;">
0.0915332
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.0617076
</td>
<td style="text-align:right;">
0.0474450
</td>
<td style="text-align:right;">
0.0024051
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
368
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
Talcahuano
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-36.69306
</td>
<td style="text-align:right;">
-73.0986111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
33251520
</td>
<td style="text-align:right;">
1036320
</td>
<td style="text-align:right;">
2216768.0
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.0090424
</td>
<td style="text-align:right;">
0.0961328
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.1332907
</td>
<td style="text-align:right;">
0.2111741
</td>
<td style="text-align:right;">
0.0037171
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
370
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
Buenos Aires
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.59028
</td>
<td style="text-align:right;">
-58.3775000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
12548280
</td>
<td style="text-align:right;">
460650
</td>
<td style="text-align:right;">
1568535.0
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0120086
</td>
<td style="text-align:right;">
0.0972591
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.2071763
</td>
<td style="text-align:right;">
0.2059785
</td>
<td style="text-align:right;">
0.0039538
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
381
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
La Plata Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.74500
</td>
<td style="text-align:right;">
-57.8311111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
583800
</td>
<td style="text-align:right;">
48180
</td>
<td style="text-align:right;">
116760.0
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0004412
</td>
<td style="text-align:right;">
0.0915332
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.0640002
</td>
<td style="text-align:right;">
0.0853131
</td>
<td style="text-align:right;">
0.0019024
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
382
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
Port Louis
</td>
<td style="text-align:left;">
Mauritius
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-20.15000
</td>
<td style="text-align:right;">
57.4833333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11625720
</td>
<td style="text-align:right;">
5630760
</td>
<td style="text-align:right;">
3875240.0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.0022906
</td>
<td style="text-align:right;">
0.0929839
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0317265
</td>
<td style="text-align:right;">
0.0367904
</td>
<td style="text-align:right;">
0.0016519
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
436
</td>
</tr>
</tbody>
</table>

</div>

``` r
gateway_research <- port_networks$research %>%
  activate(edges) %>%
  filter(to_realm == "Southern Ocean") %>%
  activate(nodes) %>%
  mutate(strength_out = centrality_degree(mode = "out", weights = n_voyages)) %>%
  filter(strength_out > 0)
gateway_nodes_research <- gateway_research %>%
  activate(nodes) %>%
  filter(realm != "Southern Ocean") %>%
  arrange(desc(strength_out)) %>%
  dplyr::select(place, country, ecoregion, province, realm, strength_out, everything()) %>%
  ungroup() %>%
  as_tibble() %>%
  mutate(total_voyages_to = sum(strength_out)) %>%
  mutate(pcent = strength_out / total_voyages_to * 100)

gateway_nodes_research %>%
  dplyr::select(place, country, strength_out, pcent) %>%
  mutate(pcent = sprintf("%0.2f", pcent))
```

    ## # A tibble: 22 x 4
    ##    place           country          strength_out pcent
    ##    <chr>           <chr>                   <dbl> <chr>
    ##  1 Punta Arenas    Chile                     111 46.25
    ##  2 Hobart          Australia                  24 10.00
    ##  3 Cape Town       South Africa               23 9.58 
    ##  4 Ushuaia         Argentina                  17 7.08 
    ##  5 Mare Harbour    Falkland Islands           15 6.25 
    ##  6 Lyttelton       New Zealand                11 4.58 
    ##  7 Puerto Williams Chile                       9 3.75 
    ##  8 Stanley Harbour Falkland Islands            9 3.75 
    ##  9 Sydney          Australia                   3 1.25 
    ## 10 Fremantle       Australia                   3 1.25 
    ## # … with 12 more rows

### Supply

Supply ships going to coastal Antarctic locations depart from a wide
range of ports, primarily in Temperate South America. Of the Gateway
Cities, Punta Arenas had the most departures.

``` r
gateway_ports_results$supply
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
place
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
country
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
area
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
latitude
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
longitude
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
feature
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
betweenness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
closeness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_total
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_eigen
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_hub
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_pagerank
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
cluster
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
.tidygraph\_node\_index
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
Port William
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.666667
</td>
<td style="text-align:right;">
-57.78333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
159
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
20903940
</td>
<td style="text-align:right;">
7200
</td>
<td style="text-align:right;">
190035.82
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
0.0234783
</td>
<td style="text-align:right;">
0.1212210
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.6491172
</td>
<td style="text-align:right;">
0.5747149
</td>
<td style="text-align:right;">
0.0051455
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
577
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
Punta Arenas
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-53.166667
</td>
<td style="text-align:right;">
-70.90000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
153489660
</td>
<td style="text-align:right;">
491340
</td>
<td style="text-align:right;">
2692801.05
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
0.0380555
</td>
<td style="text-align:right;">
0.1153792
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
0.3021219
</td>
<td style="text-align:right;">
0.2624165
</td>
<td style="text-align:right;">
0.0056777
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
568
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Berkeley Sound
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.568500
</td>
<td style="text-align:right;">
-57.93500
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
186
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
53048760
</td>
<td style="text-align:right;">
180000
</td>
<td style="text-align:right;">
401884.55
</td>
<td style="text-align:right;">
132
</td>
<td style="text-align:right;">
0.0407497
</td>
<td style="text-align:right;">
0.1220449
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
0.8753560
</td>
<td style="text-align:right;">
0.6362415
</td>
<td style="text-align:right;">
0.0069549
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
574
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Montevideo
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.900000
</td>
<td style="text-align:right;">
-56.26667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
187
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
150261300
</td>
<td style="text-align:right;">
145260
</td>
<td style="text-align:right;">
873612.21
</td>
<td style="text-align:right;">
172
</td>
<td style="text-align:right;">
0.0512162
</td>
<td style="text-align:right;">
0.1210079
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0.6783402
</td>
<td style="text-align:right;">
0.4943933
</td>
<td style="text-align:right;">
0.0053589
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
608
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
Montevideo Alpha Zone
</td>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-35.044610
</td>
<td style="text-align:right;">
-56.05877
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
236
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
154427820
</td>
<td style="text-align:right;">
149340
</td>
<td style="text-align:right;">
714943.61
</td>
<td style="text-align:right;">
216
</td>
<td style="text-align:right;">
0.1344066
</td>
<td style="text-align:right;">
0.1256083
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
0.9886809
</td>
<td style="text-align:right;">
0.7531212
</td>
<td style="text-align:right;">
0.0100939
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
609
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
Buenos Aires
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.590278
</td>
<td style="text-align:right;">
-58.37750
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
73745580
</td>
<td style="text-align:right;">
600300
</td>
<td style="text-align:right;">
3206329.57
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.0176420
</td>
<td style="text-align:right;">
0.1140254
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.2912576
</td>
<td style="text-align:right;">
0.1384257
</td>
<td style="text-align:right;">
0.0034694
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
602
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
Ushuaia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-54.801944
</td>
<td style="text-align:right;">
-68.30278
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
14072640
</td>
<td style="text-align:right;">
487740
</td>
<td style="text-align:right;">
1563626.67
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.0060646
</td>
<td style="text-align:right;">
0.1069117
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.1020648
</td>
<td style="text-align:right;">
0.0646544
</td>
<td style="text-align:right;">
0.0028155
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
569
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
Lyttelton
</td>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Australia, New Zealand, New Guinea etc
</td>
<td style="text-align:right;">
-43.606389
</td>
<td style="text-align:right;">
172.72806
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
12960180
</td>
<td style="text-align:right;">
67200
</td>
<td style="text-align:right;">
480006.67
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.0351279
</td>
<td style="text-align:right;">
0.1146744
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.0639106
</td>
<td style="text-align:right;">
0.0525025
</td>
<td style="text-align:right;">
0.0033007
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
183
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
Puerto Williams
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-54.933333
</td>
<td style="text-align:right;">
-67.61667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
13334700
</td>
<td style="text-align:right;">
479760
</td>
<td style="text-align:right;">
606122.73
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.0046431
</td>
<td style="text-align:right;">
0.1095491
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.2440789
</td>
<td style="text-align:right;">
0.0874728
</td>
<td style="text-align:right;">
0.0027439
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
567
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
Bintulu
</td>
<td style="text-align:left;">
Malaysia
</td>
<td style="text-align:left;">
Sunda Shelf/Java Sea
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Vietnam, Thailand, Malaysia and Indonesia
</td>
<td style="text-align:right;">
3.166667
</td>
<td style="text-align:right;">
113.03333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.1081718
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.0499351
</td>
<td style="text-align:right;">
0.0395266
</td>
<td style="text-align:right;">
0.0003798
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
73
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
Cape Town
</td>
<td style="text-align:left;">
South Africa
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
South & East African coasts
</td>
<td style="text-align:right;">
-33.916667
</td>
<td style="text-align:right;">
18.43333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
23883420
</td>
<td style="text-align:right;">
379320
</td>
<td style="text-align:right;">
645497.84
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
0.0359887
</td>
<td style="text-align:right;">
0.1212210
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
0.4126917
</td>
<td style="text-align:right;">
0.3038665
</td>
<td style="text-align:right;">
0.0040349
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
640
</td>
</tr>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
Busan
</td>
<td style="text-align:left;">
South Korea
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
35.117222
</td>
<td style="text-align:right;">
129.04444
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
201
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
101990580
</td>
<td style="text-align:right;">
374400
</td>
<td style="text-align:right;">
589540.92
</td>
<td style="text-align:right;">
173
</td>
<td style="text-align:right;">
0.0396552
</td>
<td style="text-align:right;">
0.1192263
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
0.4430996
</td>
<td style="text-align:right;">
0.5865072
</td>
<td style="text-align:right;">
0.0054059
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
538
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Vietnam, Thailand, Malaysia and Indonesia
</td>
<td style="text-align:right;">
1.268889
</td>
<td style="text-align:right;">
103.83306
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:right;">
134
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
17006220
</td>
<td style="text-align:right;">
52500
</td>
<td style="text-align:right;">
134970.00
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
0.2930267
</td>
<td style="text-align:right;">
0.1260491
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.0191928
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
66
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:left;">
Nakhodka
</td>
<td style="text-align:left;">
Russia
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
42.798611
</td>
<td style="text-align:right;">
132.87694
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
3671640
</td>
<td style="text-align:right;">
258720
</td>
<td style="text-align:right;">
367164.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0.0041628
</td>
<td style="text-align:right;">
0.1143094
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.2261052
</td>
<td style="text-align:right;">
0.2641093
</td>
<td style="text-align:right;">
0.0020300
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
467
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
Zhoushan
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
30.000000
</td>
<td style="text-align:right;">
122.10000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
85180200
</td>
<td style="text-align:right;">
1523520
</td>
<td style="text-align:right;">
2241584.21
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
0.0211316
</td>
<td style="text-align:right;">
0.1151540
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
0.4660982
</td>
<td style="text-align:right;">
0.3457197
</td>
<td style="text-align:right;">
0.0050043
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
562
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
Cabo Negro
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-52.950000
</td>
<td style="text-align:right;">
-70.78333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0000824
</td>
<td style="text-align:right;">
0.1081860
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0753727
</td>
<td style="text-align:right;">
0.0431097
</td>
<td style="text-align:right;">
0.0006211
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
563
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
Stanley Harbour
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.683333
</td>
<td style="text-align:right;">
-57.83333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
15601620
</td>
<td style="text-align:right;">
84720
</td>
<td style="text-align:right;">
226110.43
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
0.0159026
</td>
<td style="text-align:right;">
0.1177309
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.5253284
</td>
<td style="text-align:right;">
0.3312047
</td>
<td style="text-align:right;">
0.0033907
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
578
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
La Plata Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-34.745000
</td>
<td style="text-align:right;">
-57.83111
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1668780
</td>
<td style="text-align:right;">
38610
</td>
<td style="text-align:right;">
83439.00
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.0162214
</td>
<td style="text-align:right;">
0.1109618
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.2018308
</td>
<td style="text-align:right;">
0.0716725
</td>
<td style="text-align:right;">
0.0028896
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
607
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:left;">
Hong Kong
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
22.286111
</td>
<td style="text-align:right;">
114.15750
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
3780180
</td>
<td style="text-align:right;">
51900
</td>
<td style="text-align:right;">
87911.16
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.0348051
</td>
<td style="text-align:right;">
0.1197970
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.3433939
</td>
<td style="text-align:right;">
0.3561686
</td>
<td style="text-align:right;">
0.0056876
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
Xiamen
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
China, Korea and Russia
</td>
<td style="text-align:right;">
24.457500
</td>
<td style="text-align:right;">
118.07139
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
627780
</td>
<td style="text-align:right;">
108360
</td>
<td style="text-align:right;">
156945.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0059485
</td>
<td style="text-align:right;">
0.1121064
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0473698
</td>
<td style="text-align:right;">
0.0644053
</td>
<td style="text-align:right;">
0.0017029
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
Las Palmas
</td>
<td style="text-align:left;">
Canary Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Spain / Portugal inc Atlantic Islands
</td>
<td style="text-align:right;">
28.133056
</td>
<td style="text-align:right;">
-15.43194
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
41731380
</td>
<td style="text-align:right;">
85200
</td>
<td style="text-align:right;">
485248.60
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
0.0795245
</td>
<td style="text-align:right;">
0.1212032
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
0.6544253
</td>
<td style="text-align:right;">
0.4487979
</td>
<td style="text-align:right;">
0.0114698
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
236
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
Dover Strait
</td>
<td style="text-align:left;">
U.K.
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
United Kingdom inc Eire
</td>
<td style="text-align:right;">
51.020920
</td>
<td style="text-align:right;">
1.39793
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.1212673
</td>
<td style="text-align:right;">
0.1226065
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
0.2397359
</td>
<td style="text-align:right;">
0.3669946
</td>
<td style="text-align:right;">
0.0126980
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
360
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Falkland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-51.683330
</td>
<td style="text-align:right;">
-57.83333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0000029
</td>
<td style="text-align:right;">
0.1089566
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0708035
</td>
<td style="text-align:right;">
0.0727969
</td>
<td style="text-align:right;">
0.0006546
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
575
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
San Antonio(CHL)
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-33.600000
</td>
<td style="text-align:right;">
-71.61667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
873780
</td>
<td style="text-align:right;">
250920
</td>
<td style="text-align:right;">
291260.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.0038902
</td>
<td style="text-align:right;">
0.1021645
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0547299
</td>
<td style="text-align:right;">
0.0058752
</td>
<td style="text-align:right;">
0.0011404
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
585
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
Valparaiso
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-33.016667
</td>
<td style="text-align:right;">
-71.63333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
65041860
</td>
<td style="text-align:right;">
729510
</td>
<td style="text-align:right;">
2501610.00
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
0.0185514
</td>
<td style="text-align:right;">
0.1099720
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.0659107
</td>
<td style="text-align:right;">
0.0777293
</td>
<td style="text-align:right;">
0.0031844
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
591
</td>
</tr>
<tr>
<td style="text-align:left;">
26
</td>
<td style="text-align:left;">
Bahia Blanca
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-38.776111
</td>
<td style="text-align:right;">
-62.28694
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
42699480
</td>
<td style="text-align:right;">
2081220
</td>
<td style="text-align:right;">
3881770.91
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.0164388
</td>
<td style="text-align:right;">
0.1152505
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
0.1573916
</td>
<td style="text-align:right;">
0.1847428
</td>
<td style="text-align:right;">
0.0033717
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
630
</td>
</tr>
<tr>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
Recalada Anch.
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-39.000000
</td>
<td style="text-align:right;">
-61.26667
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:right;">
131
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
27869880
</td>
<td style="text-align:right;">
106380
</td>
<td style="text-align:right;">
267979.62
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:right;">
0.0081020
</td>
<td style="text-align:right;">
0.1159461
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.5165876
</td>
<td style="text-align:right;">
0.3331113
</td>
<td style="text-align:right;">
0.0035720
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
634
</td>
</tr>
<tr>
<td style="text-align:left;">
28
</td>
<td style="text-align:left;">
Matadi
</td>
<td style="text-align:left;">
Congo
</td>
<td style="text-align:left;">
Gulf of Guinea South
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
-5.816667
</td>
<td style="text-align:right;">
13.45000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
10624080
</td>
<td style="text-align:right;">
578700
</td>
<td style="text-align:right;">
664005.00
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.0040592
</td>
<td style="text-align:right;">
0.1155891
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.2009993
</td>
<td style="text-align:right;">
0.2422948
</td>
<td style="text-align:right;">
0.0024096
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
669
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:left;">
Kakinada
</td>
<td style="text-align:left;">
India
</td>
<td style="text-align:left;">
Eastern India
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
India, Pakistan and Burma
</td>
<td style="text-align:right;">
17.000000
</td>
<td style="text-align:right;">
82.28333
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1044000
</td>
<td style="text-align:right;">
1044000
</td>
<td style="text-align:right;">
1044000.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0013745
</td>
<td style="text-align:right;">
0.1029797
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.0060664
</td>
<td style="text-align:right;">
0.0143402
</td>
<td style="text-align:right;">
0.0011292
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
758
</td>
</tr>
</tbody>
</table>

</div>

``` r
gateway_supply <- port_networks$supply %>%
  activate(edges) %>%
  filter(to_realm == "Southern Ocean") %>%
  activate(nodes) %>%
  mutate(strength_out = centrality_degree(mode = "out", weights = n_voyages)) %>%
  filter(strength_out > 0)
gateway_nodes_supply <- gateway_supply %>%
  activate(nodes) %>%
  filter(realm != "Southern Ocean") %>%
  arrange(desc(strength_out)) %>%
  dplyr::select(place, country, ecoregion, province, realm, strength_out, everything()) %>%
  ungroup() %>%
  as_tibble() %>%
  mutate(total_voyages_to = sum(strength_out)) %>%
  mutate(pcent = strength_out / total_voyages_to * 100)

gateway_nodes_supply %>%
  dplyr::select(place, country, strength_out, pcent) %>%
  mutate(pcent = sprintf("%0.2f", pcent))
```

    ## # A tibble: 29 x 4
    ##    place                 country          strength_out pcent
    ##    <chr>                 <chr>                   <dbl> <chr>
    ##  1 Port William          Falkland Islands           26 18.71
    ##  2 Punta Arenas          Chile                      14 10.07
    ##  3 Berkeley Sound        Falkland Islands           12 8.63 
    ##  4 Montevideo            Uruguay                    12 8.63 
    ##  5 Montevideo Alpha Zone Uruguay                    11 7.91 
    ##  6 Buenos Aires          Argentina                   8 5.76 
    ##  7 Ushuaia               Argentina                   7 5.04 
    ##  8 Lyttelton             New Zealand                 6 4.32 
    ##  9 Puerto Williams       Chile                       6 4.32 
    ## 10 Bintulu               Malaysia                    5 3.60 
    ## # … with 19 more rows

### Other

Very few ships and voyages fall into the category of “other”, but most
voyages by these ships left from either Punta Arenas or Ushuaia.

``` r
gateway_ports_results$other
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
place
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
country
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
area
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
latitude
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
longitude
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
feature
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
betweenness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
closeness\_centrality
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_total
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_eigen
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_hub
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
centrality\_pagerank
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
cluster
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
.tidygraph\_node\_index
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
Punta Arenas
</td>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
South America Pacific coast
</td>
<td style="text-align:right;">
-53.166667
</td>
<td style="text-align:right;">
-70.900000
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3044100
</td>
<td style="text-align:right;">
849660
</td>
<td style="text-align:right;">
1014700
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.2741505
</td>
<td style="text-align:right;">
0.0172058
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0008083
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0176774
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
92
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
Ushuaia
</td>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
South America Atlantic coast
</td>
<td style="text-align:right;">
-54.801944
</td>
<td style="text-align:right;">
-68.302778
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2334240
</td>
<td style="text-align:right;">
602340
</td>
<td style="text-align:right;">
583560
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.0993011
</td>
<td style="text-align:right;">
0.0172295
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.0000771
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0209452
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
93
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Apapa-Lagos
</td>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
6.438333
</td>
<td style="text-align:right;">
3.388611
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
379980
</td>
<td style="text-align:right;">
379980
</td>
<td style="text-align:right;">
379980
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.0421935
</td>
<td style="text-align:right;">
0.0168850
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.0000137
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0055110
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
101
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Bonny
</td>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Africa Atlantic coast
</td>
<td style="text-align:right;">
4.421667
</td>
<td style="text-align:right;">
7.150278
</td>
<td style="text-align:left;">
Port
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0164516
</td>
<td style="text-align:right;">
0.0164409
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0000023
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0038456
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
102
</td>
</tr>
</tbody>
</table>

</div>

``` r
gateway_other <- port_networks$other %>%
  activate(edges) %>%
  filter(to_realm == "Southern Ocean") %>%
  activate(nodes) %>%
  mutate(strength_out = centrality_degree(mode = "out", weights = n_voyages)) %>%
  filter(strength_out > 0)
gateway_nodes_other <- gateway_other %>%
  activate(nodes) %>%
  filter(realm != "Southern Ocean") %>%
  arrange(desc(strength_out)) %>%
  dplyr::select(place, country, ecoregion, province, realm, strength_out, everything()) %>%
  ungroup() %>%
  as_tibble() %>%
  mutate(total_voyages_to = sum(strength_out)) %>%
  mutate(pcent = strength_out / total_voyages_to * 100)

gateway_nodes_other %>%
  dplyr::select(place, country, strength_out, pcent) %>%
  mutate(pcent = sprintf("%0.2f", pcent))
```

    ## # A tibble: 4 x 4
    ##   place        country   strength_out pcent
    ##   <chr>        <chr>            <dbl> <chr>
    ## 1 Punta Arenas Chile                3 37.50
    ## 2 Ushuaia      Argentina            3 37.50
    ## 3 Apapa-Lagos  Nigeria              1 12.50
    ## 4 Bonny        Nigeria              1 12.50

## Main regions of activity to and from Antarctic ecoregions

``` r
gateway_region_results <- gateway_region_importance(list_of_eco_networks = ecoregion_networks, output_nodes = TRUE)
gateway_region_results$all
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
lat\_zone
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2702
</td>
<td style="text-align:right;">
2708
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
6395
</td>
<td style="text-align:right;">
141
</td>
<td style="text-align:right;">
1026201780
</td>
<td style="text-align:right;">
38880
</td>
<td style="text-align:right;">
160469.39
</td>
</tr>
<tr>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1993
</td>
<td style="text-align:right;">
2000
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
10442
</td>
<td style="text-align:right;">
114
</td>
<td style="text-align:right;">
1226667840
</td>
<td style="text-align:right;">
22260
</td>
<td style="text-align:right;">
117474.41
</td>
</tr>
<tr>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1128
</td>
<td style="text-align:right;">
1210
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
2264
</td>
<td style="text-align:right;">
123
</td>
<td style="text-align:right;">
1663244100
</td>
<td style="text-align:right;">
80340
</td>
<td style="text-align:right;">
1240301.34
</td>
</tr>
<tr>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
393
</td>
<td style="text-align:right;">
280
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1187
</td>
<td style="text-align:right;">
101
</td>
<td style="text-align:right;">
704934780
</td>
<td style="text-align:right;">
48150
</td>
<td style="text-align:right;">
845245.54
</td>
</tr>
<tr>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
351
</td>
<td style="text-align:right;">
352
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
348
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
10956780
</td>
<td style="text-align:right;">
27450
</td>
<td style="text-align:right;">
124508.86
</td>
</tr>
<tr>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
188
</td>
<td style="text-align:right;">
191
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
465
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
233187060
</td>
<td style="text-align:right;">
67440
</td>
<td style="text-align:right;">
501477.55
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
129
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
290
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
289506660
</td>
<td style="text-align:right;">
251250
</td>
<td style="text-align:right;">
998298.83
</td>
</tr>
<tr>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
105
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
460
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
210635640
</td>
<td style="text-align:right;">
50370
</td>
<td style="text-align:right;">
457903.57
</td>
</tr>
<tr>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
83
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1300
</td>
<td style="text-align:right;">
113
</td>
<td style="text-align:right;">
1586039640
</td>
<td style="text-align:right;">
77730
</td>
<td style="text-align:right;">
1369636.99
</td>
</tr>
<tr>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
98583120
</td>
<td style="text-align:right;">
314700
</td>
<td style="text-align:right;">
1146315.35
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
112
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
183761280
</td>
<td style="text-align:right;">
536580
</td>
<td style="text-align:right;">
1640725.71
</td>
</tr>
<tr>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
345
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
769298460
</td>
<td style="text-align:right;">
198300
</td>
<td style="text-align:right;">
2737716.94
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
123
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
230770200
</td>
<td style="text-align:right;">
400140
</td>
<td style="text-align:right;">
2331012.12
</td>
</tr>
<tr>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
107
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
69799620
</td>
<td style="text-align:right;">
317520
</td>
<td style="text-align:right;">
652332.90
</td>
</tr>
<tr>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
277
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
342509640
</td>
<td style="text-align:right;">
81900
</td>
<td style="text-align:right;">
1831602.35
</td>
</tr>
<tr>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
3031920
</td>
<td style="text-align:right;">
46320
</td>
<td style="text-align:right;">
112293.33
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
31330200
</td>
<td style="text-align:right;">
74760
</td>
<td style="text-align:right;">
949400.00
</td>
</tr>
<tr>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
41190240
</td>
<td style="text-align:right;">
121260
</td>
<td style="text-align:right;">
710176.55
</td>
</tr>
<tr>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
4557
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
1164620760
</td>
<td style="text-align:right;">
59340
</td>
<td style="text-align:right;">
329361.07
</td>
</tr>
<tr>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1320
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
1523519280
</td>
<td style="text-align:right;">
93300
</td>
<td style="text-align:right;">
1330584.52
</td>
</tr>
<tr>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
20414880
</td>
<td style="text-align:right;">
111360
</td>
<td style="text-align:right;">
268616.84
</td>
</tr>
<tr>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
924180
</td>
<td style="text-align:right;">
59700
</td>
<td style="text-align:right;">
92418.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
220
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
84809640
</td>
<td style="text-align:right;">
106800
</td>
<td style="text-align:right;">
584894.07
</td>
</tr>
<tr>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
723
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
281896920
</td>
<td style="text-align:right;">
59760
</td>
<td style="text-align:right;">
495425.17
</td>
</tr>
<tr>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
75830040
</td>
<td style="text-align:right;">
133830
</td>
<td style="text-align:right;">
1579792.50
</td>
</tr>
<tr>
<td style="text-align:left;">
Fiji Islands
</td>
<td style="text-align:left;">
Tropical Southwestern Pacific
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
17416920
</td>
<td style="text-align:right;">
51360
</td>
<td style="text-align:right;">
527785.45
</td>
</tr>
<tr>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
510650400
</td>
<td style="text-align:right;">
47340
</td>
<td style="text-align:right;">
5055944.55
</td>
</tr>
<tr>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
4215780
</td>
<td style="text-align:right;">
45360
</td>
<td style="text-align:right;">
156140.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Sunda Shelf/Java Sea
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
4171320
</td>
<td style="text-align:right;">
55080
</td>
<td style="text-align:right;">
106956.92
</td>
</tr>
<tr>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
226
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
198863820
</td>
<td style="text-align:right;">
89400
</td>
<td style="text-align:right;">
1410381.70
</td>
</tr>
<tr>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
411
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
46256580
</td>
<td style="text-align:right;">
52260
</td>
<td style="text-align:right;">
132540.34
</td>
</tr>
<tr>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1512
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
917266800
</td>
<td style="text-align:right;">
69630
</td>
<td style="text-align:right;">
637876.77
</td>
</tr>
<tr>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
722
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
437749740
</td>
<td style="text-align:right;">
82860
</td>
<td style="text-align:right;">
736952.42
</td>
</tr>
<tr>
<td style="text-align:left;">
Manning-Hawkesbury
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
14886960
</td>
<td style="text-align:right;">
147120
</td>
<td style="text-align:right;">
303815.51
</td>
</tr>
<tr>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
352
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
192198660
</td>
<td style="text-align:right;">
105450
</td>
<td style="text-align:right;">
636419.40
</td>
</tr>
<tr>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
693
</td>
<td style="text-align:right;">
65
</td>
<td style="text-align:right;">
29522160
</td>
<td style="text-align:right;">
44820
</td>
<td style="text-align:right;">
72894.22
</td>
</tr>
<tr>
<td style="text-align:left;">
Eastern India
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3704700
</td>
<td style="text-align:right;">
169620
</td>
<td style="text-align:right;">
284976.92
</td>
</tr>
<tr>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
696
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
422925180
</td>
<td style="text-align:right;">
212190
</td>
<td style="text-align:right;">
794972.14
</td>
</tr>
<tr>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
93
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
12907080
</td>
<td style="text-align:right;">
72780
</td>
<td style="text-align:right;">
184386.86
</td>
</tr>
<tr>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
310
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
157148040
</td>
<td style="text-align:right;">
114090
</td>
<td style="text-align:right;">
618693.07
</td>
</tr>
<tr>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
268
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
177086280
</td>
<td style="text-align:right;">
73860
</td>
<td style="text-align:right;">
989308.83
</td>
</tr>
<tr>
<td style="text-align:left;">
New Caledonia
</td>
<td style="text-align:left;">
Tropical Southwestern Pacific
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
21074760
</td>
<td style="text-align:right;">
101280
</td>
<td style="text-align:right;">
602136.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Southern Norway
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1277
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
93252540
</td>
<td style="text-align:right;">
47040
</td>
<td style="text-align:right;">
110488.79
</td>
</tr>
<tr>
<td style="text-align:left;">
Easter Island
</td>
<td style="text-align:left;">
Easter Island
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
28156920
</td>
<td style="text-align:right;">
892350
</td>
<td style="text-align:right;">
1173205.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Gulf of Aden
</td>
<td style="text-align:left;">
Red Sea and Gulf of Aden
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
2419320
</td>
<td style="text-align:right;">
431040
</td>
<td style="text-align:right;">
483864.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Gulf of Guinea South
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
21625320
</td>
<td style="text-align:right;">
428580
</td>
<td style="text-align:right;">
584468.11
</td>
</tr>
<tr>
<td style="text-align:left;">
Sahelian Upwelling
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
2380140
</td>
<td style="text-align:right;">
45180
</td>
<td style="text-align:right;">
70004.12
</td>
</tr>
<tr>
<td style="text-align:left;">
Cape Verde
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
248
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
39009360
</td>
<td style="text-align:right;">
41070
</td>
<td style="text-align:right;">
295525.45
</td>
</tr>
<tr>
<td style="text-align:left;">
Yellow Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
433
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
115523520
</td>
<td style="text-align:right;">
180090
</td>
<td style="text-align:right;">
330067.20
</td>
</tr>
<tr>
<td style="text-align:left;">
Celtic Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1403
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
184433520
</td>
<td style="text-align:right;">
47040
</td>
<td style="text-align:right;">
150557.98
</td>
</tr>
<tr>
<td style="text-align:left;">
Society Islands
</td>
<td style="text-align:left;">
Southeast Polynesia
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
288
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
191348520
</td>
<td style="text-align:right;">
157320
</td>
<td style="text-align:right;">
1518639.05
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1605600
</td>
<td style="text-align:right;">
190800
</td>
<td style="text-align:right;">
229371.43
</td>
</tr>
<tr>
<td style="text-align:left;">
Central Kuroshio Current
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1494
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
914460120
</td>
<td style="text-align:right;">
76500
</td>
<td style="text-align:right;">
700735.72
</td>
</tr>
<tr>
<td style="text-align:left;">
Chiloense
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
306
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
112138440
</td>
<td style="text-align:right;">
40770
</td>
<td style="text-align:right;">
934487.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Eastern Brazil
</td>
<td style="text-align:left;">
Tropical Southwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
6164760
</td>
<td style="text-align:right;">
43200
</td>
<td style="text-align:right;">
116316.23
</td>
</tr>
<tr>
<td style="text-align:left;">
Guayaquil
</td>
<td style="text-align:left;">
Tropical East Pacific
</td>
<td style="text-align:left;">
Tropical Eastern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
113
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
19000560
</td>
<td style="text-align:right;">
51000
</td>
<td style="text-align:right;">
208797.36
</td>
</tr>
<tr>
<td style="text-align:left;">
Houtman
</td>
<td style="text-align:left;">
West Central Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
275400
</td>
<td style="text-align:right;">
37050
</td>
<td style="text-align:right;">
68850.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Natal
</td>
<td style="text-align:left;">
Agulhas
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
180
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
46278360
</td>
<td style="text-align:right;">
55590
</td>
<td style="text-align:right;">
304462.89
</td>
</tr>
<tr>
<td style="text-align:left;">
Puget Trough/Georgia Basin
</td>
<td style="text-align:left;">
Cold Temperate Northeast Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
410
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
147090480
</td>
<td style="text-align:right;">
39150
</td>
<td style="text-align:right;">
385053.61
</td>
</tr>
<tr>
<td style="text-align:left;">
Northern Norway and Finnmark
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1998
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
250398180
</td>
<td style="text-align:right;">
51840
</td>
<td style="text-align:right;">
199838.93
</td>
</tr>
<tr>
<td style="text-align:left;">
Alboran Sea
</td>
<td style="text-align:left;">
Mediterranean Sea
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
604
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
75351420
</td>
<td style="text-align:right;">
41190
</td>
<td style="text-align:right;">
150102.43
</td>
</tr>
</tbody>
</table>

</div>

``` r
ecoregion_networks$all %>% 
  activate(nodes) %>% 
  filter(realm == "Southern Ocean") %>% 
  as_tibble() %>% 
  summarise(total_voyages = sum(n_voyages))
```

    ## # A tibble: 1 x 1
    ##   total_voyages
    ##           <dbl>
    ## 1         18775

``` r
ecoregion_networks$all %>% 
  activate(nodes) %>% 
  filter(ecoregion == "South Shetland Islands" | ecoregion == "Antarctic Peninsula") %>% 
  as_tibble() %>% 
  summarise(total_voyages = sum(n_voyages))
```

    ## # A tibble: 1 x 1
    ##   total_voyages
    ##           <dbl>
    ## 1         16837

``` r
16837/18775
```

    ## [1] 0.8967776

fishing

``` r
gateway_region_results$fishing
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
lat\_zone
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
360
</td>
<td style="text-align:right;">
361
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
774
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
170221440
</td>
<td style="text-align:right;">
59670
</td>
<td style="text-align:right;">
219924.34
</td>
</tr>
<tr>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
308
</td>
<td style="text-align:right;">
307
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
745
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
396077160
</td>
<td style="text-align:right;">
154080
</td>
<td style="text-align:right;">
531647.19
</td>
</tr>
<tr>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
1034330100
</td>
<td style="text-align:right;">
4173360
</td>
<td style="text-align:right;">
10447778.79
</td>
</tr>
<tr>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
184
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
180505260
</td>
<td style="text-align:right;">
196590
</td>
<td style="text-align:right;">
981006.85
</td>
</tr>
<tr>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
83497680
</td>
<td style="text-align:right;">
478260
</td>
<td style="text-align:right;">
1575427.92
</td>
</tr>
<tr>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
170
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
290327280
</td>
<td style="text-align:right;">
180000
</td>
<td style="text-align:right;">
2419394.00
</td>
</tr>
<tr>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
2172960
</td>
<td style="text-align:right;">
19500
</td>
<td style="text-align:right;">
724320.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
24286320
</td>
<td style="text-align:right;">
76500
</td>
<td style="text-align:right;">
592349.27
</td>
</tr>
<tr>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
129
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
768080040
</td>
<td style="text-align:right;">
1242120
</td>
<td style="text-align:right;">
6919640.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
65
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
150589560
</td>
<td style="text-align:right;">
287340
</td>
<td style="text-align:right;">
2841312.45
</td>
</tr>
<tr>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
470760
</td>
<td style="text-align:right;">
46320
</td>
<td style="text-align:right;">
52306.67
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
28279560
</td>
<td style="text-align:right;">
2052180
</td>
<td style="text-align:right;">
2827956.00
</td>
</tr>
<tr>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
143
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
862517040
</td>
<td style="text-align:right;">
483480
</td>
<td style="text-align:right;">
7435491.72
</td>
</tr>
<tr>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
189890220
</td>
<td style="text-align:right;">
4192380
</td>
<td style="text-align:right;">
5754249.09
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
73696680
</td>
<td style="text-align:right;">
866340
</td>
<td style="text-align:right;">
7369668.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Fiji Islands
</td>
<td style="text-align:left;">
Tropical Southwestern Pacific
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
707760
</td>
<td style="text-align:right;">
141660
</td>
<td style="text-align:right;">
235920.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
165459660
</td>
<td style="text-align:right;">
4873320
</td>
<td style="text-align:right;">
6618386.40
</td>
</tr>
<tr>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
62961060
</td>
<td style="text-align:right;">
5307900
</td>
<td style="text-align:right;">
6296106.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
34886760
</td>
<td style="text-align:right;">
3974280
</td>
<td style="text-align:right;">
4983822.86
</td>
</tr>
<tr>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
493651500
</td>
<td style="text-align:right;">
18772920
</td>
<td style="text-align:right;">
27425083.33
</td>
</tr>
<tr>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
16880280
</td>
<td style="text-align:right;">
3446040
</td>
<td style="text-align:right;">
3376056.00
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
119340
</td>
<td style="text-align:right;">
59670
</td>
<td style="text-align:right;">
59670.00
</td>
</tr>
<tr>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
494940
</td>
<td style="text-align:right;">
247470
</td>
<td style="text-align:right;">
247470.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2795220
</td>
<td style="text-align:right;">
182430
</td>
<td style="text-align:right;">
349402.50
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1132260
</td>
<td style="text-align:right;">
129120
</td>
<td style="text-align:right;">
226452.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
142347240
</td>
<td style="text-align:right;">
126390
</td>
<td style="text-align:right;">
1977045.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1318620
</td>
<td style="text-align:right;">
1318620
</td>
<td style="text-align:right;">
1318620.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Sahelian Upwelling
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
245580
</td>
<td style="text-align:right;">
245580
</td>
<td style="text-align:right;">
245580.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1066680
</td>
<td style="text-align:right;">
201300
</td>
<td style="text-align:right;">
177780.00
</td>
</tr>
<tr>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6037260
</td>
<td style="text-align:right;">
234900
</td>
<td style="text-align:right;">
862465.71
</td>
</tr>
</tbody>
</table>

</div>

tourism

``` r
gateway_region_results$tourism
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
lat\_zone
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1793
</td>
<td style="text-align:right;">
1789
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
3451
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
234893280
</td>
<td style="text-align:right;">
34320
</td>
<td style="text-align:right;">
68065.28
</td>
</tr>
<tr>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1362
</td>
<td style="text-align:right;">
1368
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
8242
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
531953040
</td>
<td style="text-align:right;">
19200
</td>
<td style="text-align:right;">
64541.74
</td>
</tr>
<tr>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
874
</td>
<td style="text-align:right;">
966
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1743
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
97651920
</td>
<td style="text-align:right;">
53340
</td>
<td style="text-align:right;">
99441.87
</td>
</tr>
<tr>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
285
</td>
<td style="text-align:right;">
287
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
287
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
5511120
</td>
<td style="text-align:right;">
27450
</td>
<td style="text-align:right;">
74474.59
</td>
</tr>
<tr>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
263
</td>
<td style="text-align:right;">
136
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
424
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
20718900
</td>
<td style="text-align:right;">
31980
</td>
<td style="text-align:right;">
62975.38
</td>
</tr>
<tr>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
3921960
</td>
<td style="text-align:right;">
28440
</td>
<td style="text-align:right;">
43098.46
</td>
</tr>
<tr>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
164
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
20525700
</td>
<td style="text-align:right;">
17370
</td>
<td style="text-align:right;">
125156.71
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4144860
</td>
<td style="text-align:right;">
61560
</td>
<td style="text-align:right;">
153513.33
</td>
</tr>
<tr>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
33557340
</td>
<td style="text-align:right;">
122250
</td>
<td style="text-align:right;">
645333.46
</td>
</tr>
<tr>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
548
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
73110780
</td>
<td style="text-align:right;">
48660
</td>
<td style="text-align:right;">
147698.55
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1239420
</td>
<td style="text-align:right;">
85860
</td>
<td style="text-align:right;">
88530.00
</td>
</tr>
<tr>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
2490780
</td>
<td style="text-align:right;">
42720
</td>
<td style="text-align:right;">
108294.78
</td>
</tr>
<tr>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
852180
</td>
<td style="text-align:right;">
41070
</td>
<td style="text-align:right;">
142030.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
996240
</td>
<td style="text-align:right;">
262440
</td>
<td style="text-align:right;">
332080.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
8315580
</td>
<td style="text-align:right;">
116100
</td>
<td style="text-align:right;">
251987.27
</td>
</tr>
<tr>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
161
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
20973240
</td>
<td style="text-align:right;">
72120
</td>
<td style="text-align:right;">
154215.00
</td>
</tr>
<tr>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
2787
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
447146700
</td>
<td style="text-align:right;">
49260
</td>
<td style="text-align:right;">
197067.74
</td>
</tr>
<tr>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
5519340
</td>
<td style="text-align:right;">
47460
</td>
<td style="text-align:right;">
324667.06
</td>
</tr>
<tr>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
113604420
</td>
<td style="text-align:right;">
134700
</td>
<td style="text-align:right;">
1747760.31
</td>
</tr>
<tr>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
114
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
12326520
</td>
<td style="text-align:right;">
50640
</td>
<td style="text-align:right;">
178645.22
</td>
</tr>
<tr>
<td style="text-align:left;">
Cape Verde
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
234
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
38050080
</td>
<td style="text-align:right;">
41520
</td>
<td style="text-align:right;">
306855.48
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
972000
</td>
<td style="text-align:right;">
169200
</td>
<td style="text-align:right;">
162000.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1275
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
652303980
</td>
<td style="text-align:right;">
67860
</td>
<td style="text-align:right;">
530328.44
</td>
</tr>
<tr>
<td style="text-align:left;">
Chiloense
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
247
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
20049600
</td>
<td style="text-align:right;">
38700
</td>
<td style="text-align:right;">
206696.91
</td>
</tr>
<tr>
<td style="text-align:left;">
Society Islands
</td>
<td style="text-align:left;">
Southeast Polynesia
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
275
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
185043420
</td>
<td style="text-align:right;">
135510
</td>
<td style="text-align:right;">
1595201.90
</td>
</tr>
</tbody>
</table>

</div>

research

``` r
gateway_region_results$research
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
lat\_zone
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
361
</td>
<td style="text-align:right;">
367
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
1432
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
346716180
</td>
<td style="text-align:right;">
40290
</td>
<td style="text-align:right;">
242120.24
</td>
</tr>
<tr>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
248
</td>
<td style="text-align:right;">
248
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
1284
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
264848880
</td>
<td style="text-align:right;">
28890
</td>
<td style="text-align:right;">
206268.60
</td>
</tr>
<tr>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
160
</td>
<td style="text-align:right;">
159
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
283
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
164591520
</td>
<td style="text-align:right;">
380610
</td>
<td style="text-align:right;">
839752.65
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
230
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
210704940
</td>
<td style="text-align:right;">
315390
</td>
<td style="text-align:right;">
916108.43
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
221387940
</td>
<td style="text-align:right;">
723120
</td>
<td style="text-align:right;">
3689799.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
136
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
27265380
</td>
<td style="text-align:right;">
226530
</td>
<td style="text-align:right;">
400961.47
</td>
</tr>
<tr>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
202
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
103399980
</td>
<td style="text-align:right;">
96360
</td>
<td style="text-align:right;">
511881.09
</td>
</tr>
<tr>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
22193160
</td>
<td style="text-align:right;">
67620
</td>
<td style="text-align:right;">
249361.35
</td>
</tr>
<tr>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
97
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
64812480
</td>
<td style="text-align:right;">
317520
</td>
<td style="text-align:right;">
668169.90
</td>
</tr>
<tr>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
147
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
382229700
</td>
<td style="text-align:right;">
282750
</td>
<td style="text-align:right;">
3412765.18
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
38713380
</td>
<td style="text-align:right;">
224040
</td>
<td style="text-align:right;">
744488.08
</td>
</tr>
<tr>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
637740
</td>
<td style="text-align:right;">
26580
</td>
<td style="text-align:right;">
91105.71
</td>
</tr>
<tr>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
131706300
</td>
<td style="text-align:right;">
550680
</td>
<td style="text-align:right;">
3377084.62
</td>
</tr>
<tr>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
13964940
</td>
<td style="text-align:right;">
161940
</td>
<td style="text-align:right;">
481549.66
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
30884880
</td>
<td style="text-align:right;">
101160
</td>
<td style="text-align:right;">
1235395.20
</td>
</tr>
<tr>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
16782660
</td>
<td style="text-align:right;">
269790
</td>
<td style="text-align:right;">
419566.50
</td>
</tr>
<tr>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1708980
</td>
<td style="text-align:right;">
73410
</td>
<td style="text-align:right;">
142415.00
</td>
</tr>
<tr>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
311700
</td>
<td style="text-align:right;">
48630
</td>
<td style="text-align:right;">
51950.00
</td>
</tr>
<tr>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
595
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
285530400
</td>
<td style="text-align:right;">
146910
</td>
<td style="text-align:right;">
602384.81
</td>
</tr>
<tr>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
28929060
</td>
<td style="text-align:right;">
217140
</td>
<td style="text-align:right;">
590388.98
</td>
</tr>
<tr>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
8933880
</td>
<td style="text-align:right;">
448020
</td>
<td style="text-align:right;">
1276268.57
</td>
</tr>
<tr>
<td style="text-align:left;">
Manning-Hawkesbury
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5303280
</td>
<td style="text-align:right;">
349140
</td>
<td style="text-align:right;">
482116.36
</td>
</tr>
<tr>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
39622500
</td>
<td style="text-align:right;">
106590
</td>
<td style="text-align:right;">
825468.75
</td>
</tr>
<tr>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2104980
</td>
<td style="text-align:right;">
153720
</td>
<td style="text-align:right;">
150355.71
</td>
</tr>
<tr>
<td style="text-align:left;">
New Caledonia
</td>
<td style="text-align:left;">
Tropical Southwestern Pacific
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
10858560
</td>
<td style="text-align:right;">
427560
</td>
<td style="text-align:right;">
2171712.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
147835440
</td>
<td style="text-align:right;">
243360
</td>
<td style="text-align:right;">
2687917.09
</td>
</tr>
<tr>
<td style="text-align:left;">
Southern Norway
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1041060
</td>
<td style="text-align:right;">
178740
</td>
<td style="text-align:right;">
260265.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
33251520
</td>
<td style="text-align:right;">
1036320
</td>
<td style="text-align:right;">
2216768.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Easter Island
</td>
<td style="text-align:left;">
Easter Island
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1434780
</td>
<td style="text-align:right;">
717390
</td>
<td style="text-align:right;">
717390.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Gulf of Aden
</td>
<td style="text-align:left;">
Red Sea and Gulf of Aden
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
19140
</td>
<td style="text-align:right;">
19140
</td>
<td style="text-align:right;">
19140.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Celtic Seas
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
35841900
</td>
<td style="text-align:right;">
252060
</td>
<td style="text-align:right;">
1086118.18
</td>
</tr>
<tr>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
45159960
</td>
<td style="text-align:right;">
716940
</td>
<td style="text-align:right;">
1806398.40
</td>
</tr>
<tr>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
87
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
12111120
</td>
<td style="text-align:right;">
107700
</td>
<td style="text-align:right;">
165905.75
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
487800
</td>
<td style="text-align:right;">
71610
</td>
<td style="text-align:right;">
121950.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Central Kuroshio Current
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
253
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
718011300
</td>
<td style="text-align:right;">
401700
</td>
<td style="text-align:right;">
3177041.15
</td>
</tr>
<tr>
<td style="text-align:left;">
Eastern Brazil
</td>
<td style="text-align:left;">
Tropical Southwestern Atlantic
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2176620
</td>
<td style="text-align:right;">
1088310
</td>
<td style="text-align:right;">
1088310.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Puget Trough/Georgia Basin
</td>
<td style="text-align:left;">
Cold Temperate Northeast Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
68319900
</td>
<td style="text-align:right;">
2183280
</td>
<td style="text-align:right;">
4554660.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Society Islands
</td>
<td style="text-align:left;">
Southeast Polynesia
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
6234840
</td>
<td style="text-align:right;">
486000
</td>
<td style="text-align:right;">
779355.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
110302200
</td>
<td style="text-align:right;">
355080
</td>
<td style="text-align:right;">
3342490.91
</td>
</tr>
<tr>
<td style="text-align:left;">
Yellow Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
16371360
</td>
<td style="text-align:right;">
1385520
</td>
<td style="text-align:right;">
1169382.86
</td>
</tr>
<tr>
<td style="text-align:left;">
Alboran Sea
</td>
<td style="text-align:left;">
Mediterranean Sea
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
36639900
</td>
<td style="text-align:right;">
181080
</td>
<td style="text-align:right;">
990267.57
</td>
</tr>
</tbody>
</table>

</div>

supply

``` r
gateway_region_results$supply
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
lat\_zone
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
178
</td>
<td style="text-align:right;">
180
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
685
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
257309160
</td>
<td style="text-align:right;">
76440
</td>
<td style="text-align:right;">
375633.81
</td>
</tr>
<tr>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
70
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
152
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
28704540
</td>
<td style="text-align:right;">
60690
</td>
<td style="text-align:right;">
188845.66
</td>
</tr>
<tr>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
457
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
366623220
</td>
<td style="text-align:right;">
165600
</td>
<td style="text-align:right;">
1156540.13
</td>
</tr>
<tr>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
561
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
710135040
</td>
<td style="text-align:right;">
109800
</td>
<td style="text-align:right;">
1417435.21
</td>
</tr>
<tr>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
101
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
26566680
</td>
<td style="text-align:right;">
85980
</td>
<td style="text-align:right;">
263036.44
</td>
</tr>
<tr>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
101
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
360686820
</td>
<td style="text-align:right;">
1902000
</td>
<td style="text-align:right;">
6557942.18
</td>
</tr>
<tr>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2634960
</td>
<td style="text-align:right;">
155370
</td>
<td style="text-align:right;">
658740.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
62302740
</td>
<td style="text-align:right;">
69150
</td>
<td style="text-align:right;">
1198129.62
</td>
</tr>
<tr>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
8586600
</td>
<td style="text-align:right;">
45510
</td>
<td style="text-align:right;">
186665.22
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
48859860
</td>
<td style="text-align:right;">
698520
</td>
<td style="text-align:right;">
1320536.76
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
25688160
</td>
<td style="text-align:right;">
359550
</td>
<td style="text-align:right;">
1167643.64
</td>
</tr>
<tr>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
901
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
538707960
</td>
<td style="text-align:right;">
99840
</td>
<td style="text-align:right;">
681908.81
</td>
</tr>
<tr>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
122874780
</td>
<td style="text-align:right;">
198720
</td>
<td style="text-align:right;">
2409309.41
</td>
</tr>
<tr>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
325980
</td>
<td style="text-align:right;">
62370
</td>
<td style="text-align:right;">
54330.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Sunda Shelf/Java Sea
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2066820
</td>
<td style="text-align:right;">
105690
</td>
<td style="text-align:right;">
206682.00
</td>
</tr>
<tr>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
1100
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
389278680
</td>
<td style="text-align:right;">
88920
</td>
<td style="text-align:right;">
521122.73
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
180
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
81950160
</td>
<td style="text-align:right;">
133410
</td>
<td style="text-align:right;">
671722.62
</td>
</tr>
<tr>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
243
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
24861420
</td>
<td style="text-align:right;">
54540
</td>
<td style="text-align:right;">
120103.48
</td>
</tr>
<tr>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
117540
</td>
<td style="text-align:right;">
58770
</td>
<td style="text-align:right;">
58770.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2045400
</td>
<td style="text-align:right;">
276780
</td>
<td style="text-align:right;">
409080.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Eastern India
</td>
<td style="text-align:left;">
Bay of Bengal
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3454320
</td>
<td style="text-align:right;">
169740
</td>
<td style="text-align:right;">
314029.09
</td>
</tr>
<tr>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
178
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
71037300
</td>
<td style="text-align:right;">
213690
</td>
<td style="text-align:right;">
486556.85
</td>
</tr>
<tr>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South China Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
200
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
21247140
</td>
<td style="text-align:right;">
91380
</td>
<td style="text-align:right;">
122110.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
124260
</td>
<td style="text-align:right;">
124260
</td>
<td style="text-align:right;">
124260.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
120262680
</td>
<td style="text-align:right;">
675420
</td>
<td style="text-align:right;">
2073494.48
</td>
</tr>
<tr>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
2172420
</td>
<td style="text-align:right;">
157920
</td>
<td style="text-align:right;">
241380.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
95
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
42491280
</td>
<td style="text-align:right;">
93360
</td>
<td style="text-align:right;">
466937.14
</td>
</tr>
<tr>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
157
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
33894720
</td>
<td style="text-align:right;">
50220
</td>
<td style="text-align:right;">
240388.09
</td>
</tr>
<tr>
<td style="text-align:left;">
Gulf of Guinea South
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
21540000
</td>
<td style="text-align:right;">
474900
</td>
<td style="text-align:right;">
633529.41
</td>
</tr>
<tr>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
2075700
</td>
<td style="text-align:right;">
33240
</td>
<td style="text-align:right;">
71575.86
</td>
</tr>
<tr>
<td style="text-align:left;">
Yellow Sea
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
386
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
93061080
</td>
<td style="text-align:right;">
179940
</td>
<td style="text-align:right;">
293568.08
</td>
</tr>
<tr>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
8953080
</td>
<td style="text-align:right;">
56040
</td>
<td style="text-align:right;">
526651.76
</td>
</tr>
<tr>
<td style="text-align:left;">
Guayaquil
</td>
<td style="text-align:left;">
Tropical East Pacific
</td>
<td style="text-align:left;">
Tropical Eastern Pacific
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
15998160
</td>
<td style="text-align:right;">
413250
</td>
<td style="text-align:right;">
533272.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Houtman
</td>
<td style="text-align:left;">
West Central Australian Shelf
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
168960
</td>
<td style="text-align:right;">
168960
</td>
<td style="text-align:right;">
168960.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Natal
</td>
<td style="text-align:left;">
Agulhas
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
12638760
</td>
<td style="text-align:right;">
55110
</td>
<td style="text-align:right;">
225692.14
</td>
</tr>
<tr>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
10681620
</td>
<td style="text-align:right;">
59280
</td>
<td style="text-align:right;">
201540.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Northern Norway and Finnmark
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
74580060
</td>
<td style="text-align:right;">
533160
</td>
<td style="text-align:right;">
1065429.43
</td>
</tr>
</tbody>
</table>

</div>

other

``` r
gateway_region_results$other
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_out
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
strength\_in
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
lat\_zone
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
22491360
</td>
<td style="text-align:right;">
929400
</td>
<td style="text-align:right;">
1730104.6
</td>
</tr>
<tr>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
17061720
</td>
<td style="text-align:right;">
110700
</td>
<td style="text-align:right;">
321919.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1623120
</td>
<td style="text-align:right;">
205860
</td>
<td style="text-align:right;">
405780.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Polar
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5084220
</td>
<td style="text-align:right;">
71220
</td>
<td style="text-align:right;">
267590.5
</td>
</tr>
<tr>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
36627720
</td>
<td style="text-align:right;">
233700
</td>
<td style="text-align:right;">
939172.3
</td>
</tr>
<tr>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5983740
</td>
<td style="text-align:right;">
559800
</td>
<td style="text-align:right;">
664860.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Gulf of Guinea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Tropical
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
58577160
</td>
<td style="text-align:right;">
2570580
</td>
<td style="text-align:right;">
19525720.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Temperate
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4564800
</td>
<td style="text-align:right;">
166260
</td>
<td style="text-align:right;">
652114.3
</td>
</tr>
</tbody>
</table>

</div>

How about the connections between ecoregions around the Southern Ocean?

Whem looking at all ships, by far the strongest connections are between
the Antarctic Peninsula, the South Shetland Islands and the Channels and
Fjords of Southern Chile, which combined account for 5344 voyages
between regions. In comparison, the connections with the next highest
amount were between South Georgia and the Malvinas/Falklands and from
there to the South Shetland Islands and the Antarctic Peninsula, yet
combined they only account for 798 voyages between regions. The highest
ranked connection between regions that do not include the Antarctic
Peninsula, South Shetland Islands, South Georgia, Malvinas/Falklands,
Channels and Fjords of Southern Chile or South Orkney Islands is between
Bassian (Tasmania, Australia) and East Antarctic Wilkes Land (a vast
area of East Antarctica that has numerous research stations). Even so,
this connection accounted for 95 voyages in both directions, equivalent
to 6% of the 1547 between Southern Chile/South America and the South
Shetland Islands

``` r
gateway_region_edge_results <- gateway_region_importance(list_of_eco_networks = ecoregion_networks, output_nodes = FALSE)
gateway_region_edge_results$all
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
move
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_realm
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
internal
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
South Shetland Islands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1582
</td>
<td style="text-align:right;">
102
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:right;">
46181280
</td>
<td style="text-align:right;">
16320
</td>
<td style="text-align:right;">
29191.71
</td>
<td style="text-align:right;">
1582
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Shetland Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1564
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
136
</td>
<td style="text-align:right;">
51024600
</td>
<td style="text-align:right;">
13920
</td>
<td style="text-align:right;">
32645.30
</td>
<td style="text-align:right;">
1563
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Shetland Islands\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
812
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
93
</td>
<td style="text-align:right;">
116547780
</td>
<td style="text-align:right;">
176760
</td>
<td style="text-align:right;">
223700.15
</td>
<td style="text-align:right;">
521
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Shetland Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
735
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
69126840
</td>
<td style="text-align:right;">
123990
</td>
<td style="text-align:right;">
142824.05
</td>
<td style="text-align:right;">
484
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
349
</td>
<td style="text-align:right;">
56
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
85680300
</td>
<td style="text-align:right;">
277830
</td>
<td style="text-align:right;">
303830.85
</td>
<td style="text-align:right;">
282
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
302
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
56
</td>
<td style="text-align:right;">
19070940
</td>
<td style="text-align:right;">
43320
</td>
<td style="text-align:right;">
87481.38
</td>
<td style="text-align:right;">
218
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Georgia
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
220
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
17261700
</td>
<td style="text-align:right;">
345420
</td>
<td style="text-align:right;">
401434.88
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
South Shetland Islands\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
37179060
</td>
<td style="text-align:right;">
238740
</td>
<td style="text-align:right;">
375546.06
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Shetland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
113
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
56
</td>
<td style="text-align:right;">
33127860
</td>
<td style="text-align:right;">
233880
</td>
<td style="text-align:right;">
394379.29
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
South Georgia\_South Shetland Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
107
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
7755360
</td>
<td style="text-align:right;">
237870
</td>
<td style="text-align:right;">
228098.82
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
South Georgia\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
97
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
14190600
</td>
<td style="text-align:right;">
546660
</td>
<td style="text-align:right;">
788366.67
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
South Orkney Islands\_South Shetland Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
7035360
</td>
<td style="text-align:right;">
28980
</td>
<td style="text-align:right;">
79048.99
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Bassian
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
37505880
</td>
<td style="text-align:right;">
699630
</td>
<td style="text-align:right;">
852406.36
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Bassian\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
21954960
</td>
<td style="text-align:right;">
488790
</td>
<td style="text-align:right;">
548874.00
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
South Georgia\_Antarctic Peninsula
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
3259620
</td>
<td style="text-align:right;">
173400
</td>
<td style="text-align:right;">
217308.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:left;">
South Shetland Islands\_Rio de la Plata
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
51888600
</td>
<td style="text-align:right;">
728250
</td>
<td style="text-align:right;">
1235442.86
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
South Georgia\_South Orkney Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
2305680
</td>
<td style="text-align:right;">
107460
</td>
<td style="text-align:right;">
256186.67
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
South Shetland Islands\_South Georgia
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
2227440
</td>
<td style="text-align:right;">
295890
</td>
<td style="text-align:right;">
371240.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
South Shetland Islands\_South Orkney Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
3109440
</td>
<td style="text-align:right;">
33150
</td>
<td style="text-align:right;">
74034.29
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Georgia
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
5284500
</td>
<td style="text-align:right;">
476880
</td>
<td style="text-align:right;">
480409.09
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Orkney Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
10829700
</td>
<td style="text-align:right;">
290640
</td>
<td style="text-align:right;">
373437.93
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
Ross Sea\_Central New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
33070380
</td>
<td style="text-align:right;">
795540
</td>
<td style="text-align:right;">
1066786.45
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Rio de la Plata\_South Shetland Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
29856420
</td>
<td style="text-align:right;">
623430
</td>
<td style="text-align:right;">
995214.00
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
South Orkney Islands\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
6010680
</td>
<td style="text-align:right;">
206580
</td>
<td style="text-align:right;">
333926.67
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
8866440
</td>
<td style="text-align:right;">
244500
</td>
<td style="text-align:right;">
521555.29
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Ross Sea\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
5117160
</td>
<td style="text-align:right;">
28050
</td>
<td style="text-align:right;">
213215.00
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Ross Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
8862120
</td>
<td style="text-align:right;">
113580
</td>
<td style="text-align:right;">
385309.57
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Central New Zealand\_Ross Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
22046880
</td>
<td style="text-align:right;">
524790
</td>
<td style="text-align:right;">
1377930.00
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Rio de la Plata\_South Orkney Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
9637980
</td>
<td style="text-align:right;">
486600
</td>
<td style="text-align:right;">
481899.00
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Orkney Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
7301040
</td>
<td style="text-align:right;">
371640
</td>
<td style="text-align:right;">
486736.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Malvinas/Falklands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1456380
</td>
<td style="text-align:right;">
82080
</td>
<td style="text-align:right;">
132398.18
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
South Orkney Islands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3985200
</td>
<td style="text-align:right;">
86640
</td>
<td style="text-align:right;">
221400.00
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Weddell Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1658160
</td>
<td style="text-align:right;">
25440
</td>
<td style="text-align:right;">
97538.82
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Ross Sea\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
8110500
</td>
<td style="text-align:right;">
304920
</td>
<td style="text-align:right;">
477088.24
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
38511660
</td>
<td style="text-align:right;">
1586790
</td>
<td style="text-align:right;">
2406978.75
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:left;">
South Georgia\_Rio de la Plata
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
3318060
</td>
<td style="text-align:right;">
520590
</td>
<td style="text-align:right;">
553010.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:left;">
South Orkney Islands\_Rio de la Plata
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
19804080
</td>
<td style="text-align:right;">
682740
</td>
<td style="text-align:right;">
1320272.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
146
</td>
<td style="text-align:left;">
South Shetland Islands\_Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
8653500
</td>
<td style="text-align:right;">
536400
</td>
<td style="text-align:right;">
576900.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
16407720
</td>
<td style="text-align:right;">
945090
</td>
<td style="text-align:right;">
1171980.00
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Peter the First Island
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
1952100
</td>
<td style="text-align:right;">
34200
</td>
<td style="text-align:right;">
130140.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
Namaqua\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
8904900
</td>
<td style="text-align:right;">
569400
</td>
<td style="text-align:right;">
684992.31
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
South Georgia\_Namaqua
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
4569180
</td>
<td style="text-align:right;">
1118700
</td>
<td style="text-align:right;">
1523060.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
136
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
South New Zealand\_Ross Sea
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
8266140
</td>
<td style="text-align:right;">
679320
</td>
<td style="text-align:right;">
635856.92
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_East Antarctic Enderby Land
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1157040
</td>
<td style="text-align:right;">
22380
</td>
<td style="text-align:right;">
89003.08
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
South Orkney Islands\_South Georgia
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
851820
</td>
<td style="text-align:right;">
217680
</td>
<td style="text-align:right;">
212955.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:left;">
South Shetland Islands\_Mascarene Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
24031560
</td>
<td style="text-align:right;">
1656900
</td>
<td style="text-align:right;">
1848581.54
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Georgia
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
3858000
</td>
<td style="text-align:right;">
466020
</td>
<td style="text-align:right;">
643000.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
Namaqua\_Weddell Sea
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8596560
</td>
<td style="text-align:right;">
776730
</td>
<td style="text-align:right;">
859656.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
Rio de la Plata\_South Georgia
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2491320
</td>
<td style="text-align:right;">
453360
</td>
<td style="text-align:right;">
622830.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Georgia\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
739740
</td>
<td style="text-align:right;">
369870
</td>
<td style="text-align:right;">
369870.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
Weddell Sea\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3198420
</td>
<td style="text-align:right;">
95130
</td>
<td style="text-align:right;">
266535.00
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Namaqua
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
11641260
</td>
<td style="text-align:right;">
623160
</td>
<td style="text-align:right;">
1164126.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1115220
</td>
<td style="text-align:right;">
57960
</td>
<td style="text-align:right;">
101383.64
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Orkney Islands\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
8350680
</td>
<td style="text-align:right;">
581580
</td>
<td style="text-align:right;">
835068.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_East Antarctic Enderby Land
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1230120
</td>
<td style="text-align:right;">
16290
</td>
<td style="text-align:right;">
123012.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Central New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
9470520
</td>
<td style="text-align:right;">
872430
</td>
<td style="text-align:right;">
947052.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
165
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
North Sea\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
-21167820
</td>
<td style="text-align:right;">
-365220
</td>
<td style="text-align:right;">
-2645977.50
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
Weddell Sea\_Namaqua
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
9320040
</td>
<td style="text-align:right;">
848880
</td>
<td style="text-align:right;">
932004.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
458580
</td>
<td style="text-align:right;">
20940
</td>
<td style="text-align:right;">
50953.33
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5477100
</td>
<td style="text-align:right;">
20880
</td>
<td style="text-align:right;">
608566.67
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Orkney Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1697940
</td>
<td style="text-align:right;">
126420
</td>
<td style="text-align:right;">
188660.00
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Leeuwin\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5887680
</td>
<td style="text-align:right;">
744000
</td>
<td style="text-align:right;">
654186.67
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Namaqua\_South Shetland Islands
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
16478940
</td>
<td style="text-align:right;">
1879500
</td>
<td style="text-align:right;">
2059867.50
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
136
</td>
<td style="text-align:left;">
Ross Sea\_South New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7677420
</td>
<td style="text-align:right;">
750060
</td>
<td style="text-align:right;">
853046.67
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Ross Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
33446340
</td>
<td style="text-align:right;">
4840860
</td>
<td style="text-align:right;">
4180792.50
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
246360
</td>
<td style="text-align:right;">
18450
</td>
<td style="text-align:right;">
30795.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
136
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_South New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5532240
</td>
<td style="text-align:right;">
674850
</td>
<td style="text-align:right;">
691530.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
East China Sea\_South Shetland Islands
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
15621540
</td>
<td style="text-align:right;">
2376120
</td>
<td style="text-align:right;">
2603590.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Rio de la Plata\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3691380
</td>
<td style="text-align:right;">
444870
</td>
<td style="text-align:right;">
461422.50
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
146
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf\_South Shetland Islands
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2430420
</td>
<td style="text-align:right;">
451200
</td>
<td style="text-align:right;">
486084.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Central New Zealand\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2938320
</td>
<td style="text-align:right;">
547080
</td>
<td style="text-align:right;">
587664.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
165
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_North Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1983780
</td>
<td style="text-align:right;">
991890
</td>
<td style="text-align:right;">
991890.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Leeuwin
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
8698080
</td>
<td style="text-align:right;">
1114920
</td>
<td style="text-align:right;">
1242582.86
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Peter the First Island\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1922700
</td>
<td style="text-align:right;">
15240
</td>
<td style="text-align:right;">
274671.43
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Peter the First Island\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
881520
</td>
<td style="text-align:right;">
117900
</td>
<td style="text-align:right;">
125931.43
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Weddell Sea\_South Orkney Islands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2901720
</td>
<td style="text-align:right;">
283920
</td>
<td style="text-align:right;">
414531.43
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2469240
</td>
<td style="text-align:right;">
949440
</td>
<td style="text-align:right;">
823080.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Peter the First Island
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
836520
</td>
<td style="text-align:right;">
102630
</td>
<td style="text-align:right;">
139420.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Araucanian\_South Shetland Islands
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7112040
</td>
<td style="text-align:right;">
783270
</td>
<td style="text-align:right;">
1778010.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Central New Zealand\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4466040
</td>
<td style="text-align:right;">
550140
</td>
<td style="text-align:right;">
1488680.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Namaqua
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5682900
</td>
<td style="text-align:right;">
1258680
</td>
<td style="text-align:right;">
1136580.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Central New Zealand
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
7026480
</td>
<td style="text-align:right;">
1349880
</td>
<td style="text-align:right;">
1405296.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Ross Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
314880
</td>
<td style="text-align:right;">
41880
</td>
<td style="text-align:right;">
62976.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
165
</td>
<td style="text-align:left;">
Antarctic Peninsula\_North Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
8289600
</td>
<td style="text-align:right;">
19740
</td>
<td style="text-align:right;">
1657920.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Bassian\_Ross Sea
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3443160
</td>
<td style="text-align:right;">
470640
</td>
<td style="text-align:right;">
688632.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_Namaqua
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4557900
</td>
<td style="text-align:right;">
915000
</td>
<td style="text-align:right;">
911580.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
East China Sea\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4872060
</td>
<td style="text-align:right;">
2436030
</td>
<td style="text-align:right;">
2436030.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Mascarene Islands\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6558060
</td>
<td style="text-align:right;">
1582740
</td>
<td style="text-align:right;">
1639515.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
Namaqua\_East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2808600
</td>
<td style="text-align:right;">
559200
</td>
<td style="text-align:right;">
561720.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Namaqua\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3815460
</td>
<td style="text-align:right;">
731100
</td>
<td style="text-align:right;">
763092.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
146
</td>
<td style="text-align:left;">
South Georgia\_Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
136
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
South New Zealand\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3445800
</td>
<td style="text-align:right;">
660030
</td>
<td style="text-align:right;">
861450.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
South Sandwich Islands\_Weddell Sea
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
922200
</td>
<td style="text-align:right;">
180000
</td>
<td style="text-align:right;">
184440.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
162
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Sunda Shelf/Java Sea\_South Shetland Islands
</td>
<td style="text-align:left;">
Sunda Shelf/Java Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_East China Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
15311460
</td>
<td style="text-align:right;">
3768090
</td>
<td style="text-align:right;">
3827865.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Central Peru\_South Shetland Islands
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4212840
</td>
<td style="text-align:right;">
1044480
</td>
<td style="text-align:right;">
1053210.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Peter the First Island
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1179480
</td>
<td style="text-align:right;">
346770
</td>
<td style="text-align:right;">
294870.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_East China Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7469820
</td>
<td style="text-align:right;">
994200
</td>
<td style="text-align:right;">
1867455.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Mascarene Islands
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
13564560
</td>
<td style="text-align:right;">
3407730
</td>
<td style="text-align:right;">
3391140.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Leeuwin\_Ross Sea
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
6280560
</td>
<td style="text-align:right;">
1023600
</td>
<td style="text-align:right;">
1570140.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Malvinas/Falklands\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
29112120
</td>
<td style="text-align:right;">
7039530
</td>
<td style="text-align:right;">
7278030.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Namaqua\_South Orkney Islands
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5047860
</td>
<td style="text-align:right;">
1069020
</td>
<td style="text-align:right;">
1682620.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
165
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
North Sea\_Antarctic Peninsula
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
34080
</td>
<td style="text-align:right;">
17040
</td>
<td style="text-align:right;">
17040.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Peter the First Island\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3204960
</td>
<td style="text-align:right;">
582030
</td>
<td style="text-align:right;">
801240.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Ross Sea\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5415180
</td>
<td style="text-align:right;">
1265580
</td>
<td style="text-align:right;">
1353795.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:left;">
Ross Sea\_Peter the First Island
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2408820
</td>
<td style="text-align:right;">
580920
</td>
<td style="text-align:right;">
602205.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
162
</td>
<td style="text-align:left;">
South Shetland Islands\_Sunda Shelf/Java Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Sunda Shelf/Java Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
11580
</td>
<td style="text-align:right;">
1470
</td>
<td style="text-align:right;">
2895.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
Weddell Sea\_South Georgia
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
676500
</td>
<td style="text-align:right;">
676500
</td>
<td style="text-align:right;">
676500.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:left;">
Antarctic Peninsula\_North Patagonian Gulfs
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
219240
</td>
<td style="text-align:right;">
219240
</td>
<td style="text-align:right;">
219240.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Rio de la Plata
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1591920
</td>
<td style="text-align:right;">
505920
</td>
<td style="text-align:right;">
530640.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
130
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Southeastern Brazil
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3710400
</td>
<td style="text-align:right;">
1639440
</td>
<td style="text-align:right;">
1236800.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
8623500
</td>
<td style="text-align:right;">
2278860
</td>
<td style="text-align:right;">
2874500.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Fiji Islands\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Fiji Islands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5183340
</td>
<td style="text-align:right;">
5183340
</td>
<td style="text-align:right;">
5183340.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Fiji Islands\_Ross Sea
</td>
<td style="text-align:left;">
Fiji Islands
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
11307480
</td>
<td style="text-align:right;">
5653740
</td>
<td style="text-align:right;">
5653740.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Malacca Strait\_South Shetland Islands
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6474000
</td>
<td style="text-align:right;">
3237000
</td>
<td style="text-align:right;">
3237000.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Manning-Hawkesbury\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Manning-Hawkesbury
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1200900
</td>
<td style="text-align:right;">
406860
</td>
<td style="text-align:right;">
400300.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
Namaqua\_South Georgia
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
522540
</td>
<td style="text-align:right;">
522540
</td>
<td style="text-align:right;">
522540.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
90
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
North Patagonian Gulfs\_South Shetland Islands
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
Peter the First Island\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1379880
</td>
<td style="text-align:right;">
462480
</td>
<td style="text-align:right;">
459960.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Peter the First Island\_South Shetland Islands
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4031940
</td>
<td style="text-align:right;">
1887480
</td>
<td style="text-align:right;">
1343980.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Rio de la Plata\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5645700
</td>
<td style="text-align:right;">
2822850
</td>
<td style="text-align:right;">
2822850.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
Ross Sea\_East China Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
10537860
</td>
<td style="text-align:right;">
3681840
</td>
<td style="text-align:right;">
3512620.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
South Orkney Islands\_Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
22251300
</td>
<td style="text-align:right;">
9635940
</td>
<td style="text-align:right;">
7417100.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
South Orkney Islands\_Weddell Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
866280
</td>
<td style="text-align:right;">
307860
</td>
<td style="text-align:right;">
288760.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
South Sandwich Islands\_South Orkney Islands
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
354900
</td>
<td style="text-align:right;">
118200
</td>
<td style="text-align:right;">
118300.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:left;">
South Shetland Islands\_Namaqua
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4461120
</td>
<td style="text-align:right;">
1577520
</td>
<td style="text-align:right;">
1487040.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:left;">
South Shetland Islands\_North Patagonian Gulfs
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
281460
</td>
<td style="text-align:right;">
281460
</td>
<td style="text-align:right;">
281460.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
Weddell Sea\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2441700
</td>
<td style="text-align:right;">
761220
</td>
<td style="text-align:right;">
813900.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:left;">
Weddell Sea\_South Sandwich Islands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
573840
</td>
<td style="text-align:right;">
189000
</td>
<td style="text-align:right;">
191280.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
Azores Canaries Madeira\_South Georgia
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2719080
</td>
<td style="text-align:right;">
2719080
</td>
<td style="text-align:right;">
2719080.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Azores Canaries Madeira\_South Orkney Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1814580
</td>
<td style="text-align:right;">
907290
</td>
<td style="text-align:right;">
907290.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Azores Canaries Madeira\_South Shetland Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4415220
</td>
<td style="text-align:right;">
2207610
</td>
<td style="text-align:right;">
2207610.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Baltic Sea\_Ross Sea
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-9180
</td>
<td style="text-align:right;">
-9180
</td>
<td style="text-align:right;">
-9180.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1653360
</td>
<td style="text-align:right;">
826680
</td>
<td style="text-align:right;">
826680.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Ross Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
19830840
</td>
<td style="text-align:right;">
9915420
</td>
<td style="text-align:right;">
9915420.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Gulf of Guinea Central\_South Shetland Islands
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
Leeuwin\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
897660
</td>
<td style="text-align:right;">
897660
</td>
<td style="text-align:right;">
897660.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Sandwich Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
946500
</td>
<td style="text-align:right;">
473250
</td>
<td style="text-align:right;">
473250.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
Malvinas/Falklands\_Weddell Sea
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
90
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
North Patagonian Gulfs\_South Georgia
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
815640
</td>
<td style="text-align:right;">
815640
</td>
<td style="text-align:right;">
815640.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
165
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
North Sea\_South Shetland Islands
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7734780
</td>
<td style="text-align:right;">
7734780
</td>
<td style="text-align:right;">
7734780.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Peter the First Island\_Ross Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1078200
</td>
<td style="text-align:right;">
539100
</td>
<td style="text-align:right;">
539100.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Rio de la Plata\_Ross Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11404500
</td>
<td style="text-align:right;">
5702250
</td>
<td style="text-align:right;">
5702250.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
109
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Rio Grande\_South Shetland Islands
</td>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
114
</td>
<td style="text-align:left;">
Ross Sea\_Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4634940
</td>
<td style="text-align:right;">
2317470
</td>
<td style="text-align:right;">
2317470.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
114
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Sea of Japan/East Sea\_South Shetland Islands
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5030580
</td>
<td style="text-align:right;">
2515290
</td>
<td style="text-align:right;">
2515290.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
South Georgia\_Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
South Georgia\_Cape Verde
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Cape Verde
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
857400
</td>
<td style="text-align:right;">
857400
</td>
<td style="text-align:right;">
857400.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
South Georgia\_Weddell Sea
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:left;">
South Orkney Islands\_South Sandwich Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
192960
</td>
<td style="text-align:right;">
96480
</td>
<td style="text-align:right;">
96480.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
146
</td>
<td style="text-align:left;">
South Orkney Islands\_Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
968520
</td>
<td style="text-align:right;">
484260
</td>
<td style="text-align:right;">
484260.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:left;">
South Shetland Islands\_Malacca Strait
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5835540
</td>
<td style="text-align:right;">
2917770
</td>
<td style="text-align:right;">
2917770.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
130
</td>
<td style="text-align:left;">
South Shetland Islands\_Southeastern Brazil
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
843780
</td>
<td style="text-align:right;">
843780
</td>
<td style="text-align:right;">
843780.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Southern China\_South Shetland Islands
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Weddell Sea\_South Shetland Islands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2327580
</td>
<td style="text-align:right;">
1163790
</td>
<td style="text-align:right;">
1163790.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
562680
</td>
<td style="text-align:right;">
562680
</td>
<td style="text-align:right;">
562680.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Fiji Islands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Fiji Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1745640
</td>
<td style="text-align:right;">
1745640
</td>
<td style="text-align:right;">
1745640.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
107
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Puget Trough/Georgia Basin
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Puget Trough/Georgia Basin
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4431720
</td>
<td style="text-align:right;">
4431720
</td>
<td style="text-align:right;">
4431720.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Rio de la Plata
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1740840
</td>
<td style="text-align:right;">
1740840
</td>
<td style="text-align:right;">
1740840.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_South Orkney Islands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1524660
</td>
<td style="text-align:right;">
1524660
</td>
<td style="text-align:right;">
1524660.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Azores Canaries Madeira
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2566500
</td>
<td style="text-align:right;">
2566500
</td>
<td style="text-align:right;">
2566500.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Central Chile
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4010880
</td>
<td style="text-align:right;">
4010880
</td>
<td style="text-align:right;">
4010880.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Chiloense
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Chiloense
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Ross Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
746760
</td>
<td style="text-align:right;">
746760
</td>
<td style="text-align:right;">
746760.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
139
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Southwestern Caribbean
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4680
</td>
<td style="text-align:right;">
4680
</td>
<td style="text-align:right;">
4680.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
146
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
523080
</td>
<td style="text-align:right;">
523080
</td>
<td style="text-align:right;">
523080.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Azores Canaries Madeira\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1503060
</td>
<td style="text-align:right;">
1503060
</td>
<td style="text-align:right;">
1503060.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
Baltic Sea\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-37030080
</td>
<td style="text-align:right;">
-37030080
</td>
<td style="text-align:right;">
-37030080.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Central Chile\_South Shetland Islands
</td>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
577560
</td>
<td style="text-align:right;">
577560
</td>
<td style="text-align:right;">
577560.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Central New Zealand\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Central Peru\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1383780
</td>
<td style="text-align:right;">
1383780
</td>
<td style="text-align:right;">
1383780.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Weddell Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1154460
</td>
<td style="text-align:right;">
1154460
</td>
<td style="text-align:right;">
1154460.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
167
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Northern Norway and Finnmark
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Northern Norway and Finnmark
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1277400
</td>
<td style="text-align:right;">
1277400
</td>
<td style="text-align:right;">
1277400.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
114
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Sea of Japan/East Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1320
</td>
<td style="text-align:right;">
1320
</td>
<td style="text-align:right;">
1320.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_South Orkney Islands
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
378300
</td>
<td style="text-align:right;">
378300
</td>
<td style="text-align:right;">
378300.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_South Shetland Islands
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
115620
</td>
<td style="text-align:right;">
115620
</td>
<td style="text-align:right;">
115620.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1167120
</td>
<td style="text-align:right;">
1167120
</td>
<td style="text-align:right;">
1167120.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_South Sandwich Islands
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
631020
</td>
<td style="text-align:right;">
631020
</td>
<td style="text-align:right;">
631020.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_South Shetland Islands
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1121040
</td>
<td style="text-align:right;">
1121040
</td>
<td style="text-align:right;">
1121040.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
267360
</td>
<td style="text-align:right;">
267360
</td>
<td style="text-align:right;">
267360.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Baltic Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
770820
</td>
<td style="text-align:right;">
770820
</td>
<td style="text-align:right;">
770820.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Central Kuroshio Current
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Central Kuroshio Current
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3565440
</td>
<td style="text-align:right;">
3565440
</td>
<td style="text-align:right;">
3565440.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2100420
</td>
<td style="text-align:right;">
2100420
</td>
<td style="text-align:right;">
2100420.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
243720
</td>
<td style="text-align:right;">
243720
</td>
<td style="text-align:right;">
243720.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_East China Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8295600
</td>
<td style="text-align:right;">
8295600
</td>
<td style="text-align:right;">
8295600.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Houtman
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Houtman
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
852660
</td>
<td style="text-align:right;">
852660
</td>
<td style="text-align:right;">
852660.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Malvinas/Falklands
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1459740
</td>
<td style="text-align:right;">
1459740
</td>
<td style="text-align:right;">
1459740.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Manning-Hawkesbury
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Manning-Hawkesbury
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_South Orkney Islands
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1216200
</td>
<td style="text-align:right;">
1216200
</td>
<td style="text-align:right;">
1216200.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
East China Sea\_Antarctic Peninsula
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3373740
</td>
<td style="text-align:right;">
3373740
</td>
<td style="text-align:right;">
3373740.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
East China Sea\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2117640
</td>
<td style="text-align:right;">
2117640
</td>
<td style="text-align:right;">
2117640.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Easter Island\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Easter Island
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2278560
</td>
<td style="text-align:right;">
2278560
</td>
<td style="text-align:right;">
2278560.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Eastern India\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Eastern India
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Eastern India\_South Shetland Islands
</td>
<td style="text-align:left;">
Eastern India
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-997320
</td>
<td style="text-align:right;">
-997320
</td>
<td style="text-align:right;">
-997320.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
Gulf of Aden\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Gulf of Aden
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8943420
</td>
<td style="text-align:right;">
8943420
</td>
<td style="text-align:right;">
8943420.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
172
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Gulf of Guinea South\_South Shetland Islands
</td>
<td style="text-align:left;">
Gulf of Guinea South
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1932000
</td>
<td style="text-align:right;">
1932000
</td>
<td style="text-align:right;">
1932000.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Malacca Strait\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1351440
</td>
<td style="text-align:right;">
1351440
</td>
<td style="text-align:right;">
1351440.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Malacca Strait\_South Orkney Islands
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
87
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:left;">
New Caledonia\_Ross Sea
</td>
<td style="text-align:left;">
New Caledonia
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
519180
</td>
<td style="text-align:right;">
519180
</td>
<td style="text-align:right;">
519180.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
165
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
North Sea\_South Orkney Islands
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2049780
</td>
<td style="text-align:right;">
2049780
</td>
<td style="text-align:right;">
2049780.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
Peter the First Island\_Central New Zealand
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5358120
</td>
<td style="text-align:right;">
5358120
</td>
<td style="text-align:right;">
5358120.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:left;">
Peter the First Island\_Rio de la Plata
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
859440
</td>
<td style="text-align:right;">
859440
</td>
<td style="text-align:right;">
859440.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:left;">
Peter the First Island\_South Sandwich Islands
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
879180
</td>
<td style="text-align:right;">
879180
</td>
<td style="text-align:right;">
879180.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:left;">
Rio de la Plata\_Peter the First Island
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1206540
</td>
<td style="text-align:right;">
1206540
</td>
<td style="text-align:right;">
1206540.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:left;">
Rio de la Plata\_South Sandwich Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
561000
</td>
<td style="text-align:right;">
561000
</td>
<td style="text-align:right;">
561000.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
180
</td>
<td style="text-align:left;">
Ross Sea\_Alboran Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Alboran Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3127140
</td>
<td style="text-align:right;">
3127140
</td>
<td style="text-align:right;">
3127140.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Ross Sea\_Baltic Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1380
</td>
<td style="text-align:right;">
1380
</td>
<td style="text-align:right;">
1380.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
Ross Sea\_Celtic Seas
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Celtic Seas
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
Ross Sea\_Central Chile
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1321260
</td>
<td style="text-align:right;">
1321260
</td>
<td style="text-align:right;">
1321260.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:left;">
Ross Sea\_Leeuwin
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1912980
</td>
<td style="text-align:right;">
1912980
</td>
<td style="text-align:right;">
1912980.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
87
</td>
<td style="text-align:left;">
Ross Sea\_New Caledonia
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
New Caledonia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2522340
</td>
<td style="text-align:right;">
2522340
</td>
<td style="text-align:right;">
2522340.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:left;">
Ross Sea\_Rio de la Plata
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1562760
</td>
<td style="text-align:right;">
1562760
</td>
<td style="text-align:right;">
1562760.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
Ross Sea\_Society Islands
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Society Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1543860
</td>
<td style="text-align:right;">
1543860
</td>
<td style="text-align:right;">
1543860.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
158
</td>
<td style="text-align:left;">
Ross Sea\_Yellow Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Yellow Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
174
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:left;">
Sahelian Upwelling\_South Shetland Islands
</td>
<td style="text-align:left;">
Sahelian Upwelling
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1273200
</td>
<td style="text-align:right;">
1273200
</td>
<td style="text-align:right;">
1273200.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
114
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
Sea of Japan/East Sea\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
South Georgia\_Araucanian
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
South Georgia\_Celtic Seas
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Celtic Seas
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:left;">
South Georgia\_Central Peru
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:left;">
South Georgia\_Mascarene Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:left;">
South Georgia\_North Patagonian Gulfs
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:left;">
South Georgia\_South Sandwich Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
122
</td>
<td style="text-align:right;">
140
</td>
<td style="text-align:left;">
South Georgia\_St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1268400
</td>
<td style="text-align:right;">
1268400
</td>
<td style="text-align:right;">
1268400.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
South Orkney Islands\_Araucanian
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7227420
</td>
<td style="text-align:right;">
7227420
</td>
<td style="text-align:right;">
7227420.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
South Orkney Islands\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
282420
</td>
<td style="text-align:right;">
282420
</td>
<td style="text-align:right;">
282420.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:left;">
South Orkney Islands\_Mascarene Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2410260
</td>
<td style="text-align:right;">
2410260
</td>
<td style="text-align:right;">
2410260.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
165
</td>
<td style="text-align:left;">
South Orkney Islands\_North Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3545340
</td>
<td style="text-align:right;">
3545340
</td>
<td style="text-align:right;">
3545340.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
139
</td>
<td style="text-align:left;">
South Orkney Islands\_Southwestern Caribbean
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
10500
</td>
<td style="text-align:right;">
10500
</td>
<td style="text-align:right;">
10500.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
158
</td>
<td style="text-align:left;">
South Orkney Islands\_Yellow Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Yellow Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
6331620
</td>
<td style="text-align:right;">
6331620
</td>
<td style="text-align:right;">
6331620.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
South Sandwich Islands\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
304500
</td>
<td style="text-align:right;">
304500
</td>
<td style="text-align:right;">
304500.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:left;">
South Sandwich Islands\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5605380
</td>
<td style="text-align:right;">
5605380
</td>
<td style="text-align:right;">
5605380.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
South Sandwich Islands\_South Georgia
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
South Shetland Islands\_Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1916520
</td>
<td style="text-align:right;">
1916520
</td>
<td style="text-align:right;">
1916520.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
South Shetland Islands\_Baltic Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3781020
</td>
<td style="text-align:right;">
3781020
</td>
<td style="text-align:right;">
3781020.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
South Shetland Islands\_Cape Verde
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Cape Verde
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
South Shetland Islands\_Central Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Central Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1221420
</td>
<td style="text-align:right;">
1221420
</td>
<td style="text-align:right;">
1221420.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:left;">
South Shetland Islands\_Central Peru
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2377320
</td>
<td style="text-align:right;">
2377320
</td>
<td style="text-align:right;">
2377320.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
South Shetland Islands\_East China Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:left;">
South Shetland Islands\_Eastern Brazil
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Eastern Brazil
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3048840
</td>
<td style="text-align:right;">
3048840
</td>
<td style="text-align:right;">
3048840.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
South Shetland Islands\_Eastern India
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Eastern India
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
156840
</td>
<td style="text-align:right;">
156840
</td>
<td style="text-align:right;">
156840.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:left;">
South Shetland Islands\_Guayaquil
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Guayaquil
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Eastern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2332020
</td>
<td style="text-align:right;">
2332020
</td>
<td style="text-align:right;">
2332020.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:left;">
South Shetland Islands\_Gulf of Guinea Central
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Gulf of Guinea Central
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
281700
</td>
<td style="text-align:right;">
281700
</td>
<td style="text-align:right;">
281700.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:left;">
South Shetland Islands\_Natal
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Natal
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1228620
</td>
<td style="text-align:right;">
1228620
</td>
<td style="text-align:right;">
1228620.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
165
</td>
<td style="text-align:left;">
South Shetland Islands\_North Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3101700
</td>
<td style="text-align:right;">
3101700
</td>
<td style="text-align:right;">
3101700.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
109
</td>
<td style="text-align:left;">
South Shetland Islands\_Rio Grande
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Rio Grande
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
South Shetland Islands\_Society Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Society Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:left;">
South Shetland Islands\_Southern China
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
140
</td>
<td style="text-align:left;">
South Shetland Islands\_St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3354120
</td>
<td style="text-align:right;">
3354120
</td>
<td style="text-align:right;">
3354120.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
158
</td>
<td style="text-align:left;">
South Shetland Islands\_Yellow Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Yellow Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4959780
</td>
<td style="text-align:right;">
4959780
</td>
<td style="text-align:right;">
4959780.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
130
</td>
<td style="text-align:right;">
122
</td>
<td style="text-align:left;">
Southeastern Brazil\_South Georgia
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Southern China\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Southern China
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5982420
</td>
<td style="text-align:right;">
5982420
</td>
<td style="text-align:right;">
5982420.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
166
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:left;">
Southern Norway\_Weddell Sea
</td>
<td style="text-align:left;">
Southern Norway
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
139
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Southwestern Caribbean\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
139
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Southwestern Caribbean\_South Orkney Islands
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
146
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf\_South Orkney Islands
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
792300
</td>
<td style="text-align:right;">
792300
</td>
<td style="text-align:right;">
792300.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Weddell Sea\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
261240
</td>
<td style="text-align:right;">
261240
</td>
<td style="text-align:right;">
261240.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Weddell Sea\_Azores Canaries Madeira
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2229180
</td>
<td style="text-align:right;">
2229180
</td>
<td style="text-align:right;">
2229180.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Weddell Sea\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
956940
</td>
<td style="text-align:right;">
956940
</td>
<td style="text-align:right;">
956940.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:left;">
Weddell Sea\_Rio de la Plata
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3465780
</td>
<td style="text-align:right;">
3465780
</td>
<td style="text-align:right;">
3465780.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
166
</td>
<td style="text-align:left;">
Weddell Sea\_Southern Norway
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Southern Norway
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
</tbody>
</table>

</div>

Now I extract some key summary numbers for the areas in summary
paragraph above.

``` r
primary_key_regions <- c("South Shetland Islands", "Antarctic Peninsula","Channels and Fjords of Southern Chile")
secondary_key_regions <- c("Malvinas/Falklands", "South Georgia", "South Shetland Islands", "Antarctic Peninsula")
ecoregion_networks$all %>%
     activate(edges) %>% 
     arrange(desc(n_voyages)) %>%
     filter(from_ecoregion %in% primary_key_regions & to_ecoregion %in%       primary_key_regions) %>%
    as_tibble() %>% 
    summarise(primary_key_regions_total = sum(n_voyages))
```

    ## # A tibble: 1 x 1
    ##   primary_key_regions_total
    ##                       <dbl>
    ## 1                      5344

``` r
ecoregion_networks$all %>%
     activate(edges) %>% 
     arrange(desc(n_voyages)) %>%
     filter(from_ecoregion %in% secondary_key_regions & to_ecoregion %in%       secondary_key_regions, 
            move != "South Shetland Islands_Antarctic Peninsula",
            move != "Antarctic Peninsula_South Shetland Islands"
            ) %>%
    as_tibble() %>% 
    summarise(secondary_key_regions_total = sum(n_voyages))
```

    ## # A tibble: 1 x 1
    ##   secondary_key_regions_total
    ##                         <dbl>
    ## 1                         798

The main pattern of important regions of activity is similar for fishing
activity.

``` r
gateway_region_edge_results$fishing
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
move
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_realm
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
internal
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South Shetland Islands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
285
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
7288500
</td>
<td style="text-align:right;">
14880
</td>
<td style="text-align:right;">
25573.68
</td>
<td style="text-align:right;">
285
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Shetland Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
283
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
15066780
</td>
<td style="text-align:right;">
12360
</td>
<td style="text-align:right;">
53239.51
</td>
<td style="text-align:right;">
283
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
South Shetland Islands\_South Orkney Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
1888020
</td>
<td style="text-align:right;">
27390
</td>
<td style="text-align:right;">
85819.09
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Shetland Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
4175280
</td>
<td style="text-align:right;">
255600
</td>
<td style="text-align:right;">
260955.00
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
South Orkney Islands\_South Shetland Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
783000
</td>
<td style="text-align:right;">
30480
</td>
<td style="text-align:right;">
46058.82
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Georgia\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
6264840
</td>
<td style="text-align:right;">
3132420
</td>
<td style="text-align:right;">
3132420.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Shetland Islands\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
4431480
</td>
<td style="text-align:right;">
186480
</td>
<td style="text-align:right;">
402861.82
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
South Shetland Islands\_Rio de la Plata
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
26440500
</td>
<td style="text-align:right;">
1056990
</td>
<td style="text-align:right;">
1888607.14
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
15043320
</td>
<td style="text-align:right;">
907980
</td>
<td style="text-align:right;">
1157178.46
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
36399240
</td>
<td style="text-align:right;">
1900260
</td>
<td style="text-align:right;">
2799941.54
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Shetland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2196960
</td>
<td style="text-align:right;">
220620
</td>
<td style="text-align:right;">
219696.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
South Shetland Islands\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
8869260
</td>
<td style="text-align:right;">
310560
</td>
<td style="text-align:right;">
682250.77
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Ross Sea\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
5282220
</td>
<td style="text-align:right;">
322110
</td>
<td style="text-align:right;">
440185.00
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Georgia
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
283800
</td>
<td style="text-align:right;">
283800
</td>
<td style="text-align:right;">
283800.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Namaqua\_South Shetland Islands
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
16478940
</td>
<td style="text-align:right;">
1879500
</td>
<td style="text-align:right;">
2059867.50
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
South Orkney Islands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2812260
</td>
<td style="text-align:right;">
86520
</td>
<td style="text-align:right;">
312473.33
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Peter the First Island
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1404600
</td>
<td style="text-align:right;">
36930
</td>
<td style="text-align:right;">
175575.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Ross Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
32882640
</td>
<td style="text-align:right;">
4845060
</td>
<td style="text-align:right;">
4697520.00
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
South Orkney Islands\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5799300
</td>
<td style="text-align:right;">
566460
</td>
<td style="text-align:right;">
828471.43
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2469240
</td>
<td style="text-align:right;">
949440
</td>
<td style="text-align:right;">
823080.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3168720
</td>
<td style="text-align:right;">
522540
</td>
<td style="text-align:right;">
528120.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Orkney Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1089720
</td>
<td style="text-align:right;">
155340
</td>
<td style="text-align:right;">
181620.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Orkney Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2213160
</td>
<td style="text-align:right;">
322590
</td>
<td style="text-align:right;">
368860.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Rio de la Plata\_South Shetland Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
12906060
</td>
<td style="text-align:right;">
690210
</td>
<td style="text-align:right;">
2151010.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
South Shetland Islands\_South Georgia
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Georgia
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
476880
</td>
<td style="text-align:right;">
476880
</td>
<td style="text-align:right;">
476880.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
East China Sea\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4872060
</td>
<td style="text-align:right;">
2436030
</td>
<td style="text-align:right;">
2436030.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Orkney Islands\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1228860
</td>
<td style="text-align:right;">
237900
</td>
<td style="text-align:right;">
245772.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_East China Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
15311460
</td>
<td style="text-align:right;">
3768090
</td>
<td style="text-align:right;">
3827865.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5326140
</td>
<td style="text-align:right;">
730380
</td>
<td style="text-align:right;">
1331535.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Central New Zealand\_Ross Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
16202700
</td>
<td style="text-align:right;">
4687560
</td>
<td style="text-align:right;">
4050675.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Central Peru\_South Shetland Islands
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4212840
</td>
<td style="text-align:right;">
1044480
</td>
<td style="text-align:right;">
1053210.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_East China Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7469820
</td>
<td style="text-align:right;">
994200
</td>
<td style="text-align:right;">
1867455.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Malvinas/Falklands\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
29112120
</td>
<td style="text-align:right;">
7039530
</td>
<td style="text-align:right;">
7278030.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Namaqua\_South Orkney Islands
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5047860
</td>
<td style="text-align:right;">
1069020
</td>
<td style="text-align:right;">
1682620.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Rio de la Plata\_South Orkney Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1748700
</td>
<td style="text-align:right;">
533970
</td>
<td style="text-align:right;">
437175.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Ross Sea\_Central New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4170720
</td>
<td style="text-align:right;">
1541040
</td>
<td style="text-align:right;">
1390240.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Ross Sea\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5415180
</td>
<td style="text-align:right;">
1265580
</td>
<td style="text-align:right;">
1353795.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
South Orkney Islands\_Rio de la Plata
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
12875400
</td>
<td style="text-align:right;">
1826370
</td>
<td style="text-align:right;">
3218850.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Araucanian\_South Shetland Islands
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1566540
</td>
<td style="text-align:right;">
783270
</td>
<td style="text-align:right;">
783270.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Central New Zealand\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3544980
</td>
<td style="text-align:right;">
3544980
</td>
<td style="text-align:right;">
3544980.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
436740
</td>
<td style="text-align:right;">
218370
</td>
<td style="text-align:right;">
218370.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
8623500
</td>
<td style="text-align:right;">
2278860
</td>
<td style="text-align:right;">
2874500.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Fiji Islands\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Fiji Islands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Tropical Southwestern Pacific
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5183340
</td>
<td style="text-align:right;">
5183340
</td>
<td style="text-align:right;">
5183340.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Fiji Islands\_Ross Sea
</td>
<td style="text-align:left;">
Fiji Islands
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Tropical Southwestern Pacific
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
11307480
</td>
<td style="text-align:right;">
5653740
</td>
<td style="text-align:right;">
5653740.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Peter the First Island\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1379880
</td>
<td style="text-align:right;">
462480
</td>
<td style="text-align:right;">
459960.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Rio de la Plata\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5645700
</td>
<td style="text-align:right;">
2822850
</td>
<td style="text-align:right;">
2822850.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
Ross Sea\_East China Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
10537860
</td>
<td style="text-align:right;">
3681840
</td>
<td style="text-align:right;">
3512620.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
206580
</td>
<td style="text-align:right;">
103290
</td>
<td style="text-align:right;">
103290.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Central New Zealand
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2619780
</td>
<td style="text-align:right;">
1309890
</td>
<td style="text-align:right;">
1309890.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5207040
</td>
<td style="text-align:right;">
2603520
</td>
<td style="text-align:right;">
2603520.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Peter the First Island
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
455160
</td>
<td style="text-align:right;">
227580
</td>
<td style="text-align:right;">
227580.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Georgia
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Azores Canaries Madeira\_South Orkney Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1814580
</td>
<td style="text-align:right;">
907290
</td>
<td style="text-align:right;">
907290.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Azores Canaries Madeira\_South Shetland Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4415220
</td>
<td style="text-align:right;">
2207610
</td>
<td style="text-align:right;">
2207610.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Baltic Sea\_Ross Sea
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
-9180
</td>
<td style="text-align:right;">
-9180
</td>
<td style="text-align:right;">
-9180.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Namaqua
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
5861820
</td>
<td style="text-align:right;">
2930910
</td>
<td style="text-align:right;">
2930910.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_East Antarctic Enderby Land
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
841200
</td>
<td style="text-align:right;">
420600
</td>
<td style="text-align:right;">
420600.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Mascarene Islands
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8799420
</td>
<td style="text-align:right;">
4399710
</td>
<td style="text-align:right;">
4399710.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
East China Sea\_South Shetland Islands
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4268940
</td>
<td style="text-align:right;">
4268940
</td>
<td style="text-align:right;">
4268940.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Malvinas/Falklands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
129540
</td>
<td style="text-align:right;">
129540
</td>
<td style="text-align:right;">
129540.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Mascarene Islands\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3392580
</td>
<td style="text-align:right;">
1696290
</td>
<td style="text-align:right;">
1696290.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
Namaqua\_South Georgia
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Peter the First Island\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
378480
</td>
<td style="text-align:right;">
189240
</td>
<td style="text-align:right;">
189240.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Peter the First Island\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2308980
</td>
<td style="text-align:right;">
1154490
</td>
<td style="text-align:right;">
1154490.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Rio de la Plata\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1233900
</td>
<td style="text-align:right;">
616950
</td>
<td style="text-align:right;">
616950.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Rio de la Plata\_Ross Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11404500
</td>
<td style="text-align:right;">
5702250
</td>
<td style="text-align:right;">
5702250.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
South Georgia\_Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
South Georgia\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
South Georgia\_Namaqua
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:left;">
South Georgia\_Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
South Orkney Islands\_Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
19553580
</td>
<td style="text-align:right;">
9776790
</td>
<td style="text-align:right;">
9776790.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
South Shetland Islands\_Namaqua
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2764800
</td>
<td style="text-align:right;">
1382400
</td>
<td style="text-align:right;">
1382400.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
562680
</td>
<td style="text-align:right;">
562680
</td>
<td style="text-align:right;">
562680.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Fiji Islands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Fiji Islands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Tropical Southwestern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1745640
</td>
<td style="text-align:right;">
1745640
</td>
<td style="text-align:right;">
1745640.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Rio de la Plata
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1740840
</td>
<td style="text-align:right;">
1740840
</td>
<td style="text-align:right;">
1740840.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Ross Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
10440
</td>
<td style="text-align:right;">
10440
</td>
<td style="text-align:right;">
10440.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_South Orkney Islands
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1524660
</td>
<td style="text-align:right;">
1524660
</td>
<td style="text-align:right;">
1524660.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Azores Canaries Madeira
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2566500
</td>
<td style="text-align:right;">
2566500
</td>
<td style="text-align:right;">
2566500.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:left;">
Antarctic Peninsula\_North Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8225400
</td>
<td style="text-align:right;">
8225400
</td>
<td style="text-align:right;">
8225400.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Southwestern Caribbean
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4680
</td>
<td style="text-align:right;">
4680
</td>
<td style="text-align:right;">
4680.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Central New Zealand\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
916860
</td>
<td style="text-align:right;">
916860
</td>
<td style="text-align:right;">
916860.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Central Peru\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1383780
</td>
<td style="text-align:right;">
1383780
</td>
<td style="text-align:right;">
1383780.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Sea of Japan/East Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1320
</td>
<td style="text-align:right;">
1320
</td>
<td style="text-align:right;">
1320.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1167120
</td>
<td style="text-align:right;">
1167120
</td>
<td style="text-align:right;">
1167120.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_Namaqua
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1048380
</td>
<td style="text-align:right;">
1048380
</td>
<td style="text-align:right;">
1048380.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Baltic Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
770820
</td>
<td style="text-align:right;">
770820
</td>
<td style="text-align:right;">
770820.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Bassian
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3798240
</td>
<td style="text-align:right;">
3798240
</td>
<td style="text-align:right;">
3798240.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Central New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
902160
</td>
<td style="text-align:right;">
902160
</td>
<td style="text-align:right;">
902160.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2100420
</td>
<td style="text-align:right;">
2100420
</td>
<td style="text-align:right;">
2100420.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_East China Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8295600
</td>
<td style="text-align:right;">
8295600
</td>
<td style="text-align:right;">
8295600.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Malvinas/Falklands
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1459740
</td>
<td style="text-align:right;">
1459740
</td>
<td style="text-align:right;">
1459740.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Namaqua
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
East China Sea\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2117640
</td>
<td style="text-align:right;">
2117640
</td>
<td style="text-align:right;">
2117640.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Malacca Strait\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1351440
</td>
<td style="text-align:right;">
1351440
</td>
<td style="text-align:right;">
1351440.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Malacca Strait\_South Orkney Islands
</td>
<td style="text-align:left;">
Malacca Strait
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Sunda Shelf
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Central Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Orkney Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
252240
</td>
<td style="text-align:right;">
252240
</td>
<td style="text-align:right;">
252240.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Namaqua\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Peter the First Island\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1623660
</td>
<td style="text-align:right;">
1623660
</td>
<td style="text-align:right;">
1623660.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
Peter the First Island\_Rio de la Plata
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
859440
</td>
<td style="text-align:right;">
859440
</td>
<td style="text-align:right;">
859440.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
Peter the First Island\_South Sandwich Islands
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
879180
</td>
<td style="text-align:right;">
879180
</td>
<td style="text-align:right;">
879180.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Peter the First Island\_South Shetland Islands
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
256980
</td>
<td style="text-align:right;">
256980
</td>
<td style="text-align:right;">
256980.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Rio de la Plata\_Peter the First Island
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1206540
</td>
<td style="text-align:right;">
1206540
</td>
<td style="text-align:right;">
1206540.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Ross Sea\_Baltic Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1380
</td>
<td style="text-align:right;">
1380
</td>
<td style="text-align:right;">
1380.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Ross Sea\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19920
</td>
<td style="text-align:right;">
19920
</td>
<td style="text-align:right;">
19920.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Ross Sea\_Peter the First Island
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
711000
</td>
<td style="text-align:right;">
711000
</td>
<td style="text-align:right;">
711000.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
Ross Sea\_Rio de la Plata
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1562760
</td>
<td style="text-align:right;">
1562760
</td>
<td style="text-align:right;">
1562760.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
Sahelian Upwelling\_South Shetland Islands
</td>
<td style="text-align:left;">
Sahelian Upwelling
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1273200
</td>
<td style="text-align:right;">
1273200
</td>
<td style="text-align:right;">
1273200.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Sea of Japan/East Sea\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Sea of Japan/East Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Cold Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
South Georgia\_Araucanian
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
South Georgia\_Central Peru
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Central Peru
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
South Georgia\_South Orkney Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
688860
</td>
<td style="text-align:right;">
688860
</td>
<td style="text-align:right;">
688860.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:left;">
South Georgia\_South Sandwich Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
South Orkney Islands\_Araucanian
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7227420
</td>
<td style="text-align:right;">
7227420
</td>
<td style="text-align:right;">
7227420.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:left;">
South Orkney Islands\_South Georgia
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
South Orkney Islands\_Southwestern Caribbean
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
10500
</td>
<td style="text-align:right;">
10500
</td>
<td style="text-align:right;">
10500.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Sandwich Islands\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5605380
</td>
<td style="text-align:right;">
5605380
</td>
<td style="text-align:right;">
5605380.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:left;">
South Sandwich Islands\_Weddell Sea
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
185460
</td>
<td style="text-align:right;">
185460
</td>
<td style="text-align:right;">
185460.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
South Shetland Islands\_Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1916520
</td>
<td style="text-align:right;">
1916520
</td>
<td style="text-align:right;">
1916520.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
South Shetland Islands\_East China Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
East China Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Northwest Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
South Shetland Islands\_Mascarene Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2496840
</td>
<td style="text-align:right;">
2496840
</td>
<td style="text-align:right;">
2496840.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Southwestern Caribbean\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Southwestern Caribbean\_South Orkney Islands
</td>
<td style="text-align:left;">
Southwestern Caribbean
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Tropical Northwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf\_South Orkney Islands
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
792300
</td>
<td style="text-align:right;">
792300
</td>
<td style="text-align:right;">
792300.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
Weddell Sea\_Rio de la Plata
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3465780
</td>
<td style="text-align:right;">
3465780
</td>
<td style="text-align:right;">
3465780.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
</tbody>
</table>

</div>

Tourism is even more centred on the area formed between southern SOuth
America, Malvinas/Falklands, Scotia Arc and the Antarctic Peninsula. The
connection with the highest number of voyages that is not within that
are is from New Zealand to the Ross Sea, but only 14 tourist voyages to
that route to Antarctica versus the 568 that travelled from the Chanels
and Fjords of Southern Chile to the South Shetland Islands.

``` r
gateway_region_edge_results$tourism
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
move
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_realm
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
internal
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
South Shetland Islands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1032
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
32681220
</td>
<td style="text-align:right;">
16620
</td>
<td style="text-align:right;">
31667.85
</td>
<td style="text-align:right;">
1032
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Shetland Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1004
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
29615700
</td>
<td style="text-align:right;">
14670
</td>
<td style="text-align:right;">
29497.71
</td>
<td style="text-align:right;">
1004
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Shetland Islands\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
638
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
65243880
</td>
<td style="text-align:right;">
164160
</td>
<td style="text-align:right;">
175859.51
</td>
<td style="text-align:right;">
371
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Shetland Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
568
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
56
</td>
<td style="text-align:right;">
38632440
</td>
<td style="text-align:right;">
105720
</td>
<td style="text-align:right;">
112303.60
</td>
<td style="text-align:right;">
344
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
317
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
68001660
</td>
<td style="text-align:right;">
261900
</td>
<td style="text-align:right;">
272006.64
</td>
<td style="text-align:right;">
250
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
268
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
13892040
</td>
<td style="text-align:right;">
41580
</td>
<td style="text-align:right;">
73502.86
</td>
<td style="text-align:right;">
189
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Georgia
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
191
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
13674420
</td>
<td style="text-align:right;">
345420
</td>
<td style="text-align:right;">
369578.92
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
South Georgia\_South Shetland Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
7755360
</td>
<td style="text-align:right;">
237870
</td>
<td style="text-align:right;">
228098.82
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:left;">
South Georgia\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
5450760
</td>
<td style="text-align:right;">
558780
</td>
<td style="text-align:right;">
495523.64
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:left;">
South Shetland Islands\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
18364620
</td>
<td style="text-align:right;">
364740
</td>
<td style="text-align:right;">
374788.16
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Shetland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
20165940
</td>
<td style="text-align:right;">
653160
</td>
<td style="text-align:right;">
517075.38
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
South Orkney Islands\_South Shetland Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
2753580
</td>
<td style="text-align:right;">
27060
</td>
<td style="text-align:right;">
58586.81
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
South Georgia\_Antarctic Peninsula
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
3259620
</td>
<td style="text-align:right;">
173400
</td>
<td style="text-align:right;">
217308.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
120
</td>
<td style="text-align:left;">
South Georgia\_South Orkney Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
1616820
</td>
<td style="text-align:right;">
101100
</td>
<td style="text-align:right;">
202102.50
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
South Shetland Islands\_South Georgia
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
2227440
</td>
<td style="text-align:right;">
295890
</td>
<td style="text-align:right;">
371240.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Georgia
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
4807620
</td>
<td style="text-align:right;">
478080
</td>
<td style="text-align:right;">
480762.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
3106320
</td>
<td style="text-align:right;">
202800
</td>
<td style="text-align:right;">
282392.73
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
130
</td>
<td style="text-align:right;">
105
</td>
<td style="text-align:left;">
South New Zealand\_Ross Sea
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
8266140
</td>
<td style="text-align:right;">
679320
</td>
<td style="text-align:right;">
635856.92
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
120
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Orkney Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6590400
</td>
<td style="text-align:right;">
583200
</td>
<td style="text-align:right;">
506953.85
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Malvinas/Falklands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1057920
</td>
<td style="text-align:right;">
38280
</td>
<td style="text-align:right;">
151131.43
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
105
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Ross Sea\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
524400
</td>
<td style="text-align:right;">
23070
</td>
<td style="text-align:right;">
52440.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:left;">
South Georgia\_Namaqua
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3527760
</td>
<td style="text-align:right;">
1763880
</td>
<td style="text-align:right;">
1763880.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Georgia
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2219580
</td>
<td style="text-align:right;">
423960
</td>
<td style="text-align:right;">
443916.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
105
</td>
<td style="text-align:right;">
130
</td>
<td style="text-align:left;">
Ross Sea\_South New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7677420
</td>
<td style="text-align:right;">
750060
</td>
<td style="text-align:right;">
853046.67
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Georgia\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
472860
</td>
<td style="text-align:right;">
472860
</td>
<td style="text-align:right;">
472860.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
103
</td>
<td style="text-align:left;">
South Georgia\_Rio de la Plata
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1378440
</td>
<td style="text-align:right;">
398040
</td>
<td style="text-align:right;">
459480.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:left;">
South Shetland Islands\_Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4886880
</td>
<td style="text-align:right;">
517380
</td>
<td style="text-align:right;">
542986.67
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
130
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_South New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5532240
</td>
<td style="text-align:right;">
674850
</td>
<td style="text-align:right;">
691530.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
103
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
Rio de la Plata\_South Georgia
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2491320
</td>
<td style="text-align:right;">
453360
</td>
<td style="text-align:right;">
622830.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
105
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Ross Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
687000
</td>
<td style="text-align:right;">
37860
</td>
<td style="text-align:right;">
98142.86
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
103
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
Rio de la Plata\_South Shetland Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2989980
</td>
<td style="text-align:right;">
403560
</td>
<td style="text-align:right;">
427140.00
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
South Orkney Islands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
810360
</td>
<td style="text-align:right;">
108540
</td>
<td style="text-align:right;">
115765.71
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
103
</td>
<td style="text-align:left;">
South Shetland Islands\_Rio de la Plata
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3637920
</td>
<td style="text-align:right;">
435780
</td>
<td style="text-align:right;">
519702.86
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
South Orkney Islands\_South Georgia
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
562260
</td>
<td style="text-align:right;">
281130
</td>
<td style="text-align:right;">
281130.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
130
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
South New Zealand\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
South New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3445800
</td>
<td style="text-align:right;">
660030
</td>
<td style="text-align:right;">
861450.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
138
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf\_South Shetland Islands
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
973980
</td>
<td style="text-align:right;">
451200
</td>
<td style="text-align:right;">
324660.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Central New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3435600
</td>
<td style="text-align:right;">
819180
</td>
<td style="text-align:right;">
858900.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
105
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
Ross Sea\_Central New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4613400
</td>
<td style="text-align:right;">
894750
</td>
<td style="text-align:right;">
1153350.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
120
</td>
<td style="text-align:left;">
South Shetland Islands\_South Orkney Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
106200
</td>
<td style="text-align:right;">
25950
</td>
<td style="text-align:right;">
26550.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:left;">
Antarctic Peninsula\_North Patagonian Gulfs
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
219240
</td>
<td style="text-align:right;">
219240
</td>
<td style="text-align:right;">
219240.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
124
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Southeastern Brazil
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3710400
</td>
<td style="text-align:right;">
1639440
</td>
<td style="text-align:right;">
1236800.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Bassian\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
881220
</td>
<td style="text-align:right;">
440610
</td>
<td style="text-align:right;">
440610.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
120
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Orkney Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1472460
</td>
<td style="text-align:right;">
736230
</td>
<td style="text-align:right;">
736230.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
85
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
North Patagonian Gulfs\_South Shetland Islands
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:left;">
South Shetland Islands\_North Patagonian Gulfs
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
281460
</td>
<td style="text-align:right;">
281460
</td>
<td style="text-align:right;">
281460.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
101
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Peter the First Island
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
37020
</td>
<td style="text-align:right;">
18510
</td>
<td style="text-align:right;">
18510.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
101
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Peter the First Island
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
205260
</td>
<td style="text-align:right;">
102630
</td>
<td style="text-align:right;">
102630.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
103
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Rio de la Plata
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1086000
</td>
<td style="text-align:right;">
543000
</td>
<td style="text-align:right;">
543000.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
120
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Orkney Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
522540
</td>
<td style="text-align:right;">
261270
</td>
<td style="text-align:right;">
261270.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
85
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
North Patagonian Gulfs\_South Georgia
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
815640
</td>
<td style="text-align:right;">
815640
</td>
<td style="text-align:right;">
815640.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
101
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Peter the First Island\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
216840
</td>
<td style="text-align:right;">
108420
</td>
<td style="text-align:right;">
108420.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
101
</td>
<td style="text-align:right;">
105
</td>
<td style="text-align:left;">
Peter the First Island\_Ross Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1078200
</td>
<td style="text-align:right;">
539100
</td>
<td style="text-align:right;">
539100.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
103
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Rio de la Plata\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
713400
</td>
<td style="text-align:right;">
356700
</td>
<td style="text-align:right;">
356700.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
105
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Ross Sea\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
469620
</td>
<td style="text-align:right;">
234810
</td>
<td style="text-align:right;">
234810.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
South Georgia\_Cape Verde
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Cape Verde
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
857400
</td>
<td style="text-align:right;">
857400
</td>
<td style="text-align:right;">
857400.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
South Orkney Islands\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1245240
</td>
<td style="text-align:right;">
622620
</td>
<td style="text-align:right;">
622620.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
105
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Ross Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
41880
</td>
<td style="text-align:right;">
41880
</td>
<td style="text-align:right;">
41880.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Chiloense
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Chiloense
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
156
</td>
<td style="text-align:left;">
Antarctic Peninsula\_North Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2820
</td>
<td style="text-align:right;">
2820
</td>
<td style="text-align:right;">
2820.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
105
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Ross Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
746760
</td>
<td style="text-align:right;">
746760
</td>
<td style="text-align:right;">
746760.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
523080
</td>
<td style="text-align:right;">
523080
</td>
<td style="text-align:right;">
523080.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
121
</td>
<td style="text-align:left;">
Araucanian\_South Shetland Islands
</td>
<td style="text-align:left;">
Araucanian
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Warm Temperate Southeastern Pacific
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
101
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Peter the First Island
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
45180
</td>
<td style="text-align:right;">
45180
</td>
<td style="text-align:right;">
45180.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
156
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
North Sea\_Antarctic Peninsula
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
101
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Peter the First Island\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
60120
</td>
<td style="text-align:right;">
60120
</td>
<td style="text-align:right;">
60120.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:left;">
South Georgia\_North Patagonian Gulfs
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
North Patagonian Gulfs
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:left;">
South Georgia\_St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1268400
</td>
<td style="text-align:right;">
1268400
</td>
<td style="text-align:right;">
1268400.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:left;">
South Georgia\_Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Uruguay-Buenos Aires Shelf
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
156
</td>
<td style="text-align:left;">
South Orkney Islands\_North Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3545340
</td>
<td style="text-align:right;">
3545340
</td>
<td style="text-align:right;">
3545340.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
South Shetland Islands\_Baltic Sea
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Baltic Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3781020
</td>
<td style="text-align:right;">
3781020
</td>
<td style="text-align:right;">
3781020.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
South Shetland Islands\_Cape Verde
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Cape Verde
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
West African Transition
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
112
</td>
<td style="text-align:left;">
South Shetland Islands\_Society Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Society Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southeast Polynesia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Eastern Indo-Pacific
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
124
</td>
<td style="text-align:left;">
South Shetland Islands\_Southeastern Brazil
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
121
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:left;">
South Shetland Islands\_St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
St. Helena and Ascension Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Tropical Atlantic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3354120
</td>
<td style="text-align:right;">
3354120
</td>
<td style="text-align:right;">
3354120.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
124
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:left;">
Southeastern Brazil\_South Georgia
</td>
<td style="text-align:left;">
Southeastern Brazil
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
</tbody>
</table>

</div>

Although still heavily centered around the Antarctic Peninsula/South
Shetland Islands/South AMerica/Falklands/Scotia Arc area, research
activity is a little more evenly spread. The connections between Bassian
and East Antarctic Wilkes Land in both directions are rank 5th and 6th.

``` r
gateway_region_edge_results$research
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped table-hover table-condensed" style="font-size: 11px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
move
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_ecoregion
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_province
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
from\_realm
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
to\_realm
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_voyages
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_ships
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_trips
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
total\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
median\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
mean\_time
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
n\_time
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">
internal
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:left;">
Antarctic Peninsula\_South Shetland Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
208
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
5248800
</td>
<td style="text-align:right;">
13260
</td>
<td style="text-align:right;">
25356.52
</td>
<td style="text-align:right;">
207
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
South Shetland Islands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
200
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
4814160
</td>
<td style="text-align:right;">
12030
</td>
<td style="text-align:right;">
24070.80
</td>
<td style="text-align:right;">
200
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
South Shetland Islands\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
130
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
35565540
</td>
<td style="text-align:right;">
251940
</td>
<td style="text-align:right;">
320410.27
</td>
<td style="text-align:right;">
111
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Shetland Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
118
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
23279700
</td>
<td style="text-align:right;">
206730
</td>
<td style="text-align:right;">
223843.27
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Bassian
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
33707640
</td>
<td style="text-align:right;">
698040
</td>
<td style="text-align:right;">
783898.60
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
Bassian\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
21073740
</td>
<td style="text-align:right;">
491760
</td>
<td style="text-align:right;">
554572.11
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3827760
</td>
<td style="text-align:right;">
178140
</td>
<td style="text-align:right;">
173989.09
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
11860860
</td>
<td style="text-align:right;">
415800
</td>
<td style="text-align:right;">
515689.57
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:left;">
South Georgia\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2178900
</td>
<td style="text-align:right;">
406080
</td>
<td style="text-align:right;">
544725.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Ross Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
8061360
</td>
<td style="text-align:right;">
126480
</td>
<td style="text-align:right;">
537424.00
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
Ross Sea\_Central New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
15999840
</td>
<td style="text-align:right;">
852090
</td>
<td style="text-align:right;">
1142845.71
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Shetland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4427160
</td>
<td style="text-align:right;">
251280
</td>
<td style="text-align:right;">
442716.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
Namaqua\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
8904900
</td>
<td style="text-align:right;">
569400
</td>
<td style="text-align:right;">
684992.31
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
Ross Sea\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4572840
</td>
<td style="text-align:right;">
201660
</td>
<td style="text-align:right;">
351756.92
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:left;">
South Orkney Islands\_South Shetland Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
2302920
</td>
<td style="text-align:right;">
24240
</td>
<td style="text-align:right;">
177147.69
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:left;">
Namaqua\_Weddell Sea
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8596560
</td>
<td style="text-align:right;">
776730
</td>
<td style="text-align:right;">
859656.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
Central New Zealand\_Ross Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2335260
</td>
<td style="text-align:right;">
514980
</td>
<td style="text-align:right;">
467052.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Georgia
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2767080
</td>
<td style="text-align:right;">
997860
</td>
<td style="text-align:right;">
922360.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:left;">
South Orkney Islands\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3482760
</td>
<td style="text-align:right;">
251760
</td>
<td style="text-align:right;">
497537.14
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_East Antarctic Enderby Land
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1230120
</td>
<td style="text-align:right;">
16290
</td>
<td style="text-align:right;">
123012.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:left;">
Malvinas/Falklands\_South Orkney Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2054400
</td>
<td style="text-align:right;">
224940
</td>
<td style="text-align:right;">
293485.71
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:left;">
South Shetland Islands\_South Orkney Islands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
421980
</td>
<td style="text-align:right;">
22170
</td>
<td style="text-align:right;">
42198.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
104
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:left;">
Weddell Sea\_Namaqua
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
9320040
</td>
<td style="text-align:right;">
848880
</td>
<td style="text-align:right;">
932004.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Weddell Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
875400
</td>
<td style="text-align:right;">
20580
</td>
<td style="text-align:right;">
97266.67
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1090200
</td>
<td style="text-align:right;">
98400
</td>
<td style="text-align:right;">
121133.33
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
Leeuwin\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5887680
</td>
<td style="text-align:right;">
744000
</td>
<td style="text-align:right;">
654186.67
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:left;">
South Shetland Islands\_Malvinas/Falklands
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2922840
</td>
<td style="text-align:right;">
213300
</td>
<td style="text-align:right;">
417548.57
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:left;">
South Shetland Islands\_Rio de la Plata
</td>
<td style="text-align:left;">
South Shetland Islands
</td>
<td style="text-align:left;">
Rio de la Plata
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Warm Temperate Southwestern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4810080
</td>
<td style="text-align:right;">
714720
</td>
<td style="text-align:right;">
801680.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
252000
</td>
<td style="text-align:right;">
19020
</td>
<td style="text-align:right;">
36000.00
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_South Orkney Islands
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3342900
</td>
<td style="text-align:right;">
365010
</td>
<td style="text-align:right;">
557150.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_East Antarctic Enderby Land
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
204660
</td>
<td style="text-align:right;">
22380
</td>
<td style="text-align:right;">
29237.14
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Leeuwin
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
8698080
</td>
<td style="text-align:right;">
1114920
</td>
<td style="text-align:right;">
1242582.86
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
104
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:left;">
Weddell Sea\_South Orkney Islands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2901720
</td>
<td style="text-align:right;">
283920
</td>
<td style="text-align:right;">
414531.43
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
254040
</td>
<td style="text-align:right;">
27540
</td>
<td style="text-align:right;">
42340.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Peter the First Island
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
510480
</td>
<td style="text-align:right;">
50820
</td>
<td style="text-align:right;">
102096.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
Bassian\_Ross Sea
</td>
<td style="text-align:left;">
Bassian
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southeast Australian Shelf
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3443160
</td>
<td style="text-align:right;">
470640
</td>
<td style="text-align:right;">
688632.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
Central New Zealand\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1488660
</td>
<td style="text-align:right;">
547080
</td>
<td style="text-align:right;">
496220.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Central New Zealand
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5132760
</td>
<td style="text-align:right;">
1022040
</td>
<td style="text-align:right;">
1026552.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Malvinas/Falklands\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
268920
</td>
<td style="text-align:right;">
82080
</td>
<td style="text-align:right;">
89640.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Peter the First Island\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
238920
</td>
<td style="text-align:right;">
11460
</td>
<td style="text-align:right;">
47784.00
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:left;">
South Orkney Islands\_South Georgia
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
232200
</td>
<td style="text-align:right;">
232200
</td>
<td style="text-align:right;">
232200.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Namaqua
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3290820
</td>
<td style="text-align:right;">
559200
</td>
<td style="text-align:right;">
822705.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
81660
</td>
<td style="text-align:right;">
18450
</td>
<td style="text-align:right;">
20415.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:left;">
East Antarctic Enderby Land\_Namaqua
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3509520
</td>
<td style="text-align:right;">
862110
</td>
<td style="text-align:right;">
877380.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land\_Namaqua
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4194120
</td>
<td style="text-align:right;">
1026930
</td>
<td style="text-align:right;">
1048530.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
Leeuwin\_Ross Sea
</td>
<td style="text-align:left;">
Leeuwin
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Southwest Australian Shelf
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
6280560
</td>
<td style="text-align:right;">
1023600
</td>
<td style="text-align:right;">
1570140.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
104
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
Weddell Sea\_East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1067220
</td>
<td style="text-align:right;">
10800
</td>
<td style="text-align:right;">
266805.00
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
104
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:left;">
Weddell Sea\_South Georgia
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
676500
</td>
<td style="text-align:right;">
676500
</td>
<td style="text-align:right;">
676500.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Central New Zealand
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4406700
</td>
<td style="text-align:right;">
1364700
</td>
<td style="text-align:right;">
1468900.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea\_Ross Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
262560
</td>
<td style="text-align:right;">
110520
</td>
<td style="text-align:right;">
87520.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
244500
</td>
<td style="text-align:right;">
244500
</td>
<td style="text-align:right;">
244500.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
113
</td>
<td style="text-align:left;">
Antarctic Peninsula\_North Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
61380
</td>
<td style="text-align:right;">
19740
</td>
<td style="text-align:right;">
20460.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Central New Zealand\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Central New Zealand
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Southern New Zealand
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
921060
</td>
<td style="text-align:right;">
460530
</td>
<td style="text-align:right;">
460530.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2112420
</td>
<td style="text-align:right;">
602820
</td>
<td style="text-align:right;">
704140.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile\_Peter the First Island
</td>
<td style="text-align:left;">
Channels and Fjords of Southern Chile
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1134300
</td>
<td style="text-align:right;">
405720
</td>
<td style="text-align:right;">
378100.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
Manning-Hawkesbury\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Manning-Hawkesbury
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East Central Australian Shelf
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Australasia
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1200900
</td>
<td style="text-align:right;">
406860
</td>
<td style="text-align:right;">
400300.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
Mascarene Islands\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Mascarene Islands
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Western Indian Ocean
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Western Indo-Pacific
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3165480
</td>
<td style="text-align:right;">
1582740
</td>
<td style="text-align:right;">
1582740.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Namaqua\_East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Namaqua
</td>
<td style="text-align:left;">
East Antarctic Enderby Land
</td>
<td style="text-align:left;">
Benguela
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Temperate Southern Africa
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1446900
</td>
<td style="text-align:right;">
557220
</td>
<td style="text-align:right;">
482300.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
113
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
North Sea\_Antarctic Peninsula
</td>
<td style="text-align:left;">
North Sea
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Northern European Seas
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
34080
</td>
<td style="text-align:right;">
17040
</td>
<td style="text-align:right;">
17040.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Peter the First Island\_Antarctic Peninsula
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
286200
</td>
<td style="text-align:right;">
101340
</td>
<td style="text-align:right;">
95400.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Ross Sea\_Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Amundsen/Bellingshausen Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2358660
</td>
<td style="text-align:right;">
28200
</td>
<td style="text-align:right;">
786220.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:left;">
Ross Sea\_Peter the First Island
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1697820
</td>
<td style="text-align:right;">
535980
</td>
<td style="text-align:right;">
565940.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:left;">
South Orkney Islands\_Weddell Sea
</td>
<td style="text-align:left;">
South Orkney Islands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
866280
</td>
<td style="text-align:right;">
307860
</td>
<td style="text-align:right;">
288760.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
90
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:left;">
South Sandwich Islands\_Weddell Sea
</td>
<td style="text-align:left;">
South Sandwich Islands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
499860
</td>
<td style="text-align:right;">
168960
</td>
<td style="text-align:right;">
166620.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
104
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:left;">
Weddell Sea\_Malvinas/Falklands
</td>
<td style="text-align:left;">
Weddell Sea
</td>
<td style="text-align:left;">
Malvinas/Falklands
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Magellanic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Temperate South America
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2441700
</td>
<td style="text-align:right;">
761220
</td>
<td style="text-align:right;">
813900.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:left;">
Antarctic Peninsula\_Peter the First Island
</td>
<td style="text-align:left;">
Antarctic Peninsula
</td>
<td style="text-align:left;">
Peter the First Island
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Subantarctic Islands
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
176100
</td>
<td style="text-align:right;">
88050
</td>
<td style="text-align:right;">
88050.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:left;">
Azores Canaries Madeira\_South Georgia
</td>
<td style="text-align:left;">
Azores Canaries Madeira
</td>
<td style="text-align:left;">
South Georgia
</td>
<td style="text-align:left;">
Lusitanian
</td>
<td style="text-align:left;">
Scotia Sea
</td>
<td style="text-align:left;">
Temperate Northern Atlantic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2719080
</td>
<td style="text-align:right;">
2719080
</td>
<td style="text-align:right;">
2719080.00
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
To or from Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
East Antarctic Wilkes Land
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1653360
</td>
<td style="text-align:right;">
826680
</td>
<td style="text-align:right;">
826680.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Within Antarctica
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land\_Ross Sea
</td>
<td style="text-align:left;">
East Antarctic Dronning Maud Land
</td>
<td style="text-align:left;">
Ross Sea
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Continental High Antarctic
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:left;">
Southern Ocean
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
2
</td>