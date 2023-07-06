# pnas_ship_traffic_antarctica
The code used to analyze ship traffic data and generate figures for the publication: https://www.pnas.org/doi/full/10.1073/pnas.2110303118 

Repository for the ship pathway and network analysis, published in PNAS as "Ship traffic connects Antarctic coasts to worldwide ecosystems" and included as chapter 3 of my PhD thesis.
## Summary

The aim of the project was to generate a network of ship activity for vessels that visited the Southern Ocean between 2014-2018 inclusive to quantify the global range and intensity of Antarctic ship traffic. Networks were generated for ports and ecoregions (ecologically similar marine areas identified in the Marine Ecoregions of the World) for all ships and for each activity type (fishing, tourism, research, supply, other). I reveal the extent of the global ship network connected to Antarctica and identify key ports and global ecoregions that are strongly connected to Antarctica, representing likely source regions for non-native species. I also test assumptions that activity is higher in certain Antarctic locations and that 'Antarctic Gateway' ports are the primary conduits through which ships travel to the Southern Ocean.

The best way to follow the workflow is read the annotated code and reports in the `workflow` folder. The input data, output data, figures and objects are all contained in the following directories:

- `data` contains all the data files imported to conduct QC and all subsequent csv data files used for scripts further down the workflow
- `scripts` contains all scripts (.RmD or .R) used in this project.
- `scripts\functions` contains all the functions created and used in scripts in this project
- `workflow` contains the .md (and .html) files created when knitting the scripts. This is the best place to follow the work.
- `outputs` contains intermediate figure and preliminary data outputs
- `figures` contains publication-ready figures from final analysis

There are currently 6 main scripts associated with this analysis, all found in the 'scripts' folder:

1. 1-InformaDataWrangling.Rmd brings togehter all the data provided in its many different formats and generates 'master' files for later QC and cleaning stages
2. 2-CleaningStages3and4.Rmd performs stages 3 and 4 of data QC. Stages of QC are outlined in the first file. 
3. 3-Antarctic-Locations.Rmd performs the task of creating port equivalents around coastal Antarctica that can act as nodes in the network of ship activity.
4. 4-ExtractingBasicInfo.Rmd creates basic figures and calculations that describe the data before the network is created, and compare the observations used here to other known sources of ship information. 
4. 5-Networks.Rmd generates edge lists and node lists for port and ecoregion networks for all ships and by activity type. This script also calculates some attributes, such as time spent at or between ports. This file calls to a number of functions contained in the 'scripts/functions' folder.
6. 6-NetworkAnalyses.Rmd generates the networks, analyses them and generates summary information and plots used to draw primary conclusions. This file calls to a number of functions contained in the 'scripts/functions' folder.
7. 7-ManuscriptOutputs.Rmd This file contains the scripts for producing publication-ready figures. This file calls to a number of functions contained in the 'scripts/functions' folder. 

## Data

The story of the data purchased from Informa Lloyd's List Intelligence is convoluted and included a number of re-issued files and extra data supplied in slightly different formats. The data had many false positives observations of ships in the Southern Ocean and required a length QC process. Four sets of data were supplied at different times, and extracted by Informa using slightly different methods (summarised satellite Automatic Identification System location (AIS) fixes vs all satellite AIS location fixes, with vs without times in UTC), but the underlying principle is the same: Informa obtained the satellite AIS reading for all ships south of 60 degrees South from 2014-2018 inclusive, then provided a file with information about each of ships and another set of data with their worldwide port call information for the same period. This document describes the process of bringing the two sets of data together.

### Data files and sources

Between June 2019 and June 2020, I had numerous back and forth emails with Informa with questions relating to the data, how it was extracted and the information contained within it. Because of that, I have received a number of versions of the data, which have been used at different stages of the process. In this section I outline what data were received, when they were received and what they contain. All files are kept in the 'InformaDataRaw' subdirectory of the 'data' folder. The rest of the data files are stored locally (and backed up), with the directory structure outlined below, but due to the licence agreement the raw data cannot be shared and is not included. All data files are stored and saved in directories as described, but only data outputs from analyses are included in the GitHub repository. Data outputs, where shareable, are in the `cleaned_data` directory.

Filename                                                        | Date created | Directory                                               | Original zip file                     | Description
----------------------------------------------------------------|--------------|---------------------------------------------------------|---------------------------------------|-------------------
MOVEMENTS.csv                                                   | 2019-06-17   | ~data/InformaDataRaw/ LLID3547                          | LLID3547.zip                          | FIRST file containing the worldwide port calls for all ships south of 60S 2024-2018, ships detected from summarised AIS data. Times in local timezone, no qualifiers for system generated date/times or sequence id for port calls.  
SIGHTINGS.csv                                                   | 2019-06-17   | ~data/InformaDataRaw/ LLID3547                          | LLIDS3547.zip                         | FIRST file containing the AIS observations for all ships south of 60S 2024-2018,  ships detected from summarised AIS data.  Times in UTC. No system-generated date/times.
VESSELS.csv                                                     | 2019-06-17   | ~data/InformaDataRaw/ LLID3547                          | LLIDS3547.zip                         | FIRST file containing ship information for all ships south of 60S 2024-2018,  ships detected from summarised AIS data
Description.xlsx                                                | 2019-06-17   | ~data/InformaDataRaw                                    | NA                                    | Excel spreadsheet with definitions and descriptions of the FIRST data provided
LLI_Vessel_positions_recorded_ South_60_latitude_2014-2018.xlsx | 2019-08-23   | ~data/InformaDataRaw                                    | NA                                    | Excel spreadsheet with monthly observation and vessel information for vessels with observations south of 60S from unsummarised AIS data, worldwide port calls for those ships, latitude and longitude coordinates for the ports. This was provided when I asked about how the observations were collected and generated. 
LLI_AIS_Positions_S60_ 2014_2016.csv                            | 2019-09-06   | ~data/InformaDataRaw/ LLI_AIS_Positions_&_Port_Callings | LLI_AIS_Positions_&_Port_Callings.zip | SECOND lot of data. File containing unsummarised AIS observations from 2014 to 2016, for the new list of ships I generated from the spreadsheet created on 2019-08-23. The new list had the verified ships from the August spreadsheet that were not already in the first data files provided. Timezone is UTC. No system-generated times.
LLI_AIS_Positions_S60_ 2017_2018.csv                            | 2019-09-06   | ~data/InformaDataRaw/ LLI_AIS_Positions_&_Port_Callings | LLI_AIS_Positions_&_Port_Callings.zip | SECOND lot of data. As above, but for observations from 2017-18. Timezone is UTC. No system-generated times.
LLI_Port_Callings_2014.csv                                      | 2019-09-06   | ~data/InformaDataRaw/ LLI_AIS_Positions_&_Port_Callings | LLI_AIS_Positions_&_Port_Callings.zip | SECOND lot of data. Worlwide port calls from 2014 for the ships uncovered in the 'new' list, details as above. Timezone is local.
LLI_Port_Callings_2015.csv                                      | 2019-09-06   | ~data/InformaDataRaw/ LLI_AIS_Positions_&_Port_Callings | LLI_AIS_Positions_&_Port_Callings.zip | SECOND lot of data. Worlwide port calls from 2015 for the ships uncovered in the 'new' list, details as above. Timezone is local.
LLI_Port_Callings_2016.csv                                      | 2019-09-06   | ~data/InformaDataRaw/ LLI_AIS_Positions_&_Port_Callings | LLI_AIS_Positions_&_Port_Callings.zip | SECOND lot of data. Worlwide port calls from 2016 for the ships uncovered in the 'new' list, details as above. Timezone is local.
LLI_Port_Callings_2017.csv                                      | 2019-09-06   | ~data/InformaDataRaw/ LLI_AIS_Positions_&_Port_Callings | LLI_AIS_Positions_&_Port_Callings.zip | SECOND lot of data. Worlwide port calls from 2017 for the ships uncovered in the 'new' list, details as above. Timezone is local.
LLI_Port_Callings_2018.csv                                      | 2019-09-06   | ~data/InformaDataRaw/ LLI_AIS_Positions_&_Port_Callings | LLI_AIS_Positions_&_Port_Callings.zip | SECOND lot of data. Worlwide port calls from 2018 for the ships uncovered in the 'new' list, details as above. Timezone is local.
MOVEMENTS.csv                                                   | 2020-02-10   | ~data/InformaDataRaw/ LLID3547_MOVEMENTS                | LLIDS3547_MOVEMENTS.zip               | THIRD lot of data. Worldwide port calls equivalent to the FIRST MOVEMENTS file, but with all times in UTC and date qualifiers included if the date/time for the arrival date or sailing date was generated automatically. Some slight changes expected in data due to improved accuracy since data first issued.
VESSELS.csv                                                     | 2020-02-10   | ~data/InformaDataRaw/ LLID3547_MOVEMENTS                | LLIDS3547_MOVEMENTS.zip               | THIRD lot of data. Vessel information list equivalent to the original VESSELS list but associated with the updated MOVEMENTS file. 
DECODE_COUNTRY_FLAG.csv                                         | 2020-02-07   | ~data/InformaDataRaw                                    | NA                                    | Information for THIRD lot of data to explain the qualifiers in MOVEMENTS file relating to the country flag variable.
DECODE_DATE_QUALIFIER.csv                                       | 2020-02-07   | ~data/InformaDataRaw                                    | NA                                    | Information for THIRD lot of data to explain the qualifiers in MOVEMENTS file relating to the system generated arrival and departure dates.
LLI_Port_Callings_2014_2018.csv                                 | 2020-02-21   | ~data/InformaDataRaw                                    | NA                                    | FOURTH lot of data. Worldwide port calls equivalent to SECOND lot of data but now issued in UTC instead of local time. Arrival/departure qualifiers already in the data. 
MOVEMENTS_ID_SEQ.csv                                            | 2020-05-18   | ~data/InformaDataRaw/ 2020-05-18-LLID3547_MOVEMENTS     | 2020-05-18-LLID3547_MOVEMENTS.zip     | File containing the worldwide port calls for all ships south of 60S 2024-2018 from FIFTH issue of data but equivalent to FIRST set of movements provided with sightings and vessels. Ships detected from summarised AIS data. Times in UTC, qualifiers for system generated date/times, sequence ids for ordering port calls.  
VESSELS.csv                                                    | 2020-05-18   | ~data/InformaDataRaw/ 2020-05-18-LLIDS3547_MOVEMENTS    | 2020-05-18-LLID3547_MOVEMENTS.zip     | FIFTH lot of data. Vessel information list equivalent to the original VESSELS list but associated with the updated MOVEMENTS_ID_SEQ file.

Since the FIRST and SECOND sets of data issued were used to verify the ships present, they are still used in the first script of data wrangling and QC. However, the THIRD and FOURTH sets of data on port calls are used instead of the port call data from the first and second sets. The presence of system-generated times in the port-call data creates some complications, but these are dealt with at various stages of analysis and explained. For calculations related to time, all entries with system-generated date/times needed to be removed, but for non-time calculations and network edges all port calls were included.

Only the files actualy needed to run the code are uploaded to the GitHub repository. The rest are stored locally (and backed up).

### QC and Data cleaning

The data needed to be cleaned to ensure that the numerous false positives were removed. This was a 5 stage process with the following criteria (stages 1-3 were conducted together, manually in excel):

  1. If all the ship's Southern Ocean sightings were on land the ship was removed from subsequent analyses.  
  2. If the ship's port calls clearly conflicted with its Southern Ocean sightings it was removed. As the LLI port calls are verified by people, it was assumed that port call data was more likely to be accurate than satellite AIS data.    
  3. If the ship was a wholly unsuitable vessel it was removed. If there was any doubt, the ship was not removed. Small tugs and inland barges were considered unsuitable. They typically also had other factors that made them likely false positives, i.e. few, disconnected Southern Ocean sightings and unlikely (if not impossible) port calls.
  4. All duplicated readings (from both Southern Ocean sightings and port call movements) were removed along with any missing a date/time fix or satellite sightings that overlapped with land.
  5. A speedfilter [@trip1, @trip2] was applied that removed all points that would require a ship to travel faster than 60km/hr (maximum theoretical speed plus 10% for the fastest vessel in these data, based on selection of 15% of vessels, from all vessel and activity types).
  6. After each stage of filtering, all ships that no longer had any AIS sightings in the Southern Ocean were removed.


Cleaning stage | Total number of ships | Total number of observations   
---------------|-----------------------|-----------------------------  
Raw data       | 1040 (865 + 175)      | 1 056 755  
after 1, 2, 3  | 265                   | 373 153  
4              | 264                   | 372 359  
5              | 255                   | 326 470  


In addition to cleaning the ship data, the information and location information on each port provided by Informa also needed cleaning. Some ports were missing coordinates. These were manually located (see later section for details), but only for ports visited by the 265 ships remaining after stages 1-3 of cleaning. 

During stages 1-3, each ship was allocated an ACTIVITY_TYPE based on whether it is active in tourism, fishing, national operations, supply or other. Stages 4 and 5 of cleaning are found in the script 2-CleaningStages3and4.rmd. 

Any time manual cleaning was performed or information was added manually (i.e. not in R) a new file was created in the format outputdataframename_verified. This is so that when code is run again and an output dataframe saved, it doesn't replace the one that has the information manually added. 

### Data analysis

All steps after cleaning were conducted in R and detailed in the relevant scripts that consolidate the data, generate and analyse the networks, and produce figures and outputs. 