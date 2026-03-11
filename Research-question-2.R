# ---
# Title:  "natural hazards linked to river restoration projects"
# Date:   "2026-02-22"
# Author: "Julie Mettenbrink"
# For writing the code and solving errors partly luhki (LLM) was used
# ---

# Table of content

### Setting working environment
### 1. Introduction
### 2. Prepare data
## 2.1 Prepare RESTORE data
# 2.1.1. Adding data
## 2.2. Preparing Natural hazards data
## 2.3. Prepare GDP data
## 2.4. Prepare Population data
## 2.5. Filter other data
## 2.6. Join data
### 3. Analysing data
## 3.1 Creating new comprised tables
## 3.2. Creating graphs
# 3.2.1. Amount of natural hazards by floods per country and year
# 3.2.2. Losses by natural hazards for every country per year
# 3.2.3. Count of RESTORE projects per year and country
# 3.2.4. Combined: RESTORE projects and Natural hazards
# 3.2.4.1. Amount of natural hazards and started projects
# 3.2.4.2. Amount of natural hazards and completed projects
# 3.2.4.3. Economic losses and Amount of Restore projects
# 3.2.5. Average loss as share of GDP per country (relates to RQ1)

#################################################################################

# Setting working environment ---------------------------------------------

# Set working directory
setwd("C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data")

# Clear environment
rm(list = ls())

# Install / load libraries
# install.packages(c("readxl", "writexl", "dplyr", "tidyr", "ggplot2", "matrixStats", "paletteer", "stringi",
# "tidyverse", "knitr", "gridExtra", "conflicted", "gt", "webshot2", "plotly", "shiny"))
library(conflicted) # dealing with conflicts
library(readxl)   # Read excel files
library(writexl)  # Save excel files
library(dplyr)    # Data management: https://nyu-cdsc.github.io/learningr/assets/data-transformation.pdf
library(tidyr)    # Data cleaning: https://rstudio.github.io/cheatsheets/tidyr.pdf
library(ggplot2)  # Graphs: https://posit.co/wp-content/uploads/2022/10/data-visualization-1.pdf
library(matrixStats)
library(paletteer) # Color palettes
library(stringi) # to add data manually to RESTORE from website restorerivers.eu
library(knitr) # to plot simple tables
library(webshot2) # also needed to plot tables
library(gridExtra) # to convert table into graphic
library(tidyverse)
library(gt) # to format tables
library(webshot2) # to save tables formated with gt
library(plotly) # for interactive graphics
library(shiny) # also for interactive graphics

#################################################################################


# 1. Introduction ---------------------------------------------------------

## Aim and research question

# This script seeks to answer the second research question of the project:
# To what extent could climate-related economic losses caused by hydrological events relate
# to the implementation of river restoration measures in  European countries?

## Data description

# In this script following data were used:

# 1. Natural Hazards by floods (https://doi.org/10.5281/zenodo.17226475)
#    -> data from 1870 to 2025
#    -> time frequency annual
#    -> floods caused by sea, of relatively long duration by larger river systems, during a short duration along small rivers, from both sea and river
#    -> available for NUTS3 regions in 42 european countries (including eu-27-member-states), but
#       Canary Islands, Ceuta, and Melilla (parts of Spain) are excluded
#    -> requirements for the floods to get included in the dataset:
#       - at least 1000 ha (10 km2) inundated
#       - at least one person killed or missing, presumed dead
#       - at least 50 households or 200 people affected by their homes being inundated or which were evacuated
#       - losses in monetary terms corresponding to at least EUR 1 million in 2020 prices and exchange rates
#    -> unit/definition of data in columns:
#       - Type (Coastal, River, Flash)
#         - "coastal floods (inundation by sea),
#         – riverine floods (inundation of relatively long duration by larger river systems),
#         – flash floods (encompassing only floods of short duration along small rivers)
#         - compound floods (co-occurrence in time and space of inundation from both sea and rivers)" (Paprotny et al. 2024: 5146f)
#       - Region2024 = European NUTS 3 codes (https://ec.europa.eu/eurostat/documents/345175/629341/NUTS2021-NUTS2024.xlsx/2b35915f-9c14-6841-8197-353408c4522d?t=1717505289640)
#       - Area: Area inundated (km2)
#       - Fatalities: Persons killed, including persons missing and presumed dead
#       - Persons: Number of people evacuated or whose houses were flooded
#       - LossesNom: Direct losses to assets in monetary terms in the currency and prices of the year and location of the flood event
#       - Currency: Linked to LossesNom. Three-letter code of currency in which nominal losses are recorded
#       - LossesEuro: Direct losses to assets in monetary terms converted to euro in 2020 prices using the gross domestic product deflator

# 2. RESTORE projects (https://doi.org/10.17632/jkpjt6kkc6.1)
#    -> data from 2000 to 2024 (table is published in 2024, but data are not obviously up to this year)
#    -> missing projects:
#         - Creating a fishpass at Włocławek dam
#   -> added year manually, using the original source-website (https://www.restorerivers.eu/wiki/index.php?title=Special:RunQuery/Case_study_query_comprehensive&wpRunQuery=true&Case_study_query_comprehensive%5BStatus%5D=Complete%2C%20In%20progress&Case_study_query_comprehensive%5BUnit%5D=km&Case_study_query_comprehensive%5BProject%20started%20from%5D=2000-01-01&Case_study_query_comprehensive%5BWorks%20started%20from%5D=2000-01-01&Case_study_query_comprehensive%5BAny%20biological%20quality%20element%20monitored%5D=No&Case_study_query_comprehensive%5BAny%20hydromorphological%20quality%20element%20monitored%5D=No&Case_study_query_comprehensive%5BAny%20physico-chemical%20quality%20element%20monitored%5D=No&Case_study_query_comprehensive%5BAny%20other%20response%20monitored%5D=No&Case_study_query_comprehensive%5BResult%20type%5D=Table&_run=)
#   -> start year: Project started ; completing year: Works completed or project completed
#   -> available for Austria, Belgium, Bulgaria, Denmark, England, Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, Netherlands, Northern Ireland, Norway, Poland, Portugal, Romania, Scotland, Slovenia, Spain, Sweden, Wales
#   -> unit/definition of data in columns:
#         - Area: Area (km2)
#         - Project_status: Works completed or project completed = completed
#         - Themes (Economic, Environmental_flows, Fisheries, Flood_risk, Habitat_and_biodiversity, Hydropower, Land_use_agri, Land_use_forestry, Monitoring, Social, Spatial_planning, Water_quality, Urban planning)
#           - Economic aspects
#           - Environmental flows and water resources
#           - Fisheries
#           - Flood risk management
#           - Habitat and biodiversity
#           - Hydromorphology
#           - Hydropower
#           - Land use management - agriculture
#           - Land use management - forestry
#           - Monitoring
#           - Social benefits
#           - Spatial planning
#           - Urban
#           - Water quality
#         - Land use (in square meter)
#         - Costs (€)
#         - area (km2)
#         - river lenght affected (m)

# 3. GDP - Gross domestic product (https://data.worldbank.org/indicator/NY.GDP.MKTP.CD)
# -> originally in 2015 USD constant prices, but changed in 2020 million Euro constant prices
# -> timespan: 1960-2024
# -> time frequency annual
# -> "Gross domestic product is the total income earned through the production of goods and
#    services in an economic territory during an accounting period. It can be measured in three
#    different ways: using either the expenditure approach, the income approach, or the production approach.
#    This indicator is expressed in current prices, meaning no adjustment has been made to account for price
#    changes over time. This indicator is expressed in United States dollars."
# -> 266 regions / countries (including Netherlands, Poland, Spain)
#     -> Cleaning, selecting timespan and european countries & Changing constant prices to 2020 Euro was done in the R-Script for RQ 1

# 4. Population (https://doi.org/10.2908/DEMO_GIND)
#     -> timespan: 1990-2024
#     -> time frequency annual
#     -> Population for the 01. January of the year
#     -> latest update: 14/10/2024 23:00
#     -> european countries and some additional

# 5. Other data:
#     -> River length (https://www.naturalearthdata.com/downloads/50m-physical-vectors/50m-rivers-lake-centerlines/)
#         - Edited in ArcGIS pro: Intersected with EU27 countries and summarized per country
#     -> Country size (https://ec.europa.eu/eurostat/web/gisco/geodata/statistical-units/territorial-units-statistics?utm: adapted in the first script)
#     -> Sealed area (https://ec.europa.eu/eurostat/databrowser/view/lan_lcv_art/default/table: adapted in the first script)

# Load and clean data and save data as tables:

NHazards <- read_excel(
  "C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data/Natural hazards/NaturalHazards.xls"
) # 1. Dataset
RESTORE <- read_excel(
  "C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data/Natural hazards/Restore.xls"
) # 2. Dataset
Restoreriverseu <- read.csv("C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data/Natural hazards/result.csv") # 2. selected data from website
GDP <- read_excel("C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data/GDP.xlsx") # 3. Dataset
Population <- read_excel(
  "C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data/demographic_data.xlsx",
  sheet = 3
) # 4. Dataset
rivers <- read_excel("C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data/Rivers.xls")
country <- read_excel("C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data/Land size.xlsx")
sealed <- read_excel("C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data/Built up area.xlsx")


#################################################################################

# 2. Prepare data ---------------------------------------------------------

# Selecting countries

selected_countries <- c("Netherlands", "Poland", "Spain")

# Selecting years

selected_years <- 1990:2024


# 2.1. Prepare RESTORE data -----------------------------------------------

# Splitting values in column country, because sometimes are more than one country in one row
RESTORE_solved <- RESTORE %>%
  separate_rows(Country, sep = ",\\s*")


# Select the case study countries and remove data for other countries from the RESTORE datasets
restore_nps <- RESTORE %>%
  dplyr::filter(Country %in% selected_countries)

### Merging latest data (Restoreriverseu) with older data (RESTORE)

# separate the themes of the restoration project in the csv to have a column for every theme (1= theme relevant, 0 = theme not relevant)

Restorerivers_eu <- Restoreriverseu %>%
  separate_rows(Themes, sep = ",") %>%
  mutate(Value = 1) %>%
  spread(key = Themes, value = Value, fill = 0)

# Rename rows and recode letters

Restorerivers_eu <- Restorerivers_eu %>%
  rename(
    Project_name = Project.title,
    Project_status = Project.status,
    Economic = 'Economic aspects',
    Environmental_flows = 'Environmental flows and water resources',
    Flood_risk = 'Flood risk management',
    Habitat_and_biodiversity = 'Habitat and biodiversity',
    Land_use_agri = 'Land use management - agriculture',
    Land_use_forestry = 'Land use management - forestry',
    Social = 'Social benefits',
    Spatial_planning = 'Spatial planning',
    Urban_planning = Urban,
    Water_quality = 'Water quality'
  ) # rename columns

Restorerivers_eu$Project_name <- stri_trans_general(Restorerivers_eu$Project_name, "Latin-ASCII")
restore_nps$Project_name <- stri_trans_general(restore_nps$Project_name, "Latin-ASCII") # recode columns


# select data from restore_nps, that are also in Restorerivers_eu (already filtered downloaded from website)
restore_nps_new <- restore_nps %>%
  semi_join(Restorerivers_eu, by = "Project_name")


# some data are missing in the RESTORE-Dataset, so we need to identify the missing projects and add it to the new dataset
missing_projects <- Restorerivers_eu %>%
  anti_join(restore_nps_new, by = "Project_name") # identify missing data

# select relevant columns for the join
relevantcolumns_missing_projects <- missing_projects %>%
  select(
    Project_name,
    Country,
    Project_status,
    Economic,
    Environmental_flows,
    Fisheries,
    Flood_risk,
    Habitat_and_biodiversity,
    Hydromorphology,
    Hydropower,
    Land_use_agri,
    Land_use_forestry,
    Monitoring,
    Social,
    Spatial_planning,
    Urban_planning,
    Water_quality
  )

# merge tables to have the current version of restore projects
restore_nps_new <- restore_nps_new %>%
  bind_rows(relevantcolumns_missing_projects)


# 2.1.1. Adding data ------------------------------------------------------

### add additional data from the restorerivers.eu website
# (starting year, year completed, project costs, area size, river length affected, river basin)
# adding columns to restore_nps_new


restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = NA_integer_,
    year_completed = NA_integer_,
    costs = NA_real_,
    area_size = NA_real_,
    length_affected = NA_real_,
    river_basin = NA_character_
  )

### (prepare) adding values
## setting values (names of projects)

project_1 <- "\"Rio Verde II\". Restoration of riparian forest and the rivers Llobregat and Cardener"
project_2 <- "Aaijen- Removal of bank fixation"
project_3 <- "Actions for environmental regeneration and flow control in Odra's Basin, Burgos"
project_4 <- "Aesthetic vs. functional restoration of urban and peri-urban rivers: the Manzanares River in Madrid"
project_5 <- "Amerongse Bovenpolder"
project_6 <- "Bakenhof Dyke reconstruction"
project_7 <- "Basic proposals for restoration and enhancement of the stream of Vilamajor in Sant Antoni de Vilamajor"
project_8 <- "Bastion Maria (QUERCUS)"
project_9 <- "Beek Lage Raam"
project_10 <- "Beekherstel Beekloop BmN"
project_11 <- "Beekherstel Buurserbeek"
project_12 <- "Beekherstel Eefsebeek"
project_13 <- "Beekherstel Keersop"
project_14 <- "Beekherstel Lactariabeek"
project_15 <- "Beekherstel Reusel Baarschot-Diessen"
project_16 <- "Beekherstel Willinkbeek"
project_17 <- "Beekherstel Zuidelijk Afwateringskanaal"
project_18 <- "Beneden-Berkel"
project_19 <- "Beneden-Leeuwen-Side channel"
project_20 <- "Biodiversity conservation and recovery in the river basin Ason"
project_21 <- "Bird reserve \"El Clot\" (Tarragona)"
project_22 <- "Bird reserve \"Finca San Miguel\" (Huesca)"
project_23 <- "Bird reserve \"Las Marismas Blancas\" (Cantabria)"
project_24 <- "Bird reserve \"O Grove\" (Pontevedra)"
project_25 <- "Bird reserve \"Riet Vell\" (Tarragona)"
project_26 <- "Bird Reserve \"Tancat de la Pipa\" (Valencia)"
project_27 <- "Buiten Ooij"
project_28 <- "Conservation the public water domain in the Tajo basin, t. m. Alcocer, province of Guadalajara."
project_29 <- "Control and permeabilization of the marsh to the river, the Brazo de la Torre and Entremuros"
project_30 <- "Design and Application of a Sustainable Soil Management Model for Orchard Crops in the Donana National Park Area"
project_31 <- "Ecological restoration of The Arboleda and the Meander of Santes Creus. Aiguamurcia municipality (Alt Camp). Gaia River Basin"
project_32 <- "Environmental improvement intervention in a reach Anzur River in the village of El Nacimiento, in the Municipality of Rute (Cordoba)"
project_33 <- "Environmental improvement project of the stream of Ruby"
project_34 <- "Environmental regeneration project in Magro river bed"
project_35 <- "Environmental Restoration of Olivar-El Zaudin Metropolitan Park in Tomares"
project_36 <- "Environmental Restoration of the Lower Section of the Bembezar River and its River Environment (Phase 1)"
project_37 <- "Environmental restoration of the lower section of the river Narcea in the municipalities of Salas and Pravia (Asturias)"
project_38 <- "Environmental Restoration of the Riopudio Stream in Sevilla"
project_39 <- "Environmental restoration of the river Zujar"
project_40 <- "Environmental restoration project of the Guadajoz River (Castro del Rio, Andalusia)"
project_41 <- "Fluvial improvement in Riera de Caldes"
project_42 <- "Fluvial longitudinal continuity recovery in river Aguanaz, Cantabria, Spain"
project_43 <- "Fluvial restoration in River Carrion"
project_44 <- "Fluvial river restoration of Tordera in Sant Celoni"
project_45 <- "Guadiamar Green Corridor Project"
project_46 <- "Guadiato Life project Model for restoring and integrating water resources in a mining area, actions for an alternative development"
project_47 <- "Herinrichting beekdal Tongelreep"
project_48 <- "Herinrichting Beekloop"
project_49 <- "Hierdense Beek"
project_50 <- "Improved longitudinal continuity of the Lozoya River, upstream the Pinilla dam (Madrid)"
project_51 <- "Improvement of ecological state of the river Pisuerga between the dam of Aguilar de Campo and Alar del Rey (Palencia) - 1st Stage."
project_52 <- "Improvement of the Terri River in the industrial zone (Phase I and II)"
project_53 <- "Improving the ecological status of the Black River and tributaries (Zamora)"
project_54 <- "Increased biodiversity, river Llobregat, Sallent."
project_55 <- "International recovery of the river Minho: an example of sustainable hydraulic exploitation"
project_56 <- "INTERREG MED WETNET- Albufera de Valencia Memorandu of Cooperation for Wetland Contract"
project_57 <- "INTERREG MED WETNET - Management Plan for the Canizar Lagoon (ARAGON)"
project_58 <- "Creating a fishpass at Wloclawek dam"
project_59 <- "INTERREG MED WETNET - Odiel Marshes Wetland Contract and Action plan"
project_60 <- "Inturia dam removal"
project_61 <- "Keent"
project_62 <- "Klassiek beekherstel Astense Aa"
project_63 <- "Kleine Aa, traject Smalwater Noord"
project_64 <- "La Gotera dam removal"
project_65 <- "Environment-friendly Reduction of Flood Risk in the Multi-thread Reach of the Czarny Dunajec River"
project_66 <- "The Upper Raba River Spawning Grounds"
project_67 <- "LIFE Segura Riverlink (Through rivers)"
project_68 <- "Llobregat river restoration"
project_69 <- "Lunterse Beek"
project_70 <- "Millingerwaard-Floodplain rehabilitation"
project_71 <- "Ontpoldering Noordwaard"
project_72 <- "Opijnen- Side channel"
project_73 <- "Proposal for landscape and environmental improvement of the canal of Sils"
project_74 <- "Recovery \"CanoTravieso\""
project_75 <- "Recovery of the functionality of Brazo de la Torre"
project_76 <- "Regeneration of the banks of the river Mape"
project_77 <- "Restoration of a section of Torrente de Can Sunyer passing through Martorelles"
project_78 <- "Restoration of a stream of the Jarama River, Valdetorres de Jarama"
project_79 <- "Restoration of a stretch of the river of Santa Coloma"
project_80 <- "Restoring connectivity of the ecological corridor in the Biala River valley"
project_81 <- "Restoration of riparian zone in Aragon River"
project_82 <- "Restoration of streams Chico Soto, Soto Grande and Arroyo de la Laguna de los Reyes"
project_83 <- "Restoration of the marsh \"Gallega\""
project_84 <- "Restoration of the river area of the section of Betzuca stream"
project_85 <- "Restoration of the stream of the Partido"
project_86 <- "Restoration of the stream of Vallicrosa in the section of the source of Pic"
project_87 <- "Restoration of Viejas River from the outlet of the fish farm to its mouth"
project_88 <- "Restoration project in Verde River"
project_89 <- "Restoration project of the degraded area of the Moli source environment and improvement of river areas and adjacent riparian formations"
project_90 <- "Restoration project of the river area of the streams of Vallmanya and Reixac"
project_91 <- "Rfff"
project_92 <- "Riparia-Ter - Recovery of riparian habitats of the Ter River"
project_93 <- "River restoration in the irrigation area Lobon (Badajoz)"
project_94 <- "River restoration project in Congost between the torrent of Malhivern and end of the municipality of La Garriga"
project_95 <- "River restoration project in Sella, Asturias"
project_96 <- "RIVER RESTORATION PROJECT OF THE OLD SEGURA RIVER BED IN THE VIVILLO MEANDER (MURCIA)"
project_97 <- "River restoration project of the stream of Vallvidrera"
project_98 <- "Robledo de Chavela dam removal"
project_99 <- "Room for the River"
project_100 <- "Route water: environmental restoration project for tourist use"
project_101 <- "SCI Parga-Ladra-Tamoga: recovery of bog woodland and dystrophic lake"
project_102 <- "Strengthening associated biodiversity of habitat 92A0 and control of Invasive Alien Species in the Segura River (LIFE + Ripisilvanatura)"
project_103 <- "Sustainable management, at local level, of the alluvial aquifer of the River Tordera, through the reuse of waste water"
project_104 <- "Shaping and protecting water-mud biotopes in Garwolin Forest division through development of small retention"
project_105 <- "The Manzanares River Restoration: Demolition of an obsolete dam and riverine ecosystem rehabilitation"
project_106 <- "The San Marcos weir removal"
project_107 <- "Traditioneel Beekherstel Koffiegoot"
project_108 <- "Tungelroyse beek"
project_109 <- "Updated project for the \"Environmental Restoration and Urban and Landscape Planning of Cabra River's High Section (Cordoba)"
project_110 <- "VALAGUA (Valorizacao ambiental e gestao integrada da agua e dos habitats no Baixo Guadiana transfronteirico)"
project_111 <- "Vallacuera Ravine. Removal of a dyke"
project_112 <- "Vreugderijkerwaard side channel"
project_113 <- "Weir demolition in river Nansa, Cantabria"
project_114 <- "Works of improvement in final stretch of the Riera of Vallcorba, Sant Quirze del Valles"
project_115 <- "Zujar river restoration in the reach between the Chapel of St Mary of Zujar and the ford of \"Entrerrios\", Badajoz."
project_116 <- "Zujar river restoration in the reach between the dam and the ford of \"El Espolon\" , Badajoz."
project_117 <- "Zujar river restoration in the reach between the ford of \"El Espolon\" and the Chapel of St Mary of Zujar , Badajoz."
project_118 <- "Zujar river restoration in the reach between the ford of \"Entrerrios\" and its mouth, Badajoz."



##  prepare adding coordinates
# Code to be able to convert coordinates into longitude and latitude

convert_to_decimal <- function(coord) {
  tryCatch({
    coord_clean <- gsub(" ", "", coord)
    parts <- unlist(strsplit(coord_clean, "[°'\" ]")) # delete gaps and split coordinate into its parts (north, south, east, west)
    if (length(parts) < 3) {
      stop("Error: Could not split the coordinate into three parts. Check the format.")
    }
    degrees <- as.numeric(parts[1]) # extract degrees
    minutes <- as.numeric(parts[2]) # extract minutes
    seconds <- as.numeric(parts[3]) # extract seconds
    if (is.na(degrees) |
        is.na(minutes) |
        is.na(seconds)) {
      stop("Error: Unable to convert degrees, minutes, or seconds to numeric.")
    } # make sure, that there can be FALSE
    direction <- substr(coord_clean, nchar(coord_clean), nchar(coord_clean)) # extract cardinal point (N, S, E, W)
    if (!direction %in% c("N", "S", "E", "W")) {
      stop("Error: Direction is not one of N, S, E, W.")
    } # extract cardinal point (N, S, E, W)
    decimal <- degrees + minutes / 60 + seconds / 3600 # calculate decimal value
    if (direction == "S" ||
        direction == "W") {
      decimal <- -decimal
    } # correction of direction for south and west
    return(decimal)
  }, error = function(e) {
    print(paste("Encountered an error:", e$message))
    return(NA) # returns NA, if something went wrong
  })
}

# connect coordinates with projects
project_coords <- data.frame(
  Project_name = c(
    project_1,
    project_3,
    project_20,
    project_21,
    project_22,
    project_23,
    project_24,
    project_25,
    project_26,
    project_28,
    project_29,
    project_30,
    project_31,
    project_36,
    project_39,
    project_40,
    project_41,
    project_43,
    project_55,
    project_56,
    project_57,
    project_58,
    project_59,
    project_65,
    project_66,
    project_73,
    project_74,
    project_80,
    project_82,
    project_83,
    project_85,
    project_88,
    project_89,
    project_91,
    project_93,
    project_96,
    project_99,
    project_101,
    project_104,
    project_109,
    project_110,
    project_113,
    project_114,
    project_115,
    project_116,
    project_117,
    project_118
  ),
  Latitude = c(
    "41° 47' 10.87\" N",
    "42° 16' 2.19\" N",
    "43° 21' 10.03\" N",
    "40° 41' 1.37\" N",
    "41° 42' 37.43\" N",
    "43° 24' 36.39\" N",
    "42° 27' 12.23\" N",
    "40° 40' 8.55\" N",
    "39° 19' 49.55\" N",
    "40° 26' 44.07\" N",
    "36° 53' 34.08\" N",
    "37° 15' 31.51\" N",
    "41° 20' 43.45\" N",
    "37° 47' 26.31\" N",
    "38° 59' 18.87\" N",
    "37° 41' 10.80\" N",
    "41° 34' 17.07\" N",
    "42° 39' 56.34\" N",
    "42° 9' 15.96\" N",
    "39° 20' 19.70\" N",
    "40° 46' 10.49\" N",
    "52° 37' 4.00\" N",
    "49° 26' 9.82\" N",
    "41° 31' 29.76\" N",
    "51° 55' 9.98\" N",
    "41° 48' 17.16\" N",
    "49° 37' 31.64\" N",
    "37° 2' 1.69\" N",
    "37° 6' 4.79\" N",
    "37° 6' 32.39\" N",
    "37° 7' 58.13\" N",
    "39° 9' 13.05\" N",
    "42° 11' 57.05\" N",
    "53° 1' 19.85\" N",
    "38° 52' 8.82\" N",
    "37° 58' 49.55\" N",
    "52° 22' 31.80\" N",
    "43° 13' 17.61\" N",
    "51° 53' 49.92\" N",
    "37° 27' 58.78\" N",
    "51° 55' 9.98\" N",
    "43° 19' 35.75\" N",
    "41° 31' 38.90\" N",
    "38° 59' 35.90\" N",
    "38° 54' 44.67\" N",
    "38° 57' 16.57\" N",
    "39° 0' 17.68\" N"
  ),
  Longitude = c(
    "1° 54' 11.25\" E",
    "4° 10' 51.54\" W",
    "3° 25' 59.53\" W",
    "0° 42' 12.57\" E",
    "0° 19' 40.02\" E",
    "3° 49' 16.38\" W",
    "8° 52' 31.47\" W",
    "0° 46' 15.38\" E",
    "0° 21' 55.51\" W",
    "2° 36' 19.10\" W ",
    "6° 20' 29.45\" W",
    "6° 20' 50.46\" W",
    "1° 21' 34.41\" E",
    "5° 13' 15.98\" W",
    "5° 43' 43.44\" W",
    "4° 29' 13.40\" W",
    "2° 10' 29.82\" E",
    "4° 49' 22.17\" W",
    "8° 11' 42.02\" W",
    "0° 20' 41.12\" W",
    "3° 4' 1.24\" W",
    "19° 24' 28.00\" E",
    "19° 51' 31.35\" E",
    "2° 14' 21.48\" E",
    "19° 8' 42.49\" E",
    "2° 44' 39.89\" E",
    "20° 56' 55.82\" E",
    "6° 18' 7.32\" W",
    " 6° 27' 16.70\" W",
    "6° 22' 46.03\" W",
    "6° 27' 52.54\" W",
    "0° 28' 45.40\" W",
    "2° 42' 3.00\" E",
    "5° 3' 18.88\" E",
    "6° 39' 5.53\" W",
    "1° 10' 25.61\" W",
    "5° 37' 30.00\" E",
    "7° 39' 59.00\" W",
    "21° 36' 53.78\" E",
    "4° 31' 47.16\" W",
    "19° 8' 42.49\" E",
    "4° 29' 10.86\" W",
    "2° 5' 5.22\" E",
    "5° 41' 21.12\" W",
    "5° 30' 26.07\" W",
    "5° 35' 37.53\" W",
    "5° 46' 2.92\" W"
  ),
  stringsAsFactors = FALSE
)

# convert new project coordinates into decimal format, to have the same as in the original data
project_coords <- project_coords %>%
  mutate(
    Latitude = sapply(Latitude, convert_to_decimal),
    Longitude = sapply(Longitude, convert_to_decimal)
  )

# select only rows for later join, that do not have any values for longitude and latitude
filtered_project_coords <- project_coords %>%
  select(Project_name, Latitude, Longitude)

# join original restore-table with the newly added projects with the coordinates
restore_nps_new <- restore_nps_new %>%
  left_join(project_coords, by = "Project_name", suffix = c("", "_new")) %>% # name temporally new columns
  mutate(
    Latitude = ifelse(is.na(Latitude), Latitude_new, Latitude),
    Longitude = ifelse(is.na(Longitude), Longitude_new, Longitude)
  ) %>% # join Latitude and Longitude just if they are NA in the restore_nps_new table
  select(-Latitude_new, -Longitude_new) # delete temporally new columns



## add other values manually to all of the 118 projects

# Project 1

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_1, 2006, starting_year),
    year_completed = ifelse(Project_name == project_1, 2007, year_completed),
    costs = ifelse(Project_name == project_1, 497000, costs),
    area_size = ifelse(Project_name == project_1, NA, area_size),
    length_affected = ifelse(Project_name == project_1, NA, length_affected),
    river_basin = ifelse(Project_name == project_1, "El Llobregat", river_basin),
  )

# Project 2

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_2, 2005, starting_year),
    year_completed = ifelse(Project_name == project_2, 2006, year_completed),
    costs = ifelse(Project_name == project_2, 74000, costs),
    area_size = ifelse(Project_name == project_2, 465, area_size),
    length_affected = ifelse(Project_name == project_2, 0.4, length_affected),
    river_basin = ifelse(Project_name == project_2, "Meuse", river_basin)
  )

# Project 3

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_3, 2009, starting_year),
    year_completed = ifelse(Project_name == project_3, 2009, year_completed),
    costs = ifelse(Project_name == project_3, 4382000, costs),
    area_size = ifelse(Project_name == project_3, 798, area_size),
    length_affected = ifelse(Project_name == project_3, 8, length_affected),
    river_basin = ifelse(Project_name == project_3, "Odra", river_basin)
  )

# Project 4

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_4, 2017, starting_year),
    year_completed = ifelse(Project_name == project_4, NA, year_completed),
    costs = ifelse(Project_name == project_4, 2000000, costs),
    area_size = ifelse(Project_name == project_4, NA, area_size),
    length_affected = ifelse(Project_name == project_4, NA, length_affected),
    river_basin = ifelse(Project_name == project_4, "Manzanares", river_basin)
  )

# Project 5

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_5, 2001, starting_year),
    year_completed = ifelse(Project_name == project_5, 2015, year_completed),
    costs = ifelse(Project_name == project_5, NA, costs),
    area_size = ifelse(Project_name == project_5, 1000, area_size),
    length_affected = ifelse(Project_name == project_5, 1.4, length_affected),
    river_basin = ifelse(Project_name == project_5, "Rhine", river_basin)
  )

# Project 6

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_6, 2001, starting_year),
    year_completed = ifelse(Project_name == project_6, 2004, year_completed),
    costs = ifelse(Project_name == project_6, NA, costs),
    area_size = ifelse(Project_name == project_6, 1000, area_size),
    length_affected = ifelse(Project_name == project_6, 1.5, length_affected),
    river_basin = ifelse(Project_name == project_6, "Rhine", river_basin)
  )

# Project 7

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_7, 2010, starting_year),
    year_completed = ifelse(Project_name == project_7, 2010, year_completed),
    costs = ifelse(Project_name == project_7, 20000, costs),
    area_size = ifelse(Project_name == project_7, NA, area_size),
    length_affected = ifelse(Project_name == project_7, 0.082, length_affected),
    river_basin = ifelse(Project_name == project_7, " 	Besós", river_basin)
  )

# Project 8

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_8, 2007, starting_year),
    year_completed = ifelse(Project_name == project_8, 2010, year_completed),
    costs = ifelse(Project_name == project_8, NA, costs),
    area_size = ifelse(Project_name == project_8, NA, area_size),
    length_affected = ifelse(Project_name == project_8, NA, length_affected),
    river_basin = ifelse(Project_name == project_8, "Bastion Maria", river_basin)
  )

# Project 9

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_9, 2012, starting_year),
    year_completed = ifelse(Project_name == project_9, NA, year_completed),
    costs = ifelse(Project_name == project_9, NA, costs),
    area_size = ifelse(Project_name == project_9, 100, area_size),
    length_affected = ifelse(Project_name == project_9, NA, length_affected),
    river_basin = ifelse(Project_name == project_9, "Maas", river_basin)
  )

# Project 10

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_10, 2010, starting_year),
    year_completed = ifelse(Project_name == project_10, NA, year_completed),
    costs = ifelse(Project_name == project_10, 14300, costs),
    area_size = ifelse(Project_name == project_10, 1800, area_size),
    length_affected = ifelse(Project_name == project_10, NA, length_affected),
    river_basin = ifelse(Project_name == project_10, "Maas", river_basin)
  )

# Project 11

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_11, 2015, starting_year),
    year_completed = ifelse(Project_name == project_11, 2015, year_completed),
    costs = ifelse(Project_name == project_11, 14300, costs),
    area_size = ifelse(Project_name == project_11, 100, area_size),
    length_affected = ifelse(Project_name == project_11, NA, length_affected),
    river_basin = ifelse(Project_name == project_11, "Rijndelta", river_basin)
  )

# Project 12

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_12, 2017, starting_year),
    year_completed = ifelse(Project_name == project_12, NA, year_completed),
    costs = ifelse(Project_name == project_12, NA, costs),
    area_size = ifelse(Project_name == project_12, 100, area_size),
    length_affected = ifelse(Project_name == project_12, NA, length_affected),
    river_basin = ifelse(Project_name == project_12, "Rijndelta", river_basin)
  )

# Project 13

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_13, 2015, starting_year),
    year_completed = ifelse(Project_name == project_13, NA, year_completed),
    costs = ifelse(Project_name == project_13, NA, costs),
    area_size = ifelse(Project_name == project_13, 1800, area_size),
    length_affected = ifelse(Project_name == project_13, NA, length_affected),
    river_basin = ifelse(Project_name == project_13, "Maas", river_basin)
  )

# Project 14

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_14, 2014, starting_year),
    year_completed = ifelse(Project_name == project_14, NA, year_completed),
    costs = ifelse(Project_name == project_14, NA, costs),
    area_size = ifelse(Project_name == project_14, 100, area_size),
    length_affected = ifelse(Project_name == project_14, NA, length_affected),
    river_basin = ifelse(Project_name == project_14, "Maas", river_basin)
  )

# Project 15

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_15, 2014, starting_year),
    year_completed = ifelse(Project_name == project_15, NA, year_completed),
    costs = ifelse(Project_name == project_15, NA, costs),
    area_size = ifelse(Project_name == project_15, 1800, area_size),
    length_affected = ifelse(Project_name == project_15, NA, length_affected),
    river_basin = ifelse(Project_name == project_15, "Maas", river_basin)
  )

# Project 16

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_16, 2019, starting_year),
    year_completed = ifelse(Project_name == project_16, 2019, year_completed),
    costs = ifelse(Project_name == project_16, NA, costs),
    area_size = ifelse(Project_name == project_16, 100, area_size),
    length_affected = ifelse(Project_name == project_16, NA, length_affected),
    river_basin = ifelse(Project_name == project_16, "Rijndelta", river_basin)
  )

# Project 17

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_17, 2017, starting_year),
    year_completed = ifelse(Project_name == project_17, NA, year_completed),
    costs = ifelse(Project_name == project_17, NA, costs),
    area_size = ifelse(Project_name == project_17, 100, area_size),
    length_affected = ifelse(Project_name == project_17, NA, length_affected),
    river_basin = ifelse(Project_name == project_17, "Rijndelta", river_basin)
  )

# Project 18

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_18, 2013, starting_year),
    year_completed = ifelse(Project_name == project_18, NA, year_completed),
    costs = ifelse(Project_name == project_18, NA, costs),
    area_size = ifelse(Project_name == project_18, 100, area_size),
    length_affected = ifelse(Project_name == project_18, NA, length_affected),
    river_basin = ifelse(Project_name == project_18, "Rijndelta", river_basin)
  )

# Project 19

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_19, 1994, starting_year),
    year_completed = ifelse(Project_name == project_19, 1997, year_completed),
    costs = ifelse(Project_name == project_19, NA, costs),
    area_size = ifelse(Project_name == project_19, 10000, area_size),
    length_affected = ifelse(Project_name == project_19, 1, length_affected),
    river_basin = ifelse(Project_name == project_19, "Millingerwaard", river_basin)
  )

# Project 20

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_20, 1999, starting_year),
    year_completed = ifelse(Project_name == project_20, 2004, year_completed),
    costs = ifelse(Project_name == project_20, 1233000, costs),
    area_size = ifelse(Project_name == project_20, 100, area_size),
    length_affected = ifelse(Project_name == project_20, 39, length_affected),
    river_basin = ifelse(Project_name == project_20, "Asón", river_basin)
  )

# Project 21

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_21, 1998, starting_year),
    year_completed = ifelse(Project_name == project_21, NA, year_completed),
    costs = ifelse(Project_name == project_21, NA, costs),
    area_size = ifelse(Project_name == project_21, 0.06, area_size),
    length_affected = ifelse(Project_name == project_21, 0.4, length_affected),
    river_basin = ifelse(Project_name == project_21, "Cuenca del ebro ", river_basin)
  )

# Project 22

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_22, 2013, starting_year),
    year_completed = ifelse(Project_name == project_22, NA, year_completed),
    costs = ifelse(Project_name == project_22, NA, costs),
    area_size = ifelse(Project_name == project_22, 0.45, area_size),
    length_affected = ifelse(Project_name == project_22, 0.6, length_affected),
    river_basin = ifelse(Project_name == project_22, "Cuenca del Cinca", river_basin)
  )

# Project 23

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_23, 1999, starting_year),
    year_completed = ifelse(Project_name == project_23, NA, year_completed),
    costs = ifelse(Project_name == project_23, NA, costs),
    area_size = ifelse(Project_name == project_23, 0.41, area_size),
    length_affected = ifelse(Project_name == project_23, 4, length_affected),
    river_basin = ifelse(Project_name == project_23, "Mieray del Campiazo", river_basin)
  )

# Project 24

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_24, 2000, starting_year),
    year_completed = ifelse(Project_name == project_24, NA, year_completed),
    costs = ifelse(Project_name == project_24, NA, costs),
    area_size = ifelse(Project_name == project_24, 7.5, area_size),
    length_affected = ifelse(Project_name == project_24, 8, length_affected),
    river_basin = ifelse(Project_name == project_24, "Rias Baixas", river_basin)
  )

# Project 25

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_25, 1997, starting_year),
    year_completed = ifelse(Project_name == project_25, NA, year_completed),
    costs = ifelse(Project_name == project_25, NA, costs),
    area_size = ifelse(Project_name == project_25, 0.06, area_size),
    length_affected = ifelse(Project_name == project_25, 0.781, length_affected),
    river_basin = ifelse(Project_name == project_25, "Cuenca del ebro", river_basin)
  )

# Project 26

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_26, 2007, starting_year),
    year_completed = ifelse(Project_name == project_26, NA, year_completed),
    costs = ifelse(Project_name == project_26, NA, costs),
    area_size = ifelse(Project_name == project_26, 0.39, area_size),
    length_affected = ifelse(Project_name == project_26, 1, length_affected),
    river_basin = ifelse(Project_name == project_26, "Cuenca del Júcar", river_basin)
  )

# Project 27

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_27, 2008, starting_year),
    year_completed = ifelse(Project_name == project_27, 2009, year_completed),
    costs = ifelse(Project_name == project_27, 5000000, costs),
    area_size = ifelse(Project_name == project_27, 10000, area_size),
    length_affected = ifelse(Project_name == project_27, 0.11, length_affected),
    river_basin = ifelse(Project_name == project_27, "Millingerwaard", river_basin)
  )

# Project 28

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_28, 2006, starting_year),
    year_completed = ifelse(Project_name == project_28, 2006, year_completed),
    costs = ifelse(Project_name == project_28, NA, costs),
    area_size = ifelse(Project_name == project_28, 1000, area_size),
    length_affected = ifelse(Project_name == project_28, NA, length_affected),
    river_basin = ifelse(Project_name == project_28, "Tajo", river_basin)
  )

# Project 29

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_29, 1998, starting_year),
    year_completed = ifelse(Project_name == project_29, 1998, year_completed),
    costs = ifelse(Project_name == project_29, NA, costs),
    area_size = ifelse(Project_name == project_29, NA, area_size),
    length_affected = ifelse(Project_name == project_29, 14.6, length_affected),
    river_basin = ifelse(Project_name == project_29, "Guadalquivir", river_basin)
  )

# Project 30

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_30, 2001, starting_year),
    year_completed = ifelse(Project_name == project_30, 2004, year_completed),
    costs = ifelse(Project_name == project_30, 789000, costs),
    area_size = ifelse(Project_name == project_30, NA, area_size),
    length_affected = ifelse(Project_name == project_30, 120, length_affected),
    river_basin = ifelse(Project_name == project_30, "Guadalquivir", river_basin)
  )

# Project 31

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_31, 2010, starting_year),
    year_completed = ifelse(Project_name == project_31, NA, year_completed),
    costs = ifelse(Project_name == project_31, 43550000, costs),
    area_size = ifelse(Project_name == project_31, NA, area_size),
    length_affected = ifelse(Project_name == project_31, 120, length_affected),
    river_basin = ifelse(Project_name == project_31, "Gaia", river_basin)
  )

# Project 32

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_32, 2008, starting_year),
    year_completed = ifelse(Project_name == project_32, 2008, year_completed),
    costs = ifelse(Project_name == project_32, NA, costs),
    area_size = ifelse(Project_name == project_32, NA, area_size),
    length_affected = ifelse(Project_name == project_32, NA, length_affected),
    river_basin = ifelse(Project_name == project_32, "Guadalquivir", river_basin)
  )

# Project 33

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_33, 2007, starting_year),
    year_completed = ifelse(Project_name == project_33, 2007, year_completed),
    costs = ifelse(Project_name == project_33, 15000, costs),
    area_size = ifelse(Project_name == project_33, NA, area_size),
    length_affected = ifelse(Project_name == project_33, 0.745, length_affected),
    river_basin = ifelse(Project_name == project_33, "Llobregat", river_basin)
  )

# Project 34

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_34, 2009, starting_year),
    year_completed = ifelse(Project_name == project_34, NA, year_completed),
    costs = ifelse(Project_name == project_34, 3879000, costs),
    area_size = ifelse(Project_name == project_34, NA, area_size),
    length_affected = ifelse(Project_name == project_34, 72, length_affected),
    river_basin = ifelse(Project_name == project_34, "Júcar", river_basin)
  )

# Project 35

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_35, 2015, starting_year),
    year_completed = ifelse(Project_name == project_35, 2017, year_completed),
    costs = ifelse(Project_name == project_35, 2791878, costs),
    area_size = ifelse(Project_name == project_35, NA, area_size),
    length_affected = ifelse(Project_name == project_35, NA, length_affected),
    river_basin = ifelse(Project_name == project_35, "Guadalquivir River", river_basin)
  )

# Project 36

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_36, 2019, starting_year),
    year_completed = ifelse(Project_name == project_36, NA, year_completed),
    costs = ifelse(Project_name == project_36, 960873, costs),
    area_size = ifelse(Project_name == project_36, 1.38, area_size),
    length_affected = ifelse(Project_name == project_36, NA, length_affected),
    river_basin = ifelse(Project_name == project_36, "Guadalquivir River", river_basin)
  )

# Project 37

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_37, 2008, starting_year),
    year_completed = ifelse(Project_name == project_37, NA, year_completed),
    costs = ifelse(Project_name == project_37, NA, costs),
    area_size = ifelse(Project_name == project_37, NA, area_size),
    length_affected = ifelse(Project_name == project_37, 17, length_affected),
    river_basin = ifelse(Project_name == project_37, "Narcea", river_basin)
  )

# Project 38

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_38, 2008, starting_year),
    year_completed = ifelse(Project_name == project_38, 2015, year_completed),
    costs = ifelse(Project_name == project_38, 11176536, costs),
    area_size = ifelse(Project_name == project_38, 1.2, area_size),
    length_affected = ifelse(Project_name == project_38, NA, length_affected),
    river_basin = ifelse(Project_name == project_38, "Guadalquivir River", river_basin)
  )

# Project 39

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_39, 2011, starting_year),
    year_completed = ifelse(Project_name == project_39, NA, year_completed),
    costs = ifelse(Project_name == project_39, 16240000, costs),
    area_size = ifelse(Project_name == project_39, 8505, area_size),
    length_affected = ifelse(Project_name == project_39, 30, length_affected),
    river_basin = ifelse(Project_name == project_39, "Guadiana", river_basin)
  )

# Project 40

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_40, 2001, starting_year),
    year_completed = ifelse(Project_name == project_40, 2001, year_completed),
    costs = ifelse(Project_name == project_40, NA, costs),
    area_size = ifelse(Project_name == project_40, NA, area_size),
    length_affected = ifelse(Project_name == project_40, 0.215, length_affected),
    river_basin = ifelse(Project_name == project_40, "Guadalquivir", river_basin)
  )

# Project 41

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_41, 2009, starting_year),
    year_completed = ifelse(Project_name == project_41, 2009, year_completed),
    costs = ifelse(Project_name == project_41, 117000, costs),
    area_size = ifelse(Project_name == project_41, 1038, area_size),
    length_affected = ifelse(Project_name == project_41, 5.9, length_affected),
    river_basin = ifelse(Project_name == project_41, "Besós", river_basin)
  )

# Project 42

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_42, 2009, starting_year),
    year_completed = ifelse(Project_name == project_42, NA, year_completed),
    costs = ifelse(Project_name == project_42, 65000, costs),
    area_size = ifelse(Project_name == project_42, NA, area_size),
    length_affected = ifelse(Project_name == project_42, NA, length_affected),
    river_basin = ifelse(Project_name == project_42, "Miera", river_basin)
  )

# Project 43

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_43, 1996, starting_year),
    year_completed = ifelse(Project_name == project_43, 1996, year_completed),
    costs = ifelse(Project_name == project_43, NA, costs),
    area_size = ifelse(Project_name == project_43, 1000, area_size),
    length_affected = ifelse(Project_name == project_43, NA, length_affected),
    river_basin = ifelse(Project_name == project_43, "Pisuerga", river_basin)
  )

# Project 44

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_44, 2005, starting_year),
    year_completed = ifelse(Project_name == project_44, NA, year_completed),
    costs = ifelse(Project_name == project_44, NA, costs),
    area_size = ifelse(Project_name == project_44, NA, area_size),
    length_affected = ifelse(Project_name == project_44, NA, length_affected),
    river_basin = ifelse(Project_name == project_44, "Tordera", river_basin)
  )

# Project 45

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_45, 1998, starting_year),
    year_completed = ifelse(Project_name == project_45, 2005, year_completed),
    costs = ifelse(Project_name == project_45, 10000, costs),
    area_size = ifelse(Project_name == project_45, NA, area_size),
    length_affected = ifelse(Project_name == project_45, 12, length_affected),
    river_basin = ifelse(Project_name == project_45, "Guadalquivir", river_basin)
  )

# Project 46

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_46, 2001, starting_year),
    year_completed = ifelse(Project_name == project_46, 2004, year_completed),
    costs = ifelse(Project_name == project_46, 1439000, costs),
    area_size = ifelse(Project_name == project_46, 135, area_size),
    length_affected = ifelse(Project_name == project_46, NA, length_affected),
    river_basin = ifelse(Project_name == project_46, "Guadalquivir", river_basin)
  )

# Project 47

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_47, 2007, starting_year),
    year_completed = ifelse(Project_name == project_47, 2014, year_completed),
    costs = ifelse(Project_name == project_47, NA, costs),
    area_size = ifelse(Project_name == project_47, 1800, area_size),
    length_affected = ifelse(Project_name == project_47, 1.7, length_affected),
    river_basin = ifelse(Project_name == project_47, "Maas", river_basin)
  )

# Project 48

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_48, 2010, starting_year),
    year_completed = ifelse(Project_name == project_48, NA, year_completed),
    costs = ifelse(Project_name == project_48, 10000, costs),
    area_size = ifelse(Project_name == project_48, NA, area_size),
    length_affected = ifelse(Project_name == project_48, 0.0044, length_affected),
    river_basin = ifelse(Project_name == project_48, "NL27 BO 3 2", river_basin)
  )

# Project 49

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_49, 2013, starting_year),
    year_completed = ifelse(Project_name == project_49, 2020, year_completed),
    costs = ifelse(Project_name == project_49, 308000, costs),
    area_size = ifelse(Project_name == project_49, NA, area_size),
    length_affected = ifelse(Project_name == project_49, NA, length_affected),
    river_basin = ifelse(Project_name == project_49, "NA", river_basin)
  )


# Project 50

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_50, 2009, starting_year),
    year_completed = ifelse(Project_name == project_50, 2009, year_completed),
    costs = ifelse(Project_name == project_50, 369000, costs),
    area_size = ifelse(Project_name == project_50, NA, area_size),
    length_affected = ifelse(Project_name == project_50, NA, length_affected),
    river_basin = ifelse(Project_name == project_50, "Tajo", river_basin)
  )

# Project 51

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_51, 2009, starting_year),
    year_completed = ifelse(Project_name == project_51, 2009, year_completed),
    costs = ifelse(Project_name == project_51, 3135000, costs),
    area_size = ifelse(Project_name == project_51, 10000, area_size),
    length_affected = ifelse(Project_name == project_51, NA, length_affected),
    river_basin = ifelse(Project_name == project_51, "Pisuerga", river_basin)
  )

# Project 52

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_52, 2008, starting_year),
    year_completed = ifelse(Project_name == project_52, 2009, year_completed),
    costs = ifelse(Project_name == project_52, 507000, costs),
    area_size = ifelse(Project_name == project_52, NA, area_size),
    length_affected = ifelse(Project_name == project_52, NA, length_affected),
    river_basin = ifelse(Project_name == project_52, "Ter", river_basin)
  )

# Project 53

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_53, 2009, starting_year),
    year_completed = ifelse(Project_name == project_53, NA, year_completed),
    costs = ifelse(Project_name == project_53, 3245000, costs),
    area_size = ifelse(Project_name == project_53, NA, area_size),
    length_affected = ifelse(Project_name == project_53, NA, length_affected),
    river_basin = ifelse(Project_name == project_53, "Duero", river_basin)
  )

# Project 54

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_54, 1994, starting_year),
    year_completed = ifelse(Project_name == project_54, 1995, year_completed),
    costs = ifelse(Project_name == project_54, NA, costs),
    area_size = ifelse(Project_name == project_54, NA, area_size),
    length_affected = ifelse(Project_name == project_54, 2, length_affected),
    river_basin = ifelse(Project_name == project_54, "Llobregat", river_basin)
  )

# Project 55

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_55, 1999, starting_year),
    year_completed = ifelse(Project_name == project_55, 2002, year_completed),
    costs = ifelse(Project_name == project_55, 1278000, costs),
    area_size = ifelse(Project_name == project_55, NA, area_size),
    length_affected = ifelse(Project_name == project_55, 2, length_affected),
    river_basin = ifelse(Project_name == project_55, "Minho", river_basin)
  )

# Project 56

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_56, 2016, starting_year),
    year_completed = ifelse(Project_name == project_56, 2019, year_completed),
    costs = ifelse(Project_name == project_56, NA, costs),
    area_size = ifelse(Project_name == project_56, 0.39, area_size),
    length_affected = ifelse(Project_name == project_56, NA, length_affected),
    river_basin = ifelse(Project_name == project_56, "Cuenca del Júcar", river_basin)
  )

# Project 57

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_57, 2016, starting_year),
    year_completed = ifelse(Project_name == project_57, 2019, year_completed),
    costs = ifelse(Project_name == project_57, NA, costs),
    area_size = ifelse(Project_name == project_57, 0.06, area_size),
    length_affected = ifelse(Project_name == project_57, NA, length_affected),
    river_basin = ifelse(Project_name == project_57, "Cuenca del ebro", river_basin)
  )

# Project 58

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_58, 2014, starting_year),
    year_completed = ifelse(Project_name == project_58, 2014, year_completed),
    costs = ifelse(Project_name == project_58, NA, costs),
    area_size = ifelse(Project_name == project_58, NA, area_size),
    length_affected = ifelse(Project_name == project_58, NA, length_affected),
    river_basin = ifelse(Project_name == project_58, "Vistula River", river_basin)
  )

# Project 59

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_59, 2016, starting_year),
    year_completed = ifelse(Project_name == project_59, 2019, year_completed),
    costs = ifelse(Project_name == project_59, NA, costs),
    area_size = ifelse(Project_name == project_59, 18.87, area_size),
    length_affected = ifelse(Project_name == project_59, NA, length_affected),
    river_basin = ifelse(Project_name == project_59, "Odiel", river_basin)
  )

# Project 60

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_60, 2013, starting_year),
    year_completed = ifelse(Project_name == project_60, 2016, year_completed),
    costs = ifelse(Project_name == project_60, 242, costs),
    area_size = ifelse(Project_name == project_60, NA, area_size),
    length_affected = ifelse(Project_name == project_60, NA, length_affected),
    river_basin = ifelse(Project_name == project_60, "Odiel", river_basin)
  )

# Project 61

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_61, 2007, starting_year),
    year_completed = ifelse(Project_name == project_61, 2014, year_completed),
    costs = ifelse(Project_name == project_61, 12000000, costs),
    area_size = ifelse(Project_name == project_61, NA, area_size),
    length_affected = ifelse(Project_name == project_61, NA, length_affected),
    river_basin = ifelse(Project_name == project_61, "Meuse", river_basin)
  )

# Project 62

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_62, 2011, starting_year),
    year_completed = ifelse(Project_name == project_62, 2013, year_completed),
    costs = ifelse(Project_name == project_62, NA, costs),
    area_size = ifelse(Project_name == project_62, 57, area_size),
    length_affected = ifelse(Project_name == project_62, 3.4, length_affected),
    river_basin = ifelse(Project_name == project_62, "Maas", river_basin)
  )

# Project 63

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_63, 2008, starting_year),
    year_completed = ifelse(Project_name == project_63, 2011, year_completed),
    costs = ifelse(Project_name == project_63, NA, costs),
    area_size = ifelse(Project_name == project_63, 1800, area_size),
    length_affected = ifelse(Project_name == project_63, 1.5, length_affected),
    river_basin = ifelse(Project_name == project_63, "Maas", river_basin)
  )

# Project 64

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_64, 2011, starting_year),
    year_completed = ifelse(Project_name == project_64, NA, year_completed),
    costs = ifelse(Project_name == project_64, 125, costs),
    area_size = ifelse(Project_name == project_64, NA, area_size),
    length_affected = ifelse(Project_name == project_64, 1.5, length_affected),
    river_basin = ifelse(
      Project_name == project_64,
      "Arroyo de las Ratoneras",
      river_basin
    )
  )

# Project 65

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_65, 2011, starting_year),
    year_completed = ifelse(Project_name == project_65, 2011, year_completed),
    costs = ifelse(Project_name == project_65, 9000, costs),
    area_size = ifelse(Project_name == project_65, 160, area_size),
    length_affected = ifelse(Project_name == project_65, NA, length_affected),
    river_basin = ifelse(Project_name == project_65, "Vistula River", river_basin)
  )

# Project 66

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_66, 2012, starting_year),
    year_completed = ifelse(Project_name == project_66, 2016, year_completed),
    costs = ifelse(Project_name == project_66, 500000, costs),
    area_size = ifelse(Project_name == project_66, 644, area_size),
    length_affected = ifelse(Project_name == project_66, NA, length_affected),
    river_basin = ifelse(Project_name == project_66, "The Raba River", river_basin)
  )

# Project 67

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_67, 2013, starting_year),
    year_completed = ifelse(Project_name == project_67, 2017, year_completed),
    costs = ifelse(Project_name == project_67, 1000000, costs),
    area_size = ifelse(Project_name == project_67, 18870, area_size),
    length_affected = ifelse(Project_name == project_67, 54, length_affected),
    river_basin = ifelse(Project_name == project_67, "Segura", river_basin)
  )

# Project 68

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_68, 2009, starting_year),
    year_completed = ifelse(Project_name == project_68, 2010, year_completed),
    costs = ifelse(Project_name == project_68, NA, costs),
    area_size = ifelse(Project_name == project_68, NA, area_size),
    length_affected = ifelse(Project_name == project_68, 0.17, length_affected),
    river_basin = ifelse(Project_name == project_68, "Llobregat", river_basin)
  )

# Project 69

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_69, 2011, starting_year),
    year_completed = ifelse(Project_name == project_69, 2015, year_completed),
    costs = ifelse(Project_name == project_69, NA, costs),
    area_size = ifelse(Project_name == project_69, 700, area_size),
    length_affected = ifelse(Project_name == project_69, 4, length_affected),
    river_basin = ifelse(Project_name == project_69, "Rijndelta", river_basin)
  )

# Project 70

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_70, 1990, starting_year),
    year_completed = ifelse(Project_name == project_70, 2012, year_completed),
    costs = ifelse(Project_name == project_70, 10000, costs),
    area_size = ifelse(Project_name == project_70, 10000, area_size),
    length_affected = ifelse(Project_name == project_70, 10, length_affected),
    river_basin = ifelse(Project_name == project_70, "Millingerwaard", river_basin)
  )

# Project 71

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_71, 2011, starting_year),
    year_completed = ifelse(Project_name == project_71, 2015, year_completed),
    costs = ifelse(Project_name == project_71, NA, costs),
    area_size = ifelse(Project_name == project_71, NA, area_size),
    length_affected = ifelse(Project_name == project_71, NA, length_affected),
    river_basin = ifelse(Project_name == project_71, "Nieuwe Merwede", river_basin)
  )

# Project 72

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_72, 1994, starting_year),
    year_completed = ifelse(Project_name == project_72, 1994, year_completed),
    costs = ifelse(Project_name == project_72, NA, costs),
    area_size = ifelse(Project_name == project_72, 10000, area_size),
    length_affected = ifelse(Project_name == project_72, 1, length_affected),
    river_basin = ifelse(Project_name == project_72, "Millingerwaard", river_basin)
  )

# Project 73

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_73, 2008, starting_year),
    year_completed = ifelse(Project_name == project_73, 2008, year_completed),
    costs = ifelse(Project_name == project_73, 29000, costs),
    area_size = ifelse(Project_name == project_73, 1, area_size),
    length_affected = ifelse(Project_name == project_73, 0.07, length_affected),
    river_basin = ifelse(Project_name == project_73, "Tordera", river_basin)
  )

# Project 74

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_74, 2003, starting_year),
    year_completed = ifelse(Project_name == project_74, 2005, year_completed),
    costs = ifelse(Project_name == project_74, 35373000, costs),
    area_size = ifelse(Project_name == project_74, NA, area_size),
    length_affected = ifelse(Project_name == project_74, NA, length_affected),
    river_basin = ifelse(Project_name == project_74, "Guadalquivir", river_basin)
  )

# Project 75

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_75, 2003, starting_year),
    year_completed = ifelse(Project_name == project_75, 2005, year_completed),
    costs = ifelse(Project_name == project_75, 12929000, costs),
    area_size = ifelse(Project_name == project_75, NA, area_size),
    length_affected = ifelse(Project_name == project_75, NA, length_affected),
    river_basin = ifelse(Project_name == project_75, "Guadalquivir", river_basin)
  )

# Project 76

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_76, 2003, starting_year),
    year_completed = ifelse(Project_name == project_76, 2005, year_completed),
    costs = ifelse(Project_name == project_76, 12929000, costs),
    area_size = ifelse(Project_name == project_76, NA, area_size),
    length_affected = ifelse(Project_name == project_76, NA, length_affected),
    river_basin = ifelse(Project_name == project_76, "Guadalquivir", river_basin)
  )

# Project 77

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_77, 2008, starting_year),
    year_completed = ifelse(Project_name == project_77, 2010, year_completed),
    costs = ifelse(Project_name == project_77, 300000, costs),
    area_size = ifelse(Project_name == project_77, 26, area_size),
    length_affected = ifelse(Project_name == project_77, 6, length_affected),
    river_basin = ifelse(Project_name == project_77, "Oka", river_basin)
  )

# Project 78

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_78, 2000, starting_year),
    year_completed = ifelse(Project_name == project_78, 2000, year_completed),
    costs = ifelse(Project_name == project_78, 558000, costs),
    area_size = ifelse(Project_name == project_78, NA, area_size),
    length_affected = ifelse(Project_name == project_78, NA, length_affected),
    river_basin = ifelse(Project_name == project_78, "Tajo", river_basin)
  )

# Project 80

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_80, 2012, starting_year),
    year_completed = ifelse(Project_name == project_80, 2014, year_completed),
    costs = ifelse(Project_name == project_80, 4833217, costs),
    area_size = ifelse(Project_name == project_80, NA, area_size),
    length_affected = ifelse(Project_name == project_80, NA, length_affected),
    river_basin = ifelse(Project_name == project_80, "Biala", river_basin)
  )

# Project 81

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_81, 2005, starting_year),
    year_completed = ifelse(Project_name == project_81, 2006, year_completed),
    costs = ifelse(Project_name == project_81, 152000, costs),
    area_size = ifelse(Project_name == project_81, 1000, area_size),
    length_affected = ifelse(Project_name == project_81, NA, length_affected),
    river_basin = ifelse(Project_name == project_81, "Aragón", river_basin)
  )

# Project 82

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_82, 2005, starting_year),
    year_completed = ifelse(Project_name == project_82, 2005, year_completed),
    costs = ifelse(Project_name == project_82, 9712000, costs),
    area_size = ifelse(Project_name == project_82, NA, area_size),
    length_affected = ifelse(Project_name == project_82, NA, length_affected),
    river_basin = ifelse(Project_name == project_82, "Guadalquivir", river_basin)
  )


# Project 83

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_83, 2005, starting_year),
    year_completed = ifelse(Project_name == project_83, 2005, year_completed),
    costs = ifelse(Project_name == project_83, 2746000, costs),
    area_size = ifelse(Project_name == project_83, NA, area_size),
    length_affected = ifelse(Project_name == project_83, NA, length_affected),
    river_basin = ifelse(Project_name == project_83, "Guadalquivir", river_basin)
  )

# Project 84

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_84, 2007, starting_year),
    year_completed = ifelse(Project_name == project_84, 2007, year_completed),
    costs = ifelse(Project_name == project_84, 36000, costs),
    area_size = ifelse(Project_name == project_84, NA, area_size),
    length_affected = ifelse(Project_name == project_84, 0.459, length_affected),
    river_basin = ifelse(Project_name == project_84, "Besós", river_basin)
  )

# Project 85

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_85, 1998, starting_year),
    year_completed = ifelse(Project_name == project_85, 2005, year_completed),
    costs = ifelse(Project_name == project_85, 19972000, costs),
    area_size = ifelse(Project_name == project_85, NA, area_size),
    length_affected = ifelse(Project_name == project_85, NA, length_affected),
    river_basin = ifelse(Project_name == project_85, "Guadalquivir", river_basin)
  )

# Project 86

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_86, 2009, starting_year),
    year_completed = ifelse(Project_name == project_86, 2009, year_completed),
    costs = ifelse(Project_name == project_86, 139000, costs),
    area_size = ifelse(Project_name == project_86, NA, area_size),
    length_affected = ifelse(Project_name == project_86, 0.07, length_affected),
    river_basin = ifelse(Project_name == project_86, "Tordera", river_basin)
  )

# Project 87

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_87, 2008, starting_year),
    year_completed = ifelse(Project_name == project_87, NA, year_completed),
    costs = ifelse(Project_name == project_87, 451000000, costs),
    area_size = ifelse(Project_name == project_87, NA, area_size),
    length_affected = ifelse(Project_name == project_87, 3.187, length_affected),
    river_basin = ifelse(Project_name == project_87, "Tajo", river_basin)
  )

# Project 88

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_88, 2008, starting_year),
    year_completed = ifelse(Project_name == project_88, NA, year_completed),
    costs = ifelse(Project_name == project_88, NA, costs),
    area_size = ifelse(Project_name == project_88, NA, area_size),
    length_affected = ifelse(Project_name == project_88, 14, length_affected),
    river_basin = ifelse(Project_name == project_88, "Júcar", river_basin)
  )

# Project 89

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_89, 2008, starting_year),
    year_completed = ifelse(Project_name == project_89, 2008, year_completed),
    costs = ifelse(Project_name == project_89, 94000, costs),
    area_size = ifelse(Project_name == project_89, NA, area_size),
    length_affected = ifelse(Project_name == project_89, 0.108, length_affected),
    river_basin = ifelse(Project_name == project_89, "El Fluviá", river_basin)
  )

# Project 90

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_90, 2008, starting_year),
    year_completed = ifelse(Project_name == project_90, 2009, year_completed),
    costs = ifelse(Project_name == project_90, 413000, costs),
    area_size = ifelse(Project_name == project_90, NA, area_size),
    length_affected = ifelse(Project_name == project_90, 0.907, length_affected),
    river_basin = ifelse(Project_name == project_90, "El Riera de Reixac", river_basin)
  )


# Project 91

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_91, 2015, starting_year),
    year_completed = ifelse(Project_name == project_91, NA, year_completed),
    costs = ifelse(Project_name == project_91, NA, costs),
    area_size = ifelse(Project_name == project_91, NA, area_size),
    length_affected = ifelse(Project_name == project_91, NA, length_affected),
    river_basin = ifelse(Project_name == project_91, "North sea", river_basin)
  )

# Project 92

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_92, 2010, starting_year),
    year_completed = ifelse(Project_name == project_92, 2013, year_completed),
    costs = ifelse(Project_name == project_92, 929000, costs),
    area_size = ifelse(Project_name == project_92, NA, area_size),
    length_affected = ifelse(Project_name == project_92, NA, length_affected),
    river_basin = ifelse(Project_name == project_92, "Riu Ter", river_basin)
  )

# Project 93

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_93, 2008, starting_year),
    year_completed = ifelse(Project_name == project_93, NA, year_completed),
    costs = ifelse(Project_name == project_93, 1999000, costs),
    area_size = ifelse(Project_name == project_93, NA, area_size),
    length_affected = ifelse(Project_name == project_93, 33.65, length_affected),
    river_basin = ifelse(Project_name == project_93, "Guadiana", river_basin)
  )


# Project 94

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_94, 2009, starting_year),
    year_completed = ifelse(Project_name == project_94, 2010, year_completed),
    costs = ifelse(Project_name == project_94, 155000, costs),
    area_size = ifelse(Project_name == project_94, NA, area_size),
    length_affected = ifelse(Project_name == project_94, 33.65, length_affected),
    river_basin = ifelse(Project_name == project_94, "Besós", river_basin)
  )

# Project 95

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_95, 2009, starting_year),
    year_completed = ifelse(Project_name == project_95, NA, year_completed),
    costs = ifelse(Project_name == project_95, NA, costs),
    area_size = ifelse(Project_name == project_95, NA, area_size),
    length_affected = ifelse(Project_name == project_95, 19.5, length_affected),
    river_basin = ifelse(Project_name == project_95, "Sella", river_basin)
  )

# Project 96

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_96, 2024, starting_year),
    year_completed = ifelse(Project_name == project_96, NA, year_completed),
    costs = ifelse(Project_name == project_96, 1426461, costs),
    area_size = ifelse(Project_name == project_96, NA, area_size),
    length_affected = ifelse(Project_name == project_96, NA, length_affected),
    river_basin = ifelse(Project_name == project_96, "Segura", river_basin)
  )

# Project 97

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_97, 2010, starting_year),
    year_completed = ifelse(Project_name == project_97, 2010, year_completed),
    costs = ifelse(Project_name == project_97, 37000, costs),
    area_size = ifelse(Project_name == project_97, NA, area_size),
    length_affected = ifelse(Project_name == project_97, 0.414, length_affected),
    river_basin = ifelse(Project_name == project_97, "Llobregat", river_basin)
  )

# Project 98

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_98, 2014, starting_year),
    year_completed = ifelse(Project_name == project_98, 2014, year_completed),
    costs = ifelse(Project_name == project_98, 280000, costs),
    area_size = ifelse(Project_name == project_98, NA, area_size),
    length_affected = ifelse(Project_name == project_98, NA, length_affected),
    river_basin = ifelse(Project_name == project_98, "Arroyo de Valsequillo", river_basin)
  )

# Project 99

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_99, 2003, starting_year),
    year_completed = ifelse(Project_name == project_99, 2015, year_completed),
    costs = ifelse(Project_name == project_99, 10000, costs),
    area_size = ifelse(Project_name == project_99, 1000, area_size),
    length_affected = ifelse(Project_name == project_99, NA, length_affected),
    river_basin = ifelse(Project_name == project_99, "Rhine", river_basin)
  )

# Project 100

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_100, 2010, starting_year),
    year_completed = ifelse(Project_name == project_100, NA, year_completed),
    costs = ifelse(Project_name == project_100, 240000, costs),
    area_size = ifelse(Project_name == project_100, 1, area_size),
    length_affected = ifelse(Project_name == project_100, NA, length_affected),
    river_basin = ifelse(Project_name == project_100, "Guadalquivir", river_basin)
  )

# Project 101

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_101, 2001, starting_year),
    year_completed = ifelse(Project_name == project_101, 2006, year_completed),
    costs = ifelse(Project_name == project_101, 1444000, costs),
    area_size = ifelse(Project_name == project_101, NA, area_size),
    length_affected = ifelse(Project_name == project_101, NA, length_affected),
    river_basin = ifelse(Project_name == project_101, "Miño", river_basin)
  )

# Project 102

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_102, 2014, starting_year),
    year_completed = ifelse(Project_name == project_102, 2019, year_completed),
    costs = ifelse(Project_name == project_102, 1000000, costs),
    area_size = ifelse(Project_name == project_102, 18870, area_size),
    length_affected = ifelse(Project_name == project_102, 57, length_affected),
    river_basin = ifelse(Project_name == project_102, "Segura", river_basin)
  )

# Project 103

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_103, 2001, starting_year),
    year_completed = ifelse(Project_name == project_103, 2003, year_completed),
    costs = ifelse(Project_name == project_103, 1409000, costs),
    area_size = ifelse(Project_name == project_103, NA, area_size),
    length_affected = ifelse(Project_name == project_103, 0.055, length_affected),
    river_basin = ifelse(Project_name == project_103, "Tordera", river_basin)
  )

# Project 104

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_104, 2000, starting_year),
    year_completed = ifelse(Project_name == project_104, 2010, year_completed),
    costs = ifelse(Project_name == project_104, 420000, costs),
    area_size = ifelse(Project_name == project_104, NA, area_size),
    length_affected = ifelse(Project_name == project_104, NA, length_affected),
    river_basin = ifelse(Project_name == project_104, "Wilga", river_basin)
  )

# Project 104

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_104, 2000, starting_year),
    year_completed = ifelse(Project_name == project_104, 2010, year_completed),
    costs = ifelse(Project_name == project_104, 420000, costs),
    area_size = ifelse(Project_name == project_104, NA, area_size),
    length_affected = ifelse(Project_name == project_104, NA, length_affected),
    river_basin = ifelse(Project_name == project_104, "Wilga", river_basin)
  )

# Project 105

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_105, 2008, starting_year),
    year_completed = ifelse(Project_name == project_105, 2008, year_completed),
    costs = ifelse(Project_name == project_105, 450000, costs),
    area_size = ifelse(Project_name == project_105, NA, area_size),
    length_affected = ifelse(Project_name == project_105, NA, length_affected),
    river_basin = ifelse(Project_name == project_105, "Tajo", river_basin)
  )

# Project 106

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_106, 2013, starting_year),
    year_completed = ifelse(Project_name == project_106, 2013, year_completed),
    costs = ifelse(Project_name == project_106, 424000, costs),
    area_size = ifelse(Project_name == project_106, NA, area_size),
    length_affected = ifelse(Project_name == project_106, NA, length_affected),
    river_basin = ifelse(Project_name == project_106, "Bernesga River", river_basin)
  )

# Project 107

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_107, 1999, starting_year),
    year_completed = ifelse(Project_name == project_107, NA, year_completed),
    costs = ifelse(Project_name == project_107, NA, costs),
    area_size = ifelse(Project_name == project_107, 100, area_size),
    length_affected = ifelse(Project_name == project_107, NA, length_affected),
    river_basin = ifelse(Project_name == project_107, "Rijndelta", river_basin)
  )

# Project 108

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_108, 1999, starting_year),
    year_completed = ifelse(Project_name == project_108, 2011, year_completed),
    costs = ifelse(Project_name == project_108, 30000, costs),
    area_size = ifelse(Project_name == project_108, 400, area_size),
    length_affected = ifelse(Project_name == project_108, 30, length_affected),
    river_basin = ifelse(Project_name == project_108, "Maas", river_basin)
  )

# Project 109

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_109, 2018, starting_year),
    year_completed = ifelse(Project_name == project_109, 2019, year_completed),
    costs = ifelse(Project_name == project_109, 2507846, costs),
    area_size = ifelse(Project_name == project_109, NA, area_size),
    length_affected = ifelse(Project_name == project_109, NA, length_affected),
    river_basin = ifelse(Project_name == project_109, "Genil River", river_basin)
  )

# Project 110

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_110, 1995, starting_year),
    year_completed = ifelse(Project_name == project_110, 2010, year_completed),
    costs = ifelse(Project_name == project_110, 100000, costs),
    area_size = ifelse(Project_name == project_110, NA, area_size),
    length_affected = ifelse(Project_name == project_110, NA, length_affected),
    river_basin = ifelse(Project_name == project_110, "Gnida", river_basin)
  )

# Project 111

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_111, 2006, starting_year),
    year_completed = ifelse(Project_name == project_111, 2007, year_completed),
    costs = ifelse(Project_name == project_111, 138000, costs),
    area_size = ifelse(Project_name == project_111, NA, area_size),
    length_affected = ifelse(Project_name == project_111, 1, length_affected),
    river_basin = ifelse(Project_name == project_111, "Arga", river_basin)
  )

# Project 112

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_112, 1999, starting_year),
    year_completed = ifelse(Project_name == project_112, 2002, year_completed),
    costs = ifelse(Project_name == project_112, 5000, costs),
    area_size = ifelse(Project_name == project_112, 10000, area_size),
    length_affected = ifelse(Project_name == project_112, NA, length_affected),
    river_basin = ifelse(Project_name == project_112, "Rijndelta", river_basin)
  )

# Project 113

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_113, 2010, starting_year),
    year_completed = ifelse(Project_name == project_113, 2010, year_completed),
    costs = ifelse(Project_name == project_113, 93000, costs),
    area_size = ifelse(Project_name == project_113, 429, area_size),
    length_affected = ifelse(Project_name == project_113, 0.046, length_affected),
    river_basin = ifelse(Project_name == project_113, "Nansa", river_basin)
  )

# Project 114

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_114, 2009, starting_year),
    year_completed = ifelse(Project_name == project_114, 2009, year_completed),
    costs = ifelse(Project_name == project_114, 131000, costs),
    area_size = ifelse(Project_name == project_114, NA, area_size),
    length_affected = ifelse(Project_name == project_114, 0.215, length_affected),
    river_basin = ifelse(Project_name == project_114, "Besós", river_basin)
  )

# Project 115

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_115, 2011, starting_year),
    year_completed = ifelse(Project_name == project_115, 2011, year_completed),
    costs = ifelse(Project_name == project_115, 3999000, costs),
    area_size = ifelse(Project_name == project_115, 8505, area_size),
    length_affected = ifelse(Project_name == project_115, 13.5, length_affected),
    river_basin = ifelse(Project_name == project_115, "Guadiana", river_basin)
  )

# Project 116

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_116, 2011, starting_year),
    year_completed = ifelse(Project_name == project_116, 2011, year_completed),
    costs = ifelse(Project_name == project_116, 4997000, costs),
    area_size = ifelse(Project_name == project_116, 8505, area_size),
    length_affected = ifelse(Project_name == project_116, 9.5, length_affected),
    river_basin = ifelse(Project_name == project_116, "Guadiana", river_basin)
  )

# Project 117

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_117, 2011, starting_year),
    year_completed = ifelse(Project_name == project_117, 2011, year_completed),
    costs = ifelse(Project_name == project_117, 3748000, costs),
    area_size = ifelse(Project_name == project_117, 8505, area_size),
    length_affected = ifelse(Project_name == project_117, 8.5, length_affected),
    river_basin = ifelse(Project_name == project_117, "Guadiana", river_basin)
  )

# Project 118

restore_nps_new <- restore_nps_new %>%
  mutate(
    starting_year = ifelse(Project_name == project_118, 2011, starting_year),
    year_completed = ifelse(Project_name == project_118, 2011, year_completed),
    costs = ifelse(Project_name == project_118, 3499000, costs),
    area_size = ifelse(Project_name == project_118, 8505, area_size),
    length_affected = ifelse(Project_name == project_118, 2.5, length_affected),
    river_basin = ifelse(Project_name == project_118, "Guadiana", river_basin)
  )

# exclude 2024 to have same timespan as for RQ1
restore_nps_1990_2024 <- restore_nps_new %>%
  dplyr::filter(starting_year != 2024)

# calculate duration of work for projects
restore_nps_1990_2024 <- restore_nps_1990_2024 %>%
  mutate(Implementation_Time = year_completed - starting_year)

write_csv(restore_nps_1990_2024, "Restore-projects_1990-2024.csv") # save updated table as csv to be able to open it in arcgis



# Creating a table, that has only starting years and countries as variables and the summed up count of RESTORE projects
# and that sums the restore projects by their theme flood risk management
restore_aggregated_sy <- restore_nps_1990_2024 %>%
  separate_rows(Country, sep = ",\\s*") %>% # same name for the year is needed to be able to combine the tables
  group_by(`starting_year`, Country) %>%
  summarise(
    RESTOREProjectCount = n(),
    Flood_risk = sum(Flood_risk == 1),
    Total_costs = sum(costs, na.rm = TRUE),
    # sum of costs
    count_PROJECTCOSTS = sum(!is.na(costs)),
    # count of cells with costs unequal to NA
    .groups = 'drop'
  ) %>%
  rename(Year = `starting_year`) %>%
  mutate(Year = as.character(Year))

# all years needs to be included
restore_aggregated_sy_complete <- restore_aggregated_sy %>%
  complete(Year = as.character(seq(1990, 2024, by = 1)), Country = unique(Country)) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Creating a table, that has only completion years and countries as variables and the summed up count of RESTORE projects
restore_aggregated_yc <- restore_nps_1990_2024 %>%
  separate_rows(Country, sep = ",\\s*") %>% # in some cases there are two countries in one cell, so we need to separate
  group_by(`year_completed`, Country) %>%
  summarise(
    RESTOREProjectCount = n(),
    Flood_risk = sum(Flood_risk == 1),
    .groups = 'drop'
  ) %>%
  rename(Year = `year_completed`) %>% # same name for the year is needed to be able to combine the tables
  mutate(Year = as.character(Year))  # the year columns need to have the same format


# calculation new column for projects' implementation time
restore_nps_1990_2024 <- restore_nps_1990_2024 %>%
  mutate(Implementation_Time = year_completed - starting_year)

# calculating descriptive statistics for Restore projects
restore_mean <- restore_nps_1990_2024 %>%
  separate_rows(Country, sep = ",\\s*") %>%
  group_by(Country) %>%
  summarise(
    RESTOREProjectCount = n(),
    mean_RESTOREProjectCount = n() / 34,
    Flood_risk = sum(Flood_risk == 1),
    Mean_Flood_risk = sum(Flood_risk == 1) / 34,
    projects_with_costs = sum(!is.na(costs)),
    total_costs = sum(costs, na.rm = TRUE),
    costs_per_project = ifelse(projects_with_costs > 0, total_costs / projects_with_costs, NA),
    # Vermeidung von Division durch Null
    costs_per_flriskproject = ifelse(Flood_risk == 1, sum(costs[Flood_risk == 1], na.rm = TRUE) / Flood_risk, NA),
    .groups = 'drop'
  )

# 2.2. Preparing Natural hazards data -------------------------------------

# Research timeline 1990-2024
NHazards_1990_2024 <- NHazards %>%
  dplyr::filter(as.numeric(Year) %in% selected_years)

# Delete columns without values
NHazards_1990_2024 <- NHazards_1990_2024
NHazards_1990_2024$"Unnamed_ 1" <- NULL # Deletes the column 2024, because no data in data_1_clean are available for that year

# Research countries: netherlands, poland, spain
NHazards_nps_1990_2024 <- NHazards_1990_2024 %>%
  dplyr::filter(Country %in% selected_countries)


# Solve data after Regions to work with in in ArcGIS pro
NHazards_nps_1990_2024_solved <- NHazards_1990_2024 %>%
  separate_rows(Region2024, sep = ";\\s*")
# save the separated table
write.csv(NHazards_nps_1990_2024_solved,
          "Flood_events_regions_1990-2024.csv") # save separated table as csv to be able to open it in arcgis


# Summarize the data to NUTS0 level
NHazards_nps_1990_2024_nuts0 <- NHazards_nps_1990_2024 %>%
  group_by(Country, Year) %>%
  summarise(
    NaturalHazardCount = n(),
    .groups = 'drop',
    `Total Losses (k€)` = sum(LossesEuro, na.rm = TRUE) / 1000,
    `Flooded area (km2)` = sum(Area, na.rm = TRUE),
    `Fatalities (Persons killed)` = sum(Fatalities, na.rm = TRUE),
    `Persons affected by flood` = sum(Persons, na.rm = TRUE)
  ) %>%
  mutate(Year = as.character(Year))

# save comprised table
write.csv(NHazards_nps_1990_2024_nuts0,
          "Flood_events_regions_1990-2024_sum.csv") # save separated table as csv to be able to open it in arcgis



### Generate a table with average values of Count, flooded area, losses, fatalities and affected persons

# Table for the count of floods, to join it later with the NHazards_nps_1990_2024_filled data
NHazards_nps_1990_2024_count <- NHazards_nps_1990_2024 %>%
  group_by(Country) %>%
  summarise(NaturalHazardCount = n(), .groups = 'drop')

# Making year numeric
NHazards_nps_1990_2024 <- NHazards_nps_1990_2024 %>%
  mutate(Year = as.integer(Year))

# Create a grid for all years and countries
complete_grid <- expand_grid(Country = selected_countries, Year = selected_years)

# Every year and country that has no values gets the value 0 (if there is no flood listed the hypothesis is, that then there were no important floods)
NHazards_nps_1990_2024_filled <- complete_grid %>%
  left_join(NHazards_nps_1990_2024, by = c("Country", "Year")) %>%
  mutate(
    LossesEuro = if_else(is.na(LossesEuro), 0, as.double(LossesEuro)),
    Area = if_else(is.na(Area), 0, as.double(Area)),
    Fatalities = if_else(is.na(Fatalities), 0, as.integer(Fatalities)),
    Persons = if_else(is.na(Persons), 0, as.integer(Persons))
  )

# Summarize the relevant data for each country (and to make it easier to read, divide Losses with 1000)
NHazards_nps_1990_2024_filled <- NHazards_nps_1990_2024_filled %>%
  group_by(Country) %>%
  summarise(
    TotalLosses_kEUR = sum(LossesEuro) / 1000,
    FloodedArea_km2 = sum(Area),
    Total_Fatalities = sum(Fatalities),
    Total_PersonsAffected = sum(Persons),
    .groups = 'drop'
  )


# calculate mean for every country for the years 1990-2024
NHazards_nps_1990_2024_mean <- NHazards_nps_1990_2024_filled %>%
  left_join(NHazards_nps_1990_2024_count, by = c("Country")) %>%
  group_by(Country) %>%
  summarise(
    Avg_NaturalHazardCount = sum(NaturalHazardCount) / 34,
    Avg_TotalLosses_year_kEUR = (TotalLosses_kEUR) / 34,
    Avg_TotalLosses_year_mioEUR = (TotalLosses_kEUR / 1000) / 34,
    Avg_TotalLosses_count_kEUR = (TotalLosses_kEUR) / (NaturalHazardCount),
    # mean losses per flood
    Avg_FloodedArea_km2 = mean(FloodedArea_km2, na.rm = TRUE),
    Avg_Fatalities = mean(Total_Fatalities, na.rm = TRUE),
    Avg_PersonsAffected = mean(Total_PersonsAffected, na.rm = TRUE),
    .groups = 'drop'
  )

# summarized data
NHazards_nps_1990_2024_mean <- left_join(NHazards_nps_1990_2024_mean,
                                         NHazards_nps_1990_2024_filled,
                                         by = c("Country"))
# add the count of Natural hazards
NHazards_nps_1990_2024_mean <- left_join(NHazards_nps_1990_2024_mean,
                                         NHazards_nps_1990_2024_count,
                                         by = c("Country"))


## create a comprised table only for future plots
# calculate count of floods per year and country
hazards_per_year_country <- NHazards_nps_1990_2024 %>%
  mutate(Year = as.character(Year)) %>%
  group_by(Year, Country) %>%
  summarise(
    NaturalHazardCount = n(),
    .groups = 'drop',
    `Total Losses (k€)` = sum(LossesEuro, na.rm = TRUE) / 1000
  ) %>%
  left_join(
    NHazards_nps_1990_2024_mean %>%
      select(Country, Avg_NaturalHazardCount, Avg_TotalLosses_year_kEUR),
    by = "Country"
  )

# Summarize the data after to NUTS0 level after type of flood
NHazards_nps_1990_2024_type <- NHazards_nps_1990_2024 %>%
  group_by(Country, Year, Type) %>%
  summarise(NaturalHazardCount = n(), .groups = 'drop', )

# Make Types columns
NHazards_nps_1990_2024_type <- NHazards_nps_1990_2024_type %>%
  mutate(Year = as.character(Year)) %>%
  pivot_wider(
    names_from = Type,
    values_from = NaturalHazardCount,
    values_fill = list(NaturalHazardCount = 0)
  )

# Join separated types to future ggp1-table
hazards_per_year_country <- hazards_per_year_country %>%
  left_join(NHazards_nps_1990_2024_type, by = c("Country", "Year"))

# add missing years and replace NA-values with 0. To be able to create interactive graphics
hazards_per_year_country_complete <- hazards_per_year_country %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  complete(
    Year = as.character(seq(1990, 2024, by = 1)),
    Country = unique(Country),
    fill = list(
      `NaturalHazardCount` = 0,
      `Total Losses (k€)` = 0,
      `Avg_NaturalHazardCount` = 0,
      `Avg_TotalLosses_year_kEUR` = 0,
      River = 0,
      Flash = 0,
      `River/Coastal` = 0,
      Coastal = 0
    )
  ) %>%
  mutate(
    Year = as.factor(Year),
    Alpha = ifelse(`Total Losses (k€)` > Avg_TotalLosses_year_kEUR, 1, 0.5) # for one plot
  )

# 2.3. Prepare GDP data ---------------------------------------------------

# rename column
GDP <- GDP %>%
  rename(Country = "Country Name")

# select the three relevant countries
gdp_nps <- GDP %>%
  dplyr::filter(Country %in% selected_countries)

# Tranform to long-table, to be able to join with natural hazards
gdp_nps_long <- gdp_nps %>%
  pivot_longer(cols = matches("^19|^20"),
               names_to = "Year",
               values_to = "Value") %>%
  rename("GDP (in mio €, contant prices 2020)" = Value)

# Calculate mean for every country for the years 1990-2024
gdp_nps_mean <- gdp_nps_long %>%
  group_by(Country) %>%
  summarise(
    average_GDP_mio_EUR = mean(`GDP (in mio €, contant prices 2020)`, na.rm = TRUE),
    .groups = 'drop'
  )

# 2.4. Prepare Population data --------------------------------------------

## Clean table
# saving new column names in a table
population_new_colnames <- Population[7, ]
population_new_colnames <- population_new_colnames %>%
  select(
    -c(
      3,
      5,
      7,
      9,
      11,
      13,
      15,
      17,
      19,
      21,
      23,
      25,
      27,
      29,
      31,
      33,
      35,
      37,
      39,
      41,
      43,
      45,
      47,
      49,
      51,
      53,
      55,
      57,
      59,
      61,
      63,
      65,
      67,
      69,
      71,
      73
    )
  )

# delete description cells, also that columns that indicate a break or estimates
population_ <- Population[-c(1:8, 61:71), ]
population_ <- population_ %>%
  select(
    -c(
      3,
      5,
      7,
      9,
      11,
      13,
      15,
      17,
      19,
      21,
      23,
      25,
      27,
      29,
      31,
      33,
      35,
      37,
      39,
      41,
      43,
      45,
      47,
      49,
      51,
      53,
      55,
      57,
      59,
      61,
      63,
      65,
      67,
      69,
      71,
      73
    )
  )

# setting new column names
colnames(population_) <- population_new_colnames
population_ <- population_ %>%
  rename(Country = TIME)

# select the three relevant countries
population_nps <- population_ %>%
  dplyr::filter(Country %in% selected_countries)

## Transform to long-table, to be able to join with natural hazards
population_nps <- population_nps %>%
  pivot_longer(cols = matches("^19|^20"),
               names_to = "Year",
               values_to = "Value") %>%
  rename("Population" = Value)

# making field population numeric
population_nps <- population_nps %>%
  mutate(Population = as.numeric(Population))

# Calculating mean of population for the years 1990-2024
population_nps_mean <- population_nps %>%
  group_by(Country) %>%
  summarise(
    average_mio_population = mean(`Population` / 1000000, na.rm = TRUE),
    .groups = 'drop'
  )


# 2.5. Filter other data --------------------------------------------------

sealed_nps <- sealed %>%
  dplyr::filter(Country %in% selected_countries)

rivers_nps <- rivers %>%
  rename(Country = NUTS_ID) %>%
  mutate(
    Country = case_when(
      Country == "ES" ~ "Spain",
      Country == "PL" ~ "Poland",
      Country == "NL" ~ "Netherlands",
      TRUE ~ Country  # Falls keine der Bedingungen zutrifft, behält den Originalwert bei
    )
  ) %>%
  dplyr::filter(Country %in% selected_countries)

country_nps <- country %>%
  dplyr::filter(Country %in% selected_countries)


# 2.6. Join data ----------------------------------------------------------

# Create overview table of natural hazards and setting them in context ----

# firstly all years needs to be in the dataset
NHazards_nps_1990_2024_nuts0 <- NHazards_nps_1990_2024_nuts0 %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  complete(
    Year = as.character(seq(1990, 2024, by = 1)),
    Country = unique(Country),
    fill = list(
      `NaturalHazardCount` = 0,
      `Total Losses (k€)` = 0,
      `Flooded area (km2)` = 0,
      `Fatalities (Persons killed)` = 0,
      `Persons affected by flood` = 0
    )
  ) %>%
  mutate(Year = factor(Year, levels = as.character(seq(1990, 2024))))

# Join Natural hazards and GDP
NHazards_nps_1990_2024_nuts0 <- left_join(NHazards_nps_1990_2024_nuts0,
                                          gdp_nps_long,
                                          by = c("Country", "Year"))

# Join Natural hazards and Population
NHazards_nps_1990_2024_nuts0 <- left_join(NHazards_nps_1990_2024_nuts0,
                                          population_nps,
                                          by = c("Country", "Year"))


# save table
write_xlsx(
  NHazards_nps_1990_2024_nuts0 ,
  "NaturalHazards_1990-2024_demographics_summarised.xls"
)

## Create overview table of mean natural hazards data and setting them in context
NHazards_nps_1990_2024_mean <- NHazards_nps_1990_2024_mean %>%
  left_join(gdp_nps_mean, by = "Country") %>%
  left_join(population_nps_mean, by = "Country") %>%
  left_join(sealed_nps, by = "Country") %>%
  left_join(rivers_nps, by = "Country") %>%
  left_join(country_nps, by = "Country") %>%
  left_join(restore_mean, by = "Country")

# save table
write_xlsx(NHazards_nps_1990_2024_mean ,
           "NaturalHazards_1990-2024_mean.xls")



# Long table of NaturalHazards and RESTORE

# Merge the prepared tables
hazards_restore_amount_sy_frm <- full_join(NHazards_nps_1990_2024_nuts0,
                                           restore_aggregated_sy,
                                           by = c("Year", "Country"))
hazards_restore_amount_yc <- full_join(NHazards_nps_1990_2024_nuts0,
                                       restore_aggregated_yc,
                                       by = c("Year", "Country"))

# replace new NA-values with 0
hazards_restore_amount_sy_frm <- hazards_restore_amount_sy_frm %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
hazards_restore_amount_yc <- hazards_restore_amount_yc %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# delete rows where year is NA
hazards_restore_amount_sy_frm <- hazards_restore_amount_sy_frm %>%
  dplyr::filter(!is.na(Year))

hazards_restore_amount_yc <- hazards_restore_amount_yc %>%
  dplyr::filter(!is.na(Year))

#################################################################################


# 3. Analysing data -------------------------------------------------------


# 3.1 Creating new comprised tables ---------------------------------------

# Calculating correlation for descriptive statistics

## T-test with a significance level of 5% (two sides)
# Calculating correlation and p-value for LossesEuro and GDP per country
cor_loss_gdp_by_country <- NHazards_nps_1990_2024_nuts0 %>%
  group_by(Country) %>%
  summarise(
    correlation_gdp = cor(`Total Losses (k€)`, `GDP (in mio €, contant prices 2020)`, use = "complete.obs"),
    p_value_gdp = cor.test(`Total Losses (k€)`, `GDP (in mio €, contant prices 2020)`, use = "complete.obs")$p.value
  )

# Calculating correlation and p-value for LossesEuro and flooded area per country
cor_loss_flarea_by_country <- NHazards_nps_1990_2024_nuts0 %>%
  group_by(Country) %>%
  summarise(
    correlation_area = cor(`Total Losses (k€)`, `Flooded area (km2)`, use = "complete.obs"),
    p_value_area = cor.test(`Total Losses (k€)`, `Flooded area (km2)`, use = "complete.obs")$p.value
  )

# Calculating correlation and p-value for Count of Natural Hazards and RESTORE Projects
cor_count_hazards_projects_by_country <- hazards_restore_amount_sy_frm %>%
  group_by(Country) %>%
  summarise(
    correlation_count = cor(NaturalHazardCount, RESTOREProjectCount, use = "complete.obs"),
    p_value_count = cor.test(NaturalHazardCount, RESTOREProjectCount, use = "complete.obs")$p.value
  )

# Calculating correlation and p-value for Economic Losses of natural hazards and RESTORE projects
cor_losses_hazards_projects_by_country <- hazards_restore_amount_sy_frm %>%
  group_by(Country) %>%
  summarise(
    correlation_losses = cor(`Total Losses (k€)`, Total_costs, use = "complete.obs"),
    p_value_losses = cor.test(`Total Losses (k€)`, Total_costs, use = "complete.obs")$p.value
  )



# Merging correlation-datasets
NHazards_nps_1990_2024_mean <- NHazards_nps_1990_2024_mean %>%
  left_join(cor_loss_gdp_by_country, by = "Country") %>%
  left_join(cor_loss_flarea_by_country, by = "Country") %>%
  left_join(cor_count_hazards_projects_by_country, by = "Country") %>%
  left_join(cor_losses_hazards_projects_by_country, by = "Country")

# adjust column names for correlation columns
NHazards_nps_1990_2024_mean <- NHazards_nps_1990_2024_mean %>%
  rename(
    `Correlation (Losses vs GDP)` = correlation_gdp,
    `Correlation (Losses vs flooded Area)` = correlation_area,
    `Correlation (Count Natural Hazards vs Projects)` = correlation_count,
    `Correlation (Losses Natural Hazards vs Costs Projects)` = correlation_losses,
    `p-value 0,05 (Losses vs GDP)` = p_value_gdp,
    `p-value 0,05 (Losses vs flooded Area)` = p_value_area,
    `p-value 0,05 (Count Natural Hazards vs Projects)` = p_value_count,
    `p-value 0,05 (Losses Natural Hazards vs Costs Projects)` = p_value_losses
  )


# make some calculations to compare Restore projects and Natural hazards with the context
NHazards_nps_1990_2024_mean <- NHazards_nps_1990_2024_mean %>%
  mutate(
    LossesKeur_per_floodedarea = TotalLosses_kEUR / FloodedArea_km2,
    LossesKeur_per_flood = TotalLosses_kEUR / NaturalHazardCount,
    Losses_per_year_person = Avg_TotalLosses_year_kEUR / average_mio_population,
    AvgFloodedarea_per_Landsize = Avg_FloodedArea_km2 / `Land size(km²)`,
    LossesHazardsPerFlood_and_builtupPerCountrySize = LossesKeur_per_flood / (`Built up area` / `Land size(km²)`),
    riverlength_per_CountHazards = SUM_Rivers_river_lenght_km / NaturalHazardCount,
    riverlength_per_CountProjects = SUM_Rivers_river_lenght_km / RESTOREProjectCount,
    PersonsAffected_CountProjects = Total_PersonsAffected / RESTOREProjectCount,
    Lossesyearperson_per_CountProjects = TotalLosses_kEUR / RESTOREProjectCount,
    CountProjects_CountHazards = RESTOREProjectCount / NaturalHazardCount,
    CountHazards_CountProjects = NaturalHazardCount / RESTOREProjectCount,
    LossesHazards_CostsProjects = (TotalLosses_kEUR * 1000) / total_costs,
    PersonsAffected_per_Population = Total_PersonsAffected / (average_mio_population * 1000000),
  )

# Extract "Country"-column to functioning as title
country_names <- NHazards_nps_1990_2024_mean$Country

# Delete "Country"-Column before transposing
data_without_country <- NHazards_nps_1990_2024_mean %>% select(-Country)

# Transpose dataframe
flood_stats_transposed <- as.data.frame(t(data_without_country))

# Set originally column "countries" as column-names
colnames(flood_stats_transposed) <- country_names
flood_stats_transposed <- tibble::rownames_to_column(flood_stats_transposed, "Metric")

# all values should be numeric
flood_stats_transposed[, -1] <- lapply(flood_stats_transposed[, -1], function(x)
  as.numeric(as.character(x)))

# change to gt object
flood_stats_gt <- flood_stats_transposed %>%
  gt()

# Save as html table
gtsave(flood_stats_gt, "flood_stats.html")



# 3.2. Creating graphs ----------------------------------------------------

# 3.2.1. Amount of natural hazards by floods per country and year ---------

# creating graph with adjusted brightness (more floods than mean = bright color)
ggp1 <- hazards_per_year_country_complete %>%
  complete(
    Year = as.character(seq(1990, 2024, by = 1)),
    Country = unique(Country),
    fill = list(NaturalHazardCount = 0)
  ) %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(
    x = Year,
    y = NaturalHazardCount,
    fill = Country,
    group = Country,
    alpha = (NaturalHazardCount >= Avg_NaturalHazardCount)
  )) +
  labs(title = "Natural hazards per country and year (1990 - 2024)", x = "Year", y = "Count of natural hazards") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(2, "lines"),
    panel.grid.major.x = element_line(color = "grey80", size = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", size = 0.2),
    panel.grid.major.y = element_line(color = "grey80", size = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", size = 0.2),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_discrete(breaks = seq(1990, 2024, by = 5)) +
  scale_y_continuous(
    breaks = seq(0, 15, by = 2),
    minor_breaks = seq(0, 15, by = 1),
    labels = scales::number_format(accuracy = 1)
  ) +
  geom_vline(
    xintercept = as.character(seq(1990, 2024, by = 1)),
    color = "grey90",
    linetype = "solid",
    linewidth = 0.2
  ) +
  geom_vline(
    xintercept = as.character(c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024)),
    color = "grey80",
    linetype = "solid",
    linewidth = 0.5
  )+
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  scale_fill_manual(values = c(
    "Netherlands" = "#3A9AB2FF",
    "Poland" = "#91BAB6FF",
    "Spain" = "#BDC881FF"
  )) +
  scale_alpha_identity() +
  geom_hline(
    data = hazards_per_year_country_complete,
    aes(yintercept = Avg_NaturalHazardCount, color = Country),
    linewidth = 1,
    linetype = "dotted"
  ) +
  scale_color_manual(values = c(
    "Netherlands" = "#3A9AB2FF",
    "Poland" = "#91BAB6FF",
    "Spain" = "#BDC881FF"
  )) + # add mean-line with their country-specific color
  annotate(
    "text",
    x = "2012",
    y = 0.5,
    label = "Netherlands",
    color = "#3A9AB2FF",
    size = 3,
    hjust = 0
  ) +
  annotate(
    "text",
    x = "2012",
    y = 1.3,
    label = "Poland",
    color = "#91BAB6FF",
    size = 3,
    hjust = 0
  ) +
  annotate(
    "text",
    x = "2012",
    y = 5.8,
    label = "Spain",
    color = "#BDC881FF",
    size = 3,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = "2007",
    xend = "2007",
    y = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.95,
    yend = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.65,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = "2007",
    y = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 2.1,
    label = "Floods directive",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = "2010",
    xend = "2015",
    y = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.65,
    yend = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.65,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = "2010",
    y = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.85,
    label = "1. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = "2016",
    xend = "2021",
    y = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.65,
    yend = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.65,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = "2016",
    y = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.85,
    label = "2. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = "2022",
    xend = "2027",
    y = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.65,
    yend = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.65,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = "2022",
    y = max(
      hazards_per_year_country_complete$Avg_NaturalHazardCount,
      na.rm = TRUE
    ) * 1.85,
    label = "3. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  )

# Show plot
print(ggp1)

# Save ggp1 as picture
ggsave(
  "Natural_hazards_wide.png",
  plot = ggp1,
  width = 16,
  height = 5,
  dpi = 300
)


# 3.2.2. Losses by natural hazards for every country per year -------------

ggp_2 <- ggplot(
  hazards_per_year_country_complete,
  aes(
    x = Year,
    y = `Total Losses (k€)`,
    fill = Country,
    alpha = Alpha
  )
) +
  labs(title = "Economic losses by natural hazards per country and year (1990 - 2024)", x = "Year", y = "Economic losses by natural hazards (k€)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(2, "lines"),
    panel.grid.major.x = element_line(color = "grey80", size = 0.5),
    panel.grid.minor.x = element_line(color = "grey90", size = 0.2),
    panel.grid.major.y = element_line(color = "grey80", size = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", size = 0.2),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_discrete(breaks = seq(1990, 2024, by = 5)) +
  scale_y_continuous(
    breaks = seq(0, 10000000, by = 2000000),
    minor_breaks = seq(0, 10000000, by = 1000000),
    labels = scales::number_format(accuracy = 1)
  ) +
    geom_vline(
    xintercept = as.character(seq(1990, 2024, by = 1)),
    color = "grey90",
    linetype = "solid",
    linewidth = 0.2
  ) +
  geom_vline(
    xintercept = as.character(c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024)),
    color = "grey80",
    linetype = "solid",
    linewidth = 0.5
  )+
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  scale_fill_manual(values = c(
    "Netherlands" = "#3A9AB2FF",
    "Poland" = "#91BAB6FF",
    "Spain" = "#BDC881FF"
  )) +
  scale_alpha_identity(
    limits = c(0.6, 1),
    breaks = c(0.6, 1),
    labels = c("Lower than mean", "Higher than mean"),
    name = "Economic losses"
  ) +
  guides(
    fill = guide_legend(title = "Country"),
    alpha = guide_legend(title = "Economic losses", override.aes = list(linetype = rep(c(
      "blank"
    ), 2)))
  ) +
  geom_hline(
    aes(yintercept = Avg_TotalLosses_year_kEUR, color = Country),
    size = 1,
    linetype = "dotted"
  ) +
  scale_color_manual(values = c(
    "Netherlands" = "#3A9AB2FF",
    "Poland" = "#91BAB6FF",
    "Spain" = "#BDC881FF"
  )) +
  annotate(
    "text",
    x = "2015",
    y = 220000,
    label = "Netherlands",
    color = "#3A9AB2FF",
    size = 3,
    hjust = 0
  ) +
  annotate(
    "text",
    x = "2013",
    y = 150000,
    label = "Poland",
    color = "#91BAB6FF",
    size = 3,
    hjust = 0
  ) +
  annotate(
    "text",
    x = "2013",
    y = 1000000,
    label = "Spain",
    color = "#BDC881FF",
    size = 3,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = "2007",
    xend = "2007",
    y = 9500000,
    yend = 7000000,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = "2007",
    y = 10000000,
    label = "Floods directive",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = "2010",
    xend = "2015",
    y = 7000000,
    yend = 7000000,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = "2010",
    y = 8000000,
    label = "1. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = "2016",
    xend = "2021",
    y = 7000000,
    yend = 7000000,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = "2016",
    y = 8000000,
    label = "2. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = "2022",
    xend = "2027",
    y = 7000000,
    yend = 7000000,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = "2022",
    y = 8000000,
    label = "3. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  )


# show plot
print(ggp_2)

# save plot
ggsave(
  "Natural_hazards_economiclosses_wide.png",
  plot = ggp_2,
  width = 16,
  height = 5,
  dpi = 300
)



# 3.2.3. Count of RESTORE projects per year and country -------------------

ggp3 <- ggplot(restore_aggregated_sy_complete, aes(x = as.numeric(Year))) +
  labs(
    title = "River restoration projects per country and year (1990 - 2024)",
    subtitle = "General projects and flood risk management projects highlighted",
    x = "Year",
    y = "Count of RESTORE projects"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5)) +
  scale_y_continuous(
    breaks = seq(0, 15, by = 2),
    minor_breaks = seq(0, 15, by = 1),
    labels = scales::number_format(accuracy = 1)
  ) +
  geom_vline(
    xintercept = seq(1990, 2024, by = 1),
    color = "grey90",
    linetype = "solid",
    size = 0.2
  ) +
  geom_vline(
    xintercept = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024),
    color = "grey80",
    linetype = "solid",
    size = 0.5
  ) +
  geom_col(
    aes(y = RESTOREProjectCount, fill = Country, alpha = "Total Projects"),
    width = 0.8,
    position = "dodge"
  ) +
  geom_col(
    aes(y = Flood_risk, fill = Country, alpha = "Flood Risk Projects"),
    width = 0.8,
    position = "dodge"
  ) +
  scale_fill_manual(values = c(
    "Netherlands" = "#3A9AB2FF",
    "Poland" = "#91BAB6FF",
    "Spain" = "#BDC881FF"
  )) +
  scale_alpha_manual(
    name = "Project Type",
    values = c("Total Projects" = 0.6, "Flood Risk Projects" = 1),
    labels = c("General RESTORE projects", "Flood risk management projects")
  ) +
  guides(
    fill = guide_legend(title = "Country"),
    alpha = guide_legend(title = "Project theme", override.aes = list(linetype = "blank"))
  )+
  annotate(
    "segment",
    x = 2007,
    xend = 2007,
    y = 11,
    yend = 8,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 2007,
    y = 12,
    label = "Floods directive",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = 2010,
    xend = 2015,
    y = 8,
    yend = 8,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 2010,
    y = 10,
    label = "1. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = 2016,
    xend = 2021,
    y = 8,
    yend = 8,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 2016,
    y = 10,
    label = "2. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = 2022,
    xend = 2027,
    y = 8,
    yend = 8,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 2022,
    y = 10,
    label = "3. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  )

# show plot with 2 cautions, because in the restore_nps_1990_2024 data were already missing two starting years
print(ggp3)

# Making the plot interactive, but tooltip is not working
# ggp3_interactive <- ggplotly(ggp3, tooltip = "text")
#
# # Layout-Anpassungen in Plotly
# ggp3_interactive <- ggp3_interactive %>%
#   plotly::layout(
#     title = list(text = "River restoration projects per country and year (1990 - 2024)<br><sup>General projects and flood risk management projects highlighted</sup>"),
#     xaxis = list(title = "Year", tickangle = 45),
#     yaxis = list(title = "Amount of projects"),
#     legend = list(orientation = "h", x = 0.5, xanchor = "center"),
#     margin = list(t = 50, b = 100),
#     plot_bgcolor = "#F7F7F7"
#   )
# #show interactive plot
# ggp3_interactive
# save graphic as picture
ggsave(
  "restore_wide.png",
  plot = ggp3,
  width = 16,
  height = 5,
  dpi = 300
)

#################################################################################


# 3.2.4. Combined: RESTORE projects and Natural hazards -------------------

# divided per country
ggp_combined <- ggplot(hazards_restore_amount_sy_frm, aes(x = as.numeric(Year))) +
  labs(
    title = "RESTORE projects and natural hazards (1990 - 2024)",
    subtitle = "Includes general RESTORE projects, flood risk management, and hazards",
    x = "Year",
    y = "Count of ..."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5)) +
  scale_y_continuous(
    breaks = seq(0, 15, by = 2),
    minor_breaks = seq(0, 15, by = 1),
    labels = scales::number_format(accuracy = 1)
  ) +
  geom_vline(
    xintercept = seq(1990, 2024, by = 1),
    color = "grey90",
    linetype = "solid",
    size = 0.2
  ) +
  geom_vline(
    xintercept = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024),
    color = "grey80",
    linetype = "solid",
    size = 0.5
  ) +
  geom_col(
    aes(y = RESTOREProjectCount, fill = Country, alpha = "Total Projects"),
    width = 0.8,
    position = "dodge"
  ) +
  geom_col(
    aes(y = Flood_risk, fill = Country, alpha = "Flood Risk Projects"),
    width = 0.8,
    position = "dodge"
  ) +
  geom_point(aes(y = NaturalHazardCount, color = Country, shape = "Natural hazards"),
             size = 2) +
  geom_smooth(
    aes(y = NaturalHazardCount, color = Country),
    method = "loess",
    se = FALSE,
    size = 1
  ) +
  scale_fill_manual(values = c(
    "Netherlands" = "#3A9AB2FF",
    "Poland" = "#91BAB6FF",
    "Spain" = "#BDC881FF"
  )) +
  scale_color_manual(values = c(
    "Netherlands" = "#3A9AB2FF",
    "Poland" = "#91BAB6FF",
    "Spain" = "#BDC881FF"
  )) +
  scale_alpha_manual(
    name = "Project Type",
    values = c("Total Projects" = 0.6, "Flood Risk Projects" = 1),
    labels = c("General RESTORE projects", "Flood risk management projects")
  ) +
  scale_shape_manual(
    values = c("Natural hazards" = 16),
    guide = guide_legend(title = "Natural hazards")
  ) +
  guides(
    fill = guide_legend(title = "Country"),
    alpha = guide_legend(title = "Project theme"),
    shape = guide_legend(title = ""),
    color = guide_legend(title = "Country")
  ) +
  facet_wrap( ~ Country, scales = "fixed") +
  annotate(
    "segment",
    x = 2007,
    xend = 2007,
    y = 11,
    yend = 8,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 2007,
    y = 11.5,
    label = "Floods directive",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = 2010,
    xend = 2015,
    y = 8,
    yend = 8,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 2010,
    y = 9,
    label = "1. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = 2016,
    xend = 2021,
    y = 8,
    yend = 8,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 2016,
    y = 9,
    label = "2. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = 2022,
    xend = 2027,
    y = 8,
    yend = 8,
    color = "black",
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate(
    "text",
    x = 2022,
    y = 9,
    label = "3. Implemen-\n tation phase",
    color = "black",
    size = 4,
    hjust = 0
  )

# Zeige den Plot
print(ggp_combined)

# save plot
ggsave(
  "RESTORE_Hazards.png",
  plot = ggp_combined,
  width = 30,
  height = 8,
  dpi = 300
)


# 3.2.4.1. Amount of Natural hazards and started projects -----------------

# all three countries summed up
ggp_4 <- hazards_restore_amount_sy_frm %>%
  group_by(Year) %>%
  summarise(
    NaturalHazardCount = sum(NaturalHazardCount, na.rm = TRUE),
    RESTOREProjectCount = sum(RESTOREProjectCount, na.rm = TRUE),
    Flood_risk = sum(Flood_risk, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = as.numeric(Year))) +
  labs(
    title = "RESTORE projects and natural hazards (1990 - 2024)",
    subtitle = "Includes general RESTORE projects, flood risk management and natural hazards - \
       (summarized across Netherlands, Poland, Spain)",
    x = "Year",
    y = "Count of...",
    caption = "* Brightness shows relation to count of natural hazards"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(
      linewidth = 0.5,
      linetype = "solid",
      color = "grey80"
    ),
    panel.grid.minor.y = element_line(
      linewidth = 0.2,
      linetype = "solid",
      color = "grey90"
    )
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5)) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1),
    breaks = seq(0, 15, by = 5),
    minor_breaks = seq(0, 15, by = 1),
    limits = c(0, 15)
  ) +
  geom_vline(
    xintercept = seq(1990, 2024, by = 1),
    color = "grey90",
    linetype = "solid",
    linewidth = 0.2
  ) +
  geom_vline(
    xintercept = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024),
    color = "grey80",
    linetype = "solid",
    linewidth = 0.5
  ) +
  geom_col(
    aes(
      y = RESTOREProjectCount,
      alpha = ifelse(RESTOREProjectCount > NaturalHazardCount, 1, 0.5),
      fill = "General Projects"
    ),
    width = 0.8,
    position = "identity"
  ) +
  geom_col(
    aes(y = Flood_risk, fill = "Flood Risk Management Projects"),
    width = 0.8,
    position = "identity"
  ) +
  geom_point(aes(y = NaturalHazardCount, color = factor(
    ifelse(
      NaturalHazardCount > RESTOREProjectCount,
      "More Hazards",
      "Fewer Hazards"
    )
  )), size = 2) +
  geom_smooth(
    aes(y = NaturalHazardCount),
    method = "loess",
    se = FALSE,
    linewidth = 1,
    color = "#315B88FF"
  ) +
  scale_fill_manual(
    "Project Type",
    values = c(
      "General Projects" = "#C2D89AFF",
      "Flood Risk Management Projects" = "#8CBF9AFF"
    )
  ) +
  scale_alpha_identity() +
  scale_color_manual(
    "Natural Hazard Status",
    values = c(
      "More Hazards" = "#191F40FF",
      "Fewer Hazards" = "#477B95FF"
    ),
    labels = c("More Hazards" = "More natural hazards than RESTORE projects", "Fewer Hazards" = "Fewer natural hazards than RESTORE projects")
  ) +
  guides(
    fill = guide_legend(title = "RESTORE projects*", override.aes = list(
      alpha = c(1, 1),
      fill = c("#8CBF9AFF", "#C2D89AFF")
    )),
    color = guide_legend(title = "Natural hazards")
  )

# show plot
print(ggp_4)

# save plot
ggsave(
  "restore_hazards_start.png",
  plot = ggp_4,
  width = 16,
  height = 5,
  dpi = 300
)


# 3.2.4.2. Amount of natural hazards and completed projects ---------------

ggp_5 <- hazards_restore_amount_yc %>%
  group_by(Year) %>%
  summarize(
    NaturalHazardCount = sum(NaturalHazardCount, na.rm = TRUE),
    RESTOREProjectCount = sum(RESTOREProjectCount, na.rm = TRUE),
    Flood_risk = sum(Flood_risk, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = as.numeric(Year))) +
  labs(
    title = "Natural hazards and completed RESTORE Projects (1990 - 2024)",
    subtitle = "Includes general RESTORE projects, flood risk management and natural hazards - \
       (summarized across the Netherlands, Poland, Spain)",
    x = "Year",
    y = "Count of...",
    caption = "* Brightness shows relation to count of natural hazards"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(
      linewidth = 0.5,
      linetype = "solid",
      color = "grey80"
    ),
    panel.grid.minor.y = element_line(
      linewidth = 0.2,
      linetype = "solid",
      color = "grey90"
    )
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5)) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1),
    breaks = seq(0, 15, by = 5),
    minor_breaks = seq(0, 15, by = 1),
    limits = c(0, 15)
  ) +
  geom_vline(
    xintercept = seq(1990, 2024, by = 1),
    color = "grey90",
    linetype = "solid",
    size = 0.2
  ) +
  geom_vline(
    xintercept = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024),
    color = "grey80",
    linetype = "solid",
    size = 0.5
  ) +
  geom_col(
    aes(
      y = RESTOREProjectCount,
      alpha = ifelse(RESTOREProjectCount > NaturalHazardCount, 1, 0.5),
      fill = "General Projects"
    ),
    width = 0.8,
    position = "identity"
  ) +
  geom_col(
    aes(y = Flood_risk, fill = "Flood Risk Management Projects"),
    width = 0.8,
    position = "identity"
  ) +
  geom_point(aes(y = NaturalHazardCount, color = factor(
    ifelse(
      NaturalHazardCount > RESTOREProjectCount,
      "More Hazards",
      "Fewer Hazards"
    )
  )), size = 2) +
  geom_smooth(
    aes(y = NaturalHazardCount),
    method = "loess",
    se = FALSE,
    size = 1,
    color = "#315B88FF"
  ) +
  scale_fill_manual(
    "Project Type",
    values = c(
      "General Projects" = "#C2D89AFF",
      "Flood Risk Management Projects" = "#8CBF9AFF"
    )
  ) +
  scale_alpha_identity() +
  scale_color_manual(
    "Natural Hazard Status",
    values = c(
      "More Hazards" = "#191F40FF",
      "Fewer Hazards" = "#477B95FF"
    ),
    labels = c("More Hazards" = "More natural hazards than RESTORE projects", "Fewer Hazards" = "Fewer natural hazards than RESTORE projects")
  ) +
  guides(
    fill = guide_legend(title = "RESTORE projects*", override.aes = list(
      alpha = c(1, 1),
      fill = c("#8CBF9AFF", "#C2D89AFF")
    )),
    color = guide_legend(title = "Natural hazards")
  )

# show plot
print(ggp_5)

# save plot
ggsave(
  "restore_hazards_completed.png",
  plot = ggp_5,
  width = 16,
  height = 5,
  dpi = 300
)


# 3.2.4.3. Economic losses and Amount of Restore projects -----------------
# including flood risk managment projects (two axis)

ggp_el_frm <- hazards_restore_amount_sy_frm %>%
  group_by(Year) %>%
  summarize(
    `Total Losses (k€)` = sum(`Total Losses (k€)`, na.rm = TRUE),
    RESTOREProjectCount = sum(RESTOREProjectCount, na.rm = TRUE),
    Flood_risk = sum(Flood_risk, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = as.numeric(Year))) +
  geom_vline(
    xintercept = seq(1990, 2024, by = 1),
    color = "grey90",
    linetype = "solid",
    size = 0.2
  ) +
  geom_vline(
    xintercept = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024),
    color = "grey80",
    linetype = "solid",
    size = 0.5
  ) +
  geom_col(aes(y = RESTOREProjectCount * 1000000, fill = "General Projects"),
           show.legend = TRUE) +
  geom_col(
    aes(y = Flood_risk * 1000000, fill = "Flood Risk Management Projects"),
    position = position_dodge(width = 0.9),
    show.legend = TRUE
  ) +
  geom_point(
    aes(y = `Total Losses (k€)`, color = "Economic Losses"),
    size = 2,
    shape = 16,
    show.legend = TRUE
  ) +
  geom_smooth(
    data = subset(
      hazards_restore_amount_sy_frm,
      `Total Losses (k€)` <= quantile(`Total Losses (k€)`, 0.95, na.rm = TRUE)
    ),
    # ignores the last 5% (potentially outliers)
    aes(y = `Total Losses (k€)`, color = "Economic Losses"),
    method = "loess",
    se = FALSE,
    size = 1,
    linetype = "dotted",
    show.legend = TRUE
  ) +
  scale_y_continuous(
    name = "Economic losses by natural hazards (k€)",
    limits = c(0, 15000000),
    breaks = seq(0, 15000000, by = 5000000),
    minor_breaks = seq(0, 15000000, by = 1000000),
    sec.axis = sec_axis( ~ . / 1000000, breaks = seq(0, 15, by = 2), name = "Count of RESTORE projects")
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5)) +
  scale_fill_manual(
    name = "Legend",
    values = c(
      "General Projects" = "#C2D89AFF",
      "Flood Risk Management Projects" = "#8CBF9AFF"
    )
  ) +
  scale_color_manual(name = "Legend",
                     values = c("Economic Losses" = "#191F40FF")) +
  labs(
    title = "Economic losses and RESTORE projects",
    x = "Year",
    subtitle = "Includes general RESTORE projects, flood risk management and natural hazards - \
       (summarized across the Netherlands, Poland, Spain)",
    caption = "* Outliers not included in regression"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(
      linewidth = 0.5,
      linetype = "solid",
      color = "grey80"
    ),
    panel.grid.minor.y = element_line(
      linewidth = 0.2,
      linetype = "solid",
      color = "grey90"
    )
  ) +
  guides(
    fill = guide_legend(title = "RESTORE projects", override.aes = list(
      alpha = c(1, 1),
      fill = c("#8CBF9AFF", "#C2D89AFF")
    )),
    color = guide_legend(title = "Natural hazards*")
  )

# show plot
print(ggp_el_frm)

# save plot
ggsave(
  "losses_floodrisk.png",
  plot = ggp_el_frm,
  width = 16,
  height = 5,
  dpi = 300
)


#################################################################################

# 3.2.5. Average loss as share of GDP per country (relates to RQ1) --------

# add data positions to make a similiar graph to the one of research question 1
data_with_positions <- NHazards_nps_1990_2024_mean %>%
  mutate(
    Position = case_when(
      Country == "Slowenia" ~ 1,
      Country == "Rumania" ~ 2,
      Country == "Bulgaria" ~ 3,
      Country == "Czechia" ~ 4,
      Country == "Croatia" ~ 5,
      Country == "Hungary" ~ 6,
      Country == "Greece" ~ 7,
      Country == "Lithuania" ~ 8,
      Country == "Portugal" ~ 9,
      Country == "Spain" ~ 10,
      Country == "Italy" ~ 11,
      Country == "Germany" ~ 12,
      Country == "France" ~ 13,
      Country == "Austria" ~ 14,
      Country == "Latvia" ~ 15,
      Country == "Poland" ~ 16,
      Country == "Belgium" ~ 17,
      Country == "Slovakia" ~ 18,
      Country == "Denmark" ~ 19,
      Country == "Luxembourg" ~ 20,
      Country == "Cyprus" ~ 21,
      Country == "Ireland" ~ 22,
      Country == "Netherlands" ~ 23,
      Country == "Estonia" ~ 24,
      Country == "Malta" ~ 25,
      Country == "Sweden" ~ 26,
      Country == "Finland" ~ 27
    )
  ) %>%
  complete(Position = 1:27) %>%
  mutate(
    Country = case_when(
      Position == 1 ~ "Slowenia",
      Position == 2 ~ "Rumania",
      Position == 3 ~ "Bulgaria",
      Position == 4 ~ "Czechia",
      Position == 5 ~ "Croatia",
      Position == 6 ~ "Hungary",
      Position == 7 ~ "Greece",
      Position == 8 ~ "Lithuania",
      Position == 9 ~ "Portugal",
      Position == 10 ~ "Spain",
      Position == 11 ~ "Italy",
      Position == 12 ~ "Germany",
      Position == 13 ~ "France",
      Position == 14 ~ "Austria",
      Position == 15 ~ "Latvia",
      Position == 16 ~ "Poland",
      Position == 17 ~ "Belgium",
      Position == 18 ~ "Slovakia",
      Position == 19 ~ "Denmark",
      Position == 20 ~ "Luxembourg",
      Position == 21 ~ "Cyprus",
      Position == 22 ~ "Ireland",
      Position == 23 ~ "Netherlands",
      Position == 24 ~ "Estonia",
      Position == 25 ~ "Malta",
      Position == 26 ~ "Sweden",
      Position == 27 ~ "Finland",
      TRUE ~ NA_character_
    ),
    fill_color = Country
  )

# Create the plot
gg_relative_losses_bar <- ggplot(
  data_with_positions,
  aes(
    x = Avg_TotalLosses_year_mioEUR / average_GDP_mio_EUR,
    y = reorder(Position, desc(Position)),
    fill = Country
  )
) +
  geom_vline(
    xintercept = seq(0, 0.015, by = 0.0025),
    color = "grey90",
    linetype = "solid",
    size = 0.2
  ) +
  geom_vline(
    xintercept = c(0, 0.005, 0.01, 0.015),
    color = "grey80",
    linetype = "solid",
    size = 0.5
  ) +
  geom_col(width = 0.72) +
  geom_text(
    data = dplyr::filter(
      data_with_positions,
      Country %in% c("Spain", "Poland", "Netherlands")
    ),
    aes(
      label = scales::percent(Avg_TotalLosses_year_mioEUR / average_GDP_mio_EUR, accuracy = 0.01)
    ),
    hjust = -0.2,
    color = "black",
    size = 3
  ) +
  scale_fill_manual(
    values = c(
      "Spain" = "#BDC881FF",
      "Poland" = "#91BAB6FF",
      "Netherlands" = "#3A9AB2FF"
    ),
    na.value = "transparent"
  ) +
  scale_y_discrete(labels = rev(data_with_positions$Country)) +
  labs(
    title = "Average flood-related loss as share of GDP by country (1990-2024)",
    x = "Average loss as share of GDP",
    y = "",
    caption = "Average loss (million euro) divided by Average GDP (million euro)"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0, 0.015),
                     expand = expansion(mult = c(0, 0))) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.25),  # 细刻度线
    axis.text.y = element_text(size = 10, color = "grey20"),
    axis.text.x = element_text(size = 10, color = "grey30"),
    axis.title.x = element_text(size = 11, color = "grey20"),
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
    plot.margin = margin(10, 15, 10, 10)
  )

# show plot
print(gg_relative_losses_bar)

# save plot
ggsave(
  "risk_losses_gdp.png",
  plot = gg_relative_losses_bar,
  width = 10,
  height = 8,
  dpi = 300
)
