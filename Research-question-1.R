# ---
# Title: "Analysis of RQ1: What factors could explain the changes in the amount
#        of climate-related economic losses per country in Europe in the years 1990–2024?"
# Date:  "04.03.2026"
# Author: "Mingyang Yang"
# For writing the code and solving errors partly ChatGPT (LLM) was used
# ---

# Table of content
# 1. Prepare data
#    1.1 Prepare sealed area and land size data
#    1.2 Prepare GDP data
#    1.3 Prepare climate-related economic loss data
#    1.4 Prepare population data
#    1.5 Prepare temperature data
#    1.6 Prepare river data
#    1.7 Calculate GDP per capita
#    1.8 Merge datasets
# 2. Analyse data
#    2.1-Average climate-related loss per sealed area by country
#    2.2-Average climate-related loss as share of GDP by country
#    2.3-Correlation tests
#    2.4-Scatter plot: loss per sealed area vs temperature
#    2.5-Scatter plot: loss per sealed area vs population density
#    2.6-Scatter plot: loss per sealed area vs river density
#    2.7-Scatter plot: loss per sealed area vs GDP per capita
#    2.8-Export merged dataset
#    2.9-Country groups by GDP per capita and loss per sealed area (median split)
#    2.10-Calculate annual total economic losses in Europe


################################################################################
# 1.1-Dataset 1: Sealed area and land size (Artificial land and total area)
################################################################################

# Source
# Eurostat – Land covered by artificial surfaces by NUTS2 region
# Dataset: lan_lcv_art
# https://ec.europa.eu/eurostat/databrowser/view/lan_lcv_art
#
# Metadata:
# https://ec.europa.eu/eurostat/cache/metadata/en/lan_esms.htm


# Description
# This dataset reports the area of land covered by artificial surfaces
# across European regions at the NUTS2 level.
#
# Artificial surfaces represent land that has been converted from natural
# land cover into human-made structures or infrastructure.
#
# These surfaces typically include:
# - urban fabric
# - buildings
# - transport infrastructure
# - industrial and commercial areas
# - other constructed surfaces


# Variables used in this study
# From the available land cover categories we selected two variables:
#
# A00 – Artificial land
# TOTAL – Total area
#
# Artificial land is used in this project as a proxy for "sealed area".
# Although Eurostat uses the term "artificial land", this category is
# commonly used as an approximation for sealed or impervious surfaces,
# which strongly influence runoff generation and flood risk.
#
# Total area is used in this project as "land size".


# Geographic coverage
# EU-27 countries
#
# The analysis includes NUTS2 regions from the following EU member states:
# Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia,
# Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia,
# Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Romania,
# Slovakia, Slovenia, Spain, Sweden


# Spatial resolution
# NUTS2 regions


# Temporal coverage
# Available years in the dataset:
# 2009
# 2012
# 2015
# 2018
# 2022


# Unit of measurement
# Square kilometre (km²)
#
# The dataset reports the total land area and artificial land area
# for each NUTS2 region.


# Data preparation
# Two datasets were downloaded from the Eurostat database:
#
# 1. Artificial land (used as sealed area)
# 2. Total area (used as land size)
#
# Before importing the data into R, some summary rows and non-analytical
# information provided in the original Excel files were removed in Excel
# to avoid import and transformation problems in R.
#
# After import, both datasets are cleaned using the same procedure so that
# they can be used consistently in later analysis.


################################################################################
# Load required packages
################################################################################

# readxl: import Excel files
# dplyr: data cleaning and manipulation
library(readxl)
library(dplyr)


################################################################################
# Import datasets
################################################################################

# Import total area dataset (used as land size)
Land_size <- read_excel(
  "D:/1.24 RQ 1/DATASETS/Original/lan_lcv_art__custom_20421241_spreadsheet.xlsx"
)

# Import artificial land dataset (used as sealed area proxy)
Sealed_Area <- read_excel(
  "D:/1.24 RQ 1/DATASETS/Original/lan_lcv_art__custom_20421265_spreadsheet.xlsx"
)


# Result after import
# Land_size   → total area / land size
# Sealed_Area → artificial land area / sealed area proxy


################################################################################
# Clean both datasets
################################################################################

# A cleaning function is created and applied to both datasets in order to
# keep the workflow consistent.
#
# Cleaning steps:
# 1. Rename the first column as Country
# 2. Keep only the relevant year columns
# 3. Remove remaining non-data rows if necessary
# 4. Replace ":" with NA
# 5. Convert year columns to numeric
# 6. Calculate the mean value across available years for each country
#
# Note:
# In the downloaded Eurostat files, some flag columns may appear between
# year columns. Since they are not needed for this analysis, only the
# actual year columns are kept.

clean_land_dataset <- function(df) {
  
  # Rename first column as Country
  names(df)[1] <- "Country"
  
  # Keep only Country and year columns
  df_clean <- df %>%
    select(Country, `2009`, `2012`, `2015`, `2018`, `2022`)
  
  # Remove possible remaining non-data row
  df_clean <- df_clean %>%
    filter(Country != "GEO (Labels)")
  
  # Replace ":" with NA in all year columns
  df_clean <- df_clean %>%
    mutate(across(c(`2009`, `2012`, `2015`, `2018`, `2022`),
                  ~ na_if(as.character(.x), ":")))
  
  # Convert year columns to numeric
  df_clean <- df_clean %>%
    mutate(across(c(`2009`, `2012`, `2015`, `2018`, `2022`), as.numeric))
  
  # Calculate mean value across the available years
  df_clean <- df_clean %>%
    mutate(
      Mean_2009_2022 = rowMeans(
        select(., `2009`, `2012`, `2015`, `2018`, `2022`),
        na.rm = TRUE
      )
    )
  
  return(df_clean)
}


################################################################################
# Apply cleaning function
################################################################################

# Clean land size dataset
Land_size_clean <- clean_land_dataset(Land_size)

# Clean sealed area dataset
Sealed_Area_clean <- clean_land_dataset(Sealed_Area)

# =========================================================
# Rename the average column in Land_size dataset
# The column "Mean_2009_2022" is renamed to
# "Average_Land_Size(km2)" to clearly indicate the meaning
# =========================================================
Land_size_clean <- Land_size_clean %>%
  rename(`Average_Land_Size(km2)` = Mean_2009_2022)


# =========================================================
# Rename the average column in Sealed_Area dataset
# The column "Mean_2009_2022" is renamed to
# "Average_Seal_Area(km2)" to represent the average sealed area
# =========================================================
Sealed_Area_clean <- Sealed_Area_clean %>%
  rename(`Average_Seal_Area(km2)` = Mean_2009_2022)


# =========================================================
# Check results
# =========================================================
View(Land_size_clean)
View(Sealed_Area_clean)

################################################################################
# Check results
################################################################################

# View cleaned datasets
View(Land_size_clean)
View(Sealed_Area_clean)

# Optional: inspect first rows in console
head(Land_size_clean)
head(Sealed_Area_clean)


################################################################################
# 1.2-Dataset 2: GDP
################################################################################


# =========================================================
# Load required package
# readr: import CSV file
# =========================================================
library(readr)

# =========================================================
# Import GDP dataset from the World Development Indicators
# The dataset reports GDP in constant 2015 US$
# =========================================================
GDP <- read_delim(
  "D:/1.24 RQ 1/DATASETS/Original/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_234.csv",
  delim = ",",
  skip = 4,
  show_col_types = FALSE
)

# =========================================================
# Rename "Slovak Republic" as "Slovakia"
# This ensures consistency with the country naming used
# in the other datasets and in the selected EU country list
# =========================================================
GDP$`Country Name`[GDP$`Country Name` == "Slovak Republic"] <- "Slovakia"

# =========================================================
# Define the EU-27 country list used in this analysis
# =========================================================
EU27 <- c(
  "Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany",
  "Denmark", "Spain", "Estonia", "Finland", "France", "Greece",
  "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg",
  "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania",
  "Slovakia", "Slovenia", "Sweden"
)

# =========================================================
# Keep only EU-27 countries
# All other countries and regional aggregates are removed
# =========================================================
GDP <- GDP[GDP$`Country Name` %in% EU27, ]

# =========================================================
# Keep only the columns needed for the analysis:
# Country Name and annual GDP values from 1990 to 2024
# The metadata columns Country Code, Indicator Name,
# and Indicator Code are removed
# =========================================================
GDP <- GDP[, c("Country Name", as.character(1990:2024))]

# =========================================================
# Reorder rows to match the predefined EU-27 country list
# =========================================================
GDP <- GDP[match(EU27, GDP$`Country Name`), ]

# =========================================================
# Rename the first column from "Country Name" to "Country"
# This ensures consistency with the naming used
# in the other datasets
# =========================================================
names(GDP)[1] <- "Country"

# =========================================================
# Define the year columns used in the analysis
# =========================================================
year_columns <- as.character(1990:2024)

# =========================================================
# Convert GDP values from constant 2015 US$ to thousand Euro
#
# The original dataset reports GDP in constant 2015 US$.
# For consistency with the monetary framework used in this study,
# values are approximated into 2020 Euro terms.
# First, values are divided by 1000 to convert them into thousand units.
# Second, a factor of 0.95 is applied as an intermediate estimate,
# informed by exchange-rate and inflation considerations.
# =========================================================
GDP[year_columns] <- lapply(
  GDP[year_columns],
  function(x) as.numeric(x) / 1000 * 0.95
)

# =========================================================
# Prevent scientific notation in the R environment
# This makes values display as normal numbers instead of e+ notation
# =========================================================
options(scipen = 999)

# =========================================================
# Calculate the average GDP across 1990–2024 for each country
# A new column is added to the existing GDP dataset
# =========================================================
GDP$`Average GDP(thousand Euro)` <- rowMeans(GDP[year_columns], na.rm = TRUE)

# =========================================================
# Check the result
# =========================================================
dim(GDP)
names(GDP)
head(GDP)
View(GDP)

################################################################################
# 1.3-Dataset 3: Climate-related economic losses
################################################################################

# Source
# Eurostat – Climate related economic losses by type of event
# Dataset: cli_iad_loss
# https://ec.europa.eu/eurostat/databrowser/view/cli_iad_loss
#
# Metadata:
# https://ec.europa.eu/eurostat/cache/metadata/en/cli_iad_loss_esms.htm


# Description
# This dataset reports climate-related economic losses associated with
# extreme weather and climate events.
#
# The dataset includes losses caused by meteorological, hydrological,
# and climatological events. In this project the total losses across
# event types are used rather than analysing each event category separately.


# Variable used in this study
# LOSS – Total climate-related economic losses
#
# The dataset aggregates losses from:
# - meteorological events
# - hydrological events
# - climatological events


# Geographic coverage
# EU-27 countries


# Temporal coverage
# Annual data
# 1990–2024


# Unit of measurement
# Million euro at constant prices (2022)


# Data preparation
# A customised dataset was downloaded from the Eurostat database
# including EU-27 countries and total annual climate-related losses.
#
# Before importing the data into R, non-analytical information
# (e.g. metadata rows, notes, or summary information) was removed
# in Excel to simplify the structure and avoid import issues
# during the data processing stage.


################################################################################
# Import dataset
################################################################################

library(readxl)
library(dplyr)

Climate_loss <- read_excel(
  "D:/1.24 RQ 1/DATASETS/Original/cli_iad_loss__custom_20425725_spreadsheet.xlsx"
)

# Result
# Climate_loss → annual climate-related economic losses
# measured in million euro (constant 2022 prices)



################################################################################
# Data cleaning and preparation
################################################################################

# Rename first column to Country
names(Climate_loss)[1] <- "Country"

# Keep only country and year columns
Climate_loss_clean <- Climate_loss[, c("Country", as.character(1990:2024))]

# Remove the metadata row
Climate_loss_clean <- Climate_loss_clean[
  Climate_loss_clean$Country != "GEO (Labels)", ]

# Define year columns
year_columns <- as.character(1990:2024)

# Convert values to numeric and replace missing values with 0
Climate_loss_clean[year_columns] <- lapply(
  Climate_loss_clean[year_columns],
  function(x) {
    x <- as.numeric(x)
    x[is.na(x)] <- 0
    x
  }
)

# Convert unit from million euro to thousand euro
Climate_loss_clean[year_columns] <- lapply(
  Climate_loss_clean[year_columns],
  function(x) x * 1000
)

# Prevent scientific notation
options(scipen = 999)

# Calculate average annual loss
Climate_loss_clean$`Average Loss (thousand Euro)` <- rowMeans(
  Climate_loss_clean[year_columns],
  na.rm = TRUE
)

# Check result
dim(Climate_loss_clean)
names(Climate_loss_clean)
head(Climate_loss_clean)
View(Climate_loss_clean)


################################################################################
# 1.4-Dataset 4: Population
################################################################################

# Source
# Eurostat – Population change: demographic balance and crude rates
# Dataset: demo_gind
# https://ec.europa.eu/eurostat/databrowser/view/demo_gind
#
# Metadata:
# https://ec.europa.eu/eurostat/cache/metadata/en/demo_gind_esms.htm


# Description
# This dataset reports demographic indicators related to population change
# at the national level for European countries.
#
# In this study the indicator "Population on 1 January – total" is used.
# This measure represents the resident population at the beginning of each
# year and is commonly used in demographic and economic analyses because
# it provides a consistent reference point for annual comparisons.


# Variable used in this study
# JAN – Population on 1 January (total population)


# Geographic coverage
# EU-27 countries


# Temporal coverage
# Annual data
# 1990–2024


# Unit of measurement
# Number of persons


# Data preparation
# A customised dataset was downloaded from the Eurostat database including
# EU-27 countries and annual population values measured on 1 January.
#
# Population measured on 1 January is commonly considered the most suitable
# reference population in demographic statistics because it represents the
# resident population at the start of the year and allows consistent
# comparison across years.
#
# Before importing the dataset into R, non-analytical information
# (e.g. metadata rows, notes, or summary rows) was removed in Excel
# to simplify the dataset structure and avoid import issues during
# the data processing stage.


################################################################################
# Import dataset
################################################################################

library(readxl)
library(dplyr)

Population <- read_excel(
  "D:/1.24 RQ 1/DATASETS/Original/demo_gind__custom_20426124_spreadsheet.xlsx"
)

# Result
# Population → annual population on 1 January for each EU-27 country



################################################################################
# Data cleaning and preparation
################################################################################

# Rename first column to Country
names(Population)[1] <- "Country"

# Keep only country and year columns
Population_clean <- Population[, c("Country", as.character(1990:2024))]

# Remove metadata row
Population_clean <- Population_clean[
  Population_clean$Country != "GEO (Labels)", ]

# Define year columns
year_columns <- as.character(1990:2024)

# Convert values to numeric and replace missing values with NA
Population_clean[year_columns] <- lapply(
  Population_clean[year_columns],
  function(x) {
    x <- as.numeric(x)
    x
  }
)

# Prevent scientific notation
options(scipen = 999)

# Calculate average population across available years
Population_clean$Average_population <- rowMeans(
  Population_clean[year_columns],
  na.rm = TRUE
)

# Check result
dim(Population_clean)
names(Population_clean)
head(Population_clean)
View(Population_clean)





################################################################################
# 1.5-Dataset 5: Mean temperature
################################################################################

# Source
# Climate-ADAPT / European Climate Data Explorer
# Indicator: Mean temperature
# https://climate-adapt.eea.europa.eu/en/metadata/indicators/mean-temperature


# Description
# This dataset reports mean temperature values for European countries.
#
# In this project, mean temperature is used as a long-term background
# climate variable rather than as a year-specific explanatory variable.
# The selected time horizon is 1991–2020, which does not perfectly match
# the full study period. However, because the dataset provides long-term
# average values rather than annual observations, a direct year-by-year
# alignment is not possible.
#
# This may introduce some bias, but for the purpose of this project the
# variable is considered acceptable as a broad climatic background condition.


# Variable used in this study
# Mean temperature (°C)


# Geographic coverage
# EU-27 countries


# Temporal coverage
# 1991–2020 average


# Unit of measurement
# Degree Celsius (°C)


# Data preparation
# The downloaded file contains country codes rather than country names.
# Therefore, country codes are converted into country names after import.
#
# Before analysis, only the EU-27 countries are retained.
# The temperature column is renamed to improve clarity.


################################################################################
# Import dataset
################################################################################

library(readr)
library(dplyr)

Temperature <- read_csv(
  "D:/1.24 RQ 1/DATASETS/Original/01_mean_temperature-yearly-rcp_4_5-nuts_0-1991-2020-actual.csv",
  show_col_types = FALSE
)

# Result
# Temperature → mean temperature values by country code


################################################################################
# Data cleaning and preparation
################################################################################

# Rename columns
Temperature <- Temperature %>%
  rename(
    Country = region_id,
    `temp (℃)` = `Mean Temperature (°C) - RCP4.5 1991-2020`
  )

# Keep only EU-27 country codes
eu27_codes <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
  "DE","EL","HU","IE","IT","LV","LT","LU","MT","NL",
  "PL","PT","RO","SK","SI","ES","SE"
)

Temperature_clean <- Temperature %>%
  filter(Country %in% eu27_codes)

# Convert country codes to country names
Temperature_clean$Country <- recode(
  Temperature_clean$Country,
  "AT"="Austria","BE"="Belgium","BG"="Bulgaria","HR"="Croatia",
  "CY"="Cyprus","CZ"="Czechia","DK"="Denmark","EE"="Estonia",
  "FI"="Finland","FR"="France","DE"="Germany","EL"="Greece",
  "HU"="Hungary","IE"="Ireland","IT"="Italy","LV"="Latvia",
  "LT"="Lithuania","LU"="Luxembourg","MT"="Malta","NL"="Netherlands",
  "PL"="Poland","PT"="Portugal","RO"="Romania","SK"="Slovakia",
  "SI"="Slovenia","ES"="Spain","SE"="Sweden"
)

# Keep only two decimal places
Temperature_clean$`temp (℃)` <- round(
  Temperature_clean$`temp (℃)`, 2
)

# Check result
dim(Temperature_clean)
names(Temperature_clean)
head(Temperature_clean)
View(Temperature_clean)





################################################################################
# 1.6-Dataset 6: River length and river density
################################################################################

# Source
# This dataset was not directly downloaded from an official tabular database.
# Existing online datasets on river length were found to have several
# limitations for the purpose of this project, including insufficient
# comparability and limited suitability for the selected country-level analysis.
#
# Therefore, river length values were calculated in ArcGIS and then recorded
# manually in an Excel table for further analysis.
#
# The corresponding spatial information and visualisation can be checked
# in the StoryMap web map created for this project.


# Description
# This dataset contains country-level river length and river density values
# for the EU-27 countries.
#
# River length represents the total length of mapped rivers in each country.
# River density represents river length per square kilometre of national land area.


# Variables used in this study
# River Len.  → total river length
# River Density (km per km²) → river length per square kilometre of land area


# Geographic coverage
# EU-27 countries


# Temporal coverage
# Static dataset
#
# This dataset is treated as a background spatial characteristic rather
# than a time-varying annual variable.


# Unit of measurement
# River Len. → kilometre (km)
# River Density (km per km²) → kilometre of river length per square kilometre of land area


# Data preparation
# River length values were derived from ArcGIS calculations and entered
# into an Excel file for use in the analysis workflow.
#
# Missing values are recorded as NA.
#
# The column names are kept as they appear in the Excel table in order
# to maintain consistency with the manually prepared dataset.


################################################################################
# Import dataset
################################################################################

library(readxl)
library(dplyr)

River <- read_excel(
  "D:/1.24 RQ 1/DATASETS/Original/River length and density.xlsx"
)

# Result
# River → country-level river length and river density values


################################################################################
# Data cleaning and preparation
################################################################################

# Replace empty cells with NA if necessary
River_clean <- River %>%
  mutate(across(everything(), ~ ifelse(. == "", NA, .)))

# Data preparation
# The original Excel table contained column names with the symbol "km²".
# Special characters such as "²" may cause column selection problems
# in R due to character encoding.
#
# Therefore, the column names were standardised by replacing "km²"
# with "km2", resulting in the variables:
#
# River_Length_km
# River_Density_km_per_km2
#
# Countries without available river data (e.g. Denmark, Cyprus, Malta)
# were assigned NA values.
names(River_clean)

River_clean <- River_clean %>%
  rename(
    River_Length_km = `River Length(km)`,
    River_Density_km_per_km2 = `River Density(km per k㎡)`
  )

# Check result
dim(River_clean)
names(River_clean)
head(River_clean)
View(River_clean)




################################################################################
# 1.7-Dataset 7: GDP per capita and merged data
################################################################################

# =========================================================
# Define yearly columns
# =========================================================
year_columns <- as.character(1990:2024)


################################################################################
# Create yearly GDP per capita dataset
################################################################################

# GDP is already expressed in thousand Euro
# Population is expressed as number of persons
# GDP per capita is therefore calculated as:
# thousand Euro per person

GDP_per_capita <- GDP %>%
  select(Country, all_of(year_columns))

# Match population rows with GDP rows using Country names
Population_for_pc <- Population_clean %>%
  select(Country, all_of(year_columns)) %>%
  slice(match(GDP_per_capita$Country, Country))

# Calculate yearly GDP per capita
GDP_per_capita[year_columns] <- Map(
  function(gdp_col, pop_col) as.numeric(gdp_col) / as.numeric(pop_col),
  GDP_per_capita[year_columns],
  Population_for_pc[year_columns]
)


################################################################################
# Calculate average GDP per capita (1990–2024)
################################################################################

GDP_per_capita$`Average GDP per capita(thousand euro per person)` <- rowMeans(
  GDP_per_capita[year_columns],
  na.rm = TRUE
)

# Check dataset
dim(GDP_per_capita)
names(GDP_per_capita)
head(GDP_per_capita)
View(GDP_per_capita)


################################################################################
# Prepare summary tables for merging
################################################################################

# Average climate losses
Loss_summary <- Climate_loss_clean %>%
  select(Country, `Average Loss (thousand Euro)`)

# Average GDP
GDP_summary <- GDP %>%
  select(Country, `Average GDP(thousand Euro)`)

# Average sealed area
Sealed_area_summary <- Sealed_Area_clean %>%
  select(Country, `Average_Seal_Area(km2)`) %>%
  rename(`Sealed area(km2)` = `Average_Seal_Area(km2)`)

# Average land size
Land_size_summary <- Land_size_clean %>%
  select(Country, `Average_Land_Size(km2)`) %>%
  rename(`Land size(km2)` = `Average_Land_Size(km2)`)

# Average population
Population_summary <- Population_clean %>%
  select(Country, Average_population) %>%
  rename(`Average Population` = Average_population)

# Average GDP per capita
GDP_pc_summary <- GDP_per_capita %>%
  select(Country, `Average GDP per capita(thousand euro per person)`)

# Temperature dataset
Temperature_summary <- Temperature_clean %>%
  select(Country, `temp (℃)`)

# River dataset (already cleaned)
River_summary <- River_clean %>%
  select(Country, River_Length_km, River_Density_km_per_km2)


################################################################################
# Define EU-27 country list
################################################################################

EU27 <- c(
  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia",
  "Denmark","Estonia","Finland","France","Germany","Greece",
  "Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg",
  "Malta","Netherlands","Poland","Portugal","Romania","Slovakia",
  "Slovenia","Spain","Sweden"
)


################################################################################
# 1.8-Merge all datasets
################################################################################

# All datasets are merged using Country names.
# This avoids problems caused by different country orders.

merged_data <- data.frame(Country = EU27) %>%
  left_join(Loss_summary, by="Country") %>%
  left_join(GDP_summary, by="Country") %>%
  left_join(Sealed_area_summary, by="Country") %>%
  left_join(Population_summary, by="Country") %>%
  left_join(GDP_pc_summary, by="Country") %>%
  left_join(River_summary, by="Country") %>%
  left_join(Temperature_summary, by="Country") %>%
  left_join(Land_size_summary, by="Country")


################################################################################
# Calculate derived indicators
################################################################################

merged_data <- merged_data %>%
  mutate(
    
    # Climate loss per sealed surface
    `Loss per sealed area(thousand euro per km2)` =
      `Average Loss (thousand Euro)` / `Sealed area(km2)`,
    
    # Population density
    `Population density(per km2)` =
      `Average Population` / `Land size(km2)`,
    
    # Climate loss relative to GDP
    `Loss as share of GDP` =
      `Average Loss (thousand Euro)` / `Average GDP(thousand Euro)`
  )


################################################################################
# Reorder variables for final dataset
################################################################################

merged_data <- merged_data %>%
  select(
    Country,
    `Average Loss (thousand Euro)`,
    `Average GDP(thousand Euro)`,
    `Sealed area(km2)`,
    `Loss per sealed area(thousand euro per km2)`,
    `Population density(per km2)`,
    `Average Population`,
    `Average GDP per capita(thousand euro per person)`,
    River_Length_km,
    `temp (℃)`,
    `Loss as share of GDP`,
    `Land size(km2)`,
    River_Density_km_per_km2
  )


################################################################################
# Optional rounding for readability
################################################################################

merged_data <- merged_data %>%
  mutate(
    `Average Loss (thousand Euro)` = round(`Average Loss (thousand Euro)`,3),
    `Average GDP(thousand Euro)` = round(`Average GDP(thousand Euro)`,2),
    `Sealed area(km2)` = round(`Sealed area(km2)`,2),
    `Loss per sealed area(thousand euro per km2)` =
      round(`Loss per sealed area(thousand euro per km2)`,6),
    `Population density(per km2)` =
      round(`Population density(per km2)`,0),
    `Average Population` =
      round(`Average Population`,0),
    `Average GDP per capita(thousand euro per person)` =
      round(`Average GDP per capita(thousand euro per person)`,6),
    River_Length_km =
      round(River_Length_km,2),
    `temp (℃)` =
      round(`temp (℃)`,2),
    `Loss as share of GDP` =
      round(`Loss as share of GDP`,6),
    `Land size(km2)` =
      round(`Land size(km2)`,2),
    River_Density_km_per_km2 =
      round(River_Density_km_per_km2,4)
  )


################################################################################
# Final dataset check
################################################################################

dim(merged_data)
names(merged_data)
head(merged_data)
View(merged_data)


################################################################################
# Common plot style
################################################################################

main_fill  <- "#6FA8B6"
main_point <- "#5F9EA0"
main_line  <- "#2C6DA4"
smooth_col <- "#6FB2A2"

installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(tidyverse)
library(readxl)
library(writexl)
library(ggrepel)
library(scales)
library(ggplot2)

packages <- c(
  "tidyverse",
  "readxl",
  "writexl",
  "ggrepel",
  "scales"
)

common_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

################################################################################
# 3-Plots and analysis based on merged_data
################################################################################


installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(tidyverse)
library(readxl)
library(writexl)
library(ggrepel)
library(scales)


################################################################################
# Check merged dataset
################################################################################

dim(merged_data)
names(merged_data)
head(merged_data)
View(merged_data)

################################################################################
# 2.1-Average climate-related loss per sealed area by country
################################################################################

# Prepare data for plotting
plot_data <- merged_data %>%
  arrange(desc(`Loss per sealed area(thousand euro per km2)`)) %>%
  mutate(Country = factor(Country, levels = rev(Country)))

# Create plot
p_loss_sealed <- ggplot(
  plot_data,
  aes(
    x = `Loss per sealed area(thousand euro per km2)`,
    y = Country
  )
) +
  geom_col(
    fill = "#6FA8B6",
    width = 0.72
  ) +
  labs(
    title = "Average climate-related loss per sealed area across EU-27 countries",
    x = "Average loss per sealed area (thousand euro per km2)",
    y = NULL,
    caption = "Indicator calculated as average climate-related economic loss divided by average sealed area."
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::label_number(accuracy = 1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
    axis.text.y = element_text(size = 10, color = "grey20"),
    axis.text.x = element_text(size = 10, color = "grey30"),
    axis.title.x = element_text(size = 11, color = "grey20"),
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
    plot.margin = margin(10, 15, 10, 10)
  )

# Print plot
print(p_loss_sealed)

# Save plot
ggsave(
  filename = "Average_loss_per_sealed_area_by_country.png",
  plot = p_loss_sealed,
  path = "D:/1.24 RQ 1/DATASETS/Original",
  width = 10,
  height = 8,
  dpi = 300
)

################################################################################
# 2.2-Average climate-related loss as percentage of GDP by country
################################################################################

# Prepare data for plotting
plot_data_share <- merged_data %>%
  mutate(`Loss as percentage of GDP (%)` = `Loss as share of GDP` * 100) %>%
  arrange(desc(`Loss as percentage of GDP (%)`)) %>%
  mutate(Country = factor(Country, levels = rev(Country)))

# Create plot
p_loss_share <- ggplot(
  plot_data_share,
  aes(
    x = `Loss as percentage of GDP (%)`,
    y = Country
  )
) +
  geom_col(
    fill = "#6FA8B6",
    width = 0.72
  ) +
  labs(
    title = "Average climate-related loss as percentage of GDP by country",
    x = "Average loss as percentage of GDP (%)",
    y = NULL,
    caption = "Indicator calculated as average climate-related economic loss divided by average GDP and expressed as percentage."
  ) +
  scale_x_continuous(
    limits = c(0, 1.5),
    breaks = seq(0, 1.5, by = 0.5),
    minor_breaks = seq(0, 1.5, by = 0.25),
    expand = expansion(mult = c(0, 0)),
    labels = function(x) paste0(x, "%")
  ) +
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

# Print plot
print(p_loss_share)

# Save plot
ggsave(
  filename = "Average_loss_percentage_of_GDP_by_country.png",
  plot = p_loss_share,
  path = "D:/1.24 RQ 1/DATASETS/Original",
  width = 10,
  height = 8,
  dpi = 300
)

################################################################################
# 2.3-Correlation tests
################################################################################

cor.test(
  merged_data$`Loss per sealed area(thousand euro per km2)`,
  merged_data$`temp (℃)`
)

cor.test(
  merged_data$`Loss per sealed area(thousand euro per km2)`,
  merged_data$`Population density(per km2)`
)

cor.test(
  merged_data$`Loss per sealed area(thousand euro per km2)`,
  merged_data$River_Density_km_per_km2,
  use = "complete.obs"
)

cor.test(
  merged_data$`Loss per sealed area(thousand euro per km2)`,
  merged_data$`Average GDP per capita(thousand euro per person)`
)

################################################################################
# 2.4-Scatter plot: loss per sealed area vs temperature
################################################################################

p_temp <- ggplot(
  merged_data,
  aes(
    x = `temp (℃)`,
    y = `Loss per sealed area(thousand euro per km2)`
  )
) +
  geom_point(
    size = 2.6,
    color = "#3A9AB2FF",
    alpha = 0.9
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#333333",
    linewidth = 0.9
  ) +
  labs(
    title = "Relationship between temperature and loss per sealed area",
    x = "Average temperature (°C)",
    y = "Loss per sealed area (thousand euro per km2)",
    caption = "Each point represents one EU-27 country."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    axis.text = element_text(size = 10, color = "grey30"),
    axis.title = element_text(size = 11, color = "grey20"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0)
  )

print(p_temp)

ggsave(
  filename = "loss_per_sealed_area_vs_temperature.png",
  plot = p_temp,
  path = "D:/1.24 RQ 1/DATASETS/Original",
  width = 10,
  height = 8,
  dpi = 300
)

################################################################################
# 2.5-Scatter plot: loss per sealed area vs population density
################################################################################

p_pop <- ggplot(
  merged_data,
  aes(
    x = `Population density(per km2)`,
    y = `Loss per sealed area(thousand euro per km2)`
  )
) +
  geom_point(size = 3, color = "#5F9EA0") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Loss per sealed area vs population density",
    x = "Population density (per km2)",
    y = "Loss per sealed area (thousand euro per km2)"
  ) +
  theme_minimal(base_size = 14)

print(p_pop)

ggsave(
  filename = "loss_per_sealed_area_vs_population_density.png",
  plot = p_pop,
  path = "D:/1.24 RQ 1/DATASETS/Original",
  width = 10,
  height = 8,
  dpi = 300
)


################################################################################
# 2.6-Scatter plot: loss per sealed area vs river density
################################################################################

p_river <- ggplot(
  merged_data,
  aes(
    x = River_Density_km_per_km2,
    y = `Loss per sealed area(thousand euro per km2)`
  )
) +
  geom_point(size = 3, color = "#5F9EA0") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Loss per sealed area vs river density",
    x = "River density (km per km2)",
    y = "Loss per sealed area (thousand euro per km2)"
  ) +
  theme_minimal(base_size = 14)

print(p_river)

ggsave(
  filename = "loss_per_sealed_area_vs_river_density.png",
  plot = p_river,
  path = "D:/1.24 RQ 1/DATASETS/Original",
  width = 10,
  height = 8,
  dpi = 300
)


################################################################################
# 2.7-Scatter plot: loss per sealed area vs GDP per capita
################################################################################

p_gdp <- ggplot(
  merged_data,
  aes(
    x = `Average GDP per capita(thousand euro per person)`,
    y = `Loss per sealed area(thousand euro per km2)`
  )
) +
  geom_point(size = 3, color = "#5F9EA0") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Loss per sealed area vs GDP per capita",
    x = "Average GDP per capita (thousand euro per person)",
    y = "Loss per sealed area (thousand euro per km2)"
  ) +
  theme_minimal(base_size = 14)

print(p_gdp)

ggsave(
  filename = "loss_per_sealed_area_vs_gdp_per_capita.png",
  plot = p_gdp,
  path = "D:/1.24 RQ 1/DATASETS/Original",
  width = 10,
  height = 8,
  dpi = 300
)


################################################################################
# 2.8-Export merged dataset
################################################################################

write_xlsx(
  merged_data,
  "D:/1.24 RQ 1/DATASETS/Original/final_country_dataset_merged.xlsx"
)


################################################################################
# 2.9-Country groups by GDP per capita and loss per sealed area (median split)
################################################################################

pal <- c(
  "#3A9AB2FF", "#6FB2C1FF", "#91BAB6FF", "#A5C2A3FF",
  "#BDC881FF", "#DCCB4EFF", "#E3B710FF", "#E79805FF",
  "#EC7A05FF", "#EE5703FF", "#F11B00FF"
)

x_var <- "Average GDP per capita(thousand euro per person)"
y_var <- "Loss per sealed area(thousand euro per km2)"

x_med <- median(merged_data[[x_var]], na.rm = TRUE)
y_med <- median(merged_data[[y_var]], na.rm = TRUE)

plot_df <- merged_data %>%
  mutate(
    GDP_group = ifelse(.data[[x_var]] >= x_med, "High GDP per capita", "Low GDP per capita"),
    Loss_group = ifelse(.data[[y_var]] >= y_med, "High loss per sealed area", "Low loss per sealed area"),
    Quadrant = case_when(
      Loss_group == "High loss per sealed area" & GDP_group == "Low GDP per capita"  ~ "High loss – Low GDP",
      Loss_group == "High loss per sealed area" & GDP_group == "High GDP per capita" ~ "High loss – High GDP",
      Loss_group == "Low loss per sealed area"  & GDP_group == "Low GDP per capita"  ~ "Low loss – Low GDP",
      Loss_group == "Low loss per sealed area"  & GDP_group == "High GDP per capita" ~ "Low loss – High GDP"
    )
  )

quad_cols <- c(
  "High loss – Low GDP"  = pal[11],
  "High loss – High GDP" = pal[8],
  "Low loss – Low GDP"   = pal[4],
  "Low loss – High GDP"  = pal[1]
)

plot_df$Quadrant <- factor(
  plot_df$Quadrant,
  levels = c("High loss – Low GDP", "High loss – High GDP", "Low loss – Low GDP", "Low loss – High GDP")
)

p_quad <- ggplot(
  plot_df,
  aes(x = .data[[x_var]], y = .data[[y_var]], color = Quadrant)
) +
  geom_vline(xintercept = x_med, linetype = "dashed", linewidth = 0.7, color = "grey45") +
  geom_hline(yintercept = y_med, linetype = "dashed", linewidth = 0.7, color = "grey45") +
  geom_point(size = 3.2, alpha = 0.95) +
  ggrepel::geom_text_repel(
    aes(label = Country),
    size = 3.6,
    max.overlaps = Inf,
    box.padding = 0.35,
    point.padding = 0.25,
    seed = 42
  ) +
  scale_color_manual(values = quad_cols) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.001)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
  labs(
    title = "Country groups by GDP per capita and loss per sealed area (median split)",
    x = "Average GDP per capita (thousand euro per person)",
    y = "Average climate-related loss per sealed area (thousand euro per km2)",
    color = "Group",
    caption = "Dashed lines show median splits across countries."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

print(p_quad)

ggsave(
  filename = "country_groups_gdp_per_capita_and_loss_per_sealed_area.png",
  plot = p_quad,
  path = "D:/1.24 RQ 1/DATASETS/Original",
  width = 11,
  height = 8,
  dpi = 300
)

################################################################################
# 2.10-Calculate annual total economic losses in Europe
################################################################################

year_columns <- as.character(1990:2024)

loss_europe <- Climate_loss_clean %>%
  select(all_of(year_columns)) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Total_Loss"
  ) %>%
  mutate(Year = as.numeric(Year))

################################################################################
# Plot annual total economic losses in Europe
################################################################################

p_total_loss <- ggplot(
  loss_europe,
  aes(x = Year, y = Total_Loss)
) +
  geom_line(
    color = "#2C6DA4",
    linewidth = 0.9
  ) +
  geom_point(
    color = "#2C6DA4",
    size = 2.2
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "#AEC6CF",
    linewidth = 1.1
  ) +
  labs(
    title = "Annual total climate-related economic losses in Europe",
    x = "Year",
    y = "Total economic loss (thousand euro)",
    caption = "Values represent the annual sum across EU-27 countries."
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    axis.text = element_text(size = 10, color = "grey30"),
    axis.title = element_text(size = 11, color = "grey20"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p_total_loss)

ggsave(
  filename = "annual_total_economic_losses_europe.png",
  plot = p_total_loss,
  path = "D:/1.24 RQ 1/DATASETS/Original",
  width = 11,
  height = 8,
  dpi = 300
)