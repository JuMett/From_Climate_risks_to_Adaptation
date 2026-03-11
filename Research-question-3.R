# ---
# Title:  "river restoration projects in detail"
# Date:   "2026-03-09"
# Author: "Julie Mettenbrink"
# For writing the code and solving errors partly luhki (LLM) was used
# ---

# Table of content

### Setting working environment
### 1. Introduction

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
# How do the river restoration measures reduce the climate-related economic losses
# caused by hydrological events?


## Data description

# In this script following data were used:

# 1. RESTORE projects (https://doi.org/10.17632/jkpjt6kkc6.1)
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




# Load and clean data and save data as tables: ----------------------------

# Load data
RESTORE <- read.csv(
  "C:/Users/cjuli/Documents/Uni/WiSe2526/DAVISET/Data/Results/Restore-projects_1990-2024.csv"
)

### Clean data

# Selecting countries with costs available
restore_costs <- RESTORE %>%
  dplyr::filter(!is.na(costs)) # 74 projects from 115

restore_area <- RESTORE %>%
  dplyr::filter(!is.na(area_size)) # 57 projects from 115

restore_costs_area <- RESTORE %>%
  dplyr::filter(!is.na(costs)) %>%
  dplyr::filter(!is.na(area_size)) # 29 projects from 115

# save table
write.csv(restore_costs_area, "Projects_with_costs&areasize.csv")

#################################################################################

# Analysing data ----------------------------------------------------------

restore_costs_area <- restore_costs_area %>%
  mutate(Unit_costs_of_river_restoration_projects_keur_ha = costs / (area_size *
                                                                       100)) #transform area_size in ha

# creating a boxplot similiar to Szałkiewicz et al.(2018:8)
restore_costs_area <- restore_costs_area %>%
  mutate(Year_Group = cut(
    starting_year,
    breaks = c(1990, 2000, 2005, 2010, 2015, 2024),
    include.lowest = TRUE,
    labels = c(
      "1990-2000",
      "2001-2005",
      "2006-2010",
      "2011-2015",
      "2016-2024"
    )
  )) %>%
  dplyr::filter(Year_Group != "2016-2024") %>% # we need to exclude this, because it is only one project and an great outlier
  group_by(Year_Group) %>%
  mutate(
    ProjectCount = n(),
    Q1 = quantile(
      Unit_costs_of_river_restoration_projects_keur_ha,
      0.25,
      na.rm = TRUE
    ),
    Q3 = quantile(
      Unit_costs_of_river_restoration_projects_keur_ha,
      0.75,
      na.rm = TRUE
    ),
    IQR = Q3 - Q1,
    Lower_bound = Q1 - 1.5 * IQR,
    Upper_bound = Q3 + 1.5 * IQR,
    Non_outlier = Unit_costs_of_river_restoration_projects_keur_ha >= Lower_bound &
      Unit_costs_of_river_restoration_projects_keur_ha <= Upper_bound
  ) %>%
  ungroup()

# Calculate means for all projects and outliers excluded
total_mean <- mean(restore_costs_area$Unit_costs_of_river_restoration_projects_keur_ha,
                   na.rm = TRUE)
non_outlier_mean <- mean(restore_costs_area$Unit_costs_of_river_restoration_projects_keur_ha[restore_costs_area$Non_outlier],
                         na.rm = TRUE)

# Count of outliers and non-outliers
outlier_counts <- restore_costs_area %>%
  group_by(Year_Group) %>%
  summarise(
    Total_Projects = n(),
    Non_outliers = sum(Non_outlier, na.rm = TRUE),
    Outliers = Total_Projects - Non_outliers
  )

print(outlier_counts)

# Create boxplot
boxplot <- ggplot(
  restore_costs_area,
  aes(x = Year_Group, y = Unit_costs_of_river_restoration_projects_keur_ha, fill = Year_Group)
) +
  geom_boxplot(
    data = subset(restore_costs_area, ProjectCount > 3),
    outlier.shape = 19,
    outlier.size = 2
  ) +
  geom_jitter(
    data = subset(restore_costs_area, ProjectCount <= 3),
    width = 0.2,
    height = 0,
    size = 2
  ) +
  geom_hline(yintercept = total_mean,
             color = "black",
             linetype = "solid") +
  geom_hline(yintercept = non_outlier_mean,
             color = "black",
             linetype = "dotted") +
  annotate(
    "text",
    x = Inf,
    y = total_mean,
    label = paste0(round(total_mean, 2), " (all projects included)"),
    color = "black",
    hjust = 1.1,
    vjust = 1,
    size = 3.5
  ) +
  annotate(
    "text",
    x = Inf,
    y = non_outlier_mean,
    label = paste0(round(non_outlier_mean, 2), " (outliers excluded)"),
    color = "black",
    hjust = 1.1,
    vjust = 1,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 500),
    breaks = seq(0, 500, by = 100),
    minor_breaks = seq(0, 500, by = 50)
  ) +
  scale_fill_manual(
    values = c(
      "1990-2000" = "#E8E79AFF",
      "2001-2005" = "#8CBF9AFF",
      "2006-2010" = "#5FA2A4FF",
      "2011-2015" = "#477B95FF"
    )
  ) +
  labs(
    x = "Starting Year (Groups of 5 years)",
    y = "Costs (K€ / ha)",
    fill = "",
    caption = "Average including outliers is 3,439 k€/ha, because one projects costs were 93,137.8 k€/ha; 
    Year after 2015 excluded, because only for one projects costs were available and it was an outlier",
    title = "River restoration project costs (k€) per hectare"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
    axis.text.y = element_text(size = 12), 
    axis.title.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14), 
    plot.title = element_text(size = 16, face = "bold"),   
    plot.caption = element_text(size = 10), 
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
  )

# show plot
print(boxplot)

# save plot
ggsave(
  "boxplot_rq3.png",
  plot = boxplot,
  width = 10,
  height = 8,
  dpi = 300
)