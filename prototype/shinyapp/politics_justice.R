library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(leaflet.minicharts)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(gridExtra)
library(stringr)
library(shinyjs)
library(plotly)
library(ggrepel)
library(shinydashboard)
library(mapdata)
library(plotrix)
library(scatterpie)
library(leafpop)
library(ggpubr)
library(viridis)
library(highcharter)
#library(rCharts)
library(terra)
library(geojsonio)
library(tigris)
library(dplyr)
library(lattice)
library(ggplot2)
library(forcats)
library(plotly)
library(formattable)
library(hrbrthemes)
library(bslib)
library(reshape2) 

# Traffic stops/Race and Jurisdiction/Toggle Jurisdictions
# Function to read traffic data
read_traffic_data <- function() {
  data <- read.csv("data/politics/traffic_stops/hampton_trafficstop.csv")
  return(data)
}

# City council demographics Race
# Function to read data
read_race <- function() {
  police_df <- read.csv('data/politics/city_council_demographics/hampton_roads_police_chief.csv')
  politicans_df <- read.csv('data/politics/city_council_demographics/hampton_roads_politicans.csv')
  return(list(police_df = police_df, politicans_df = politicans_df))
}

# City council demographics Gender
# Function to read data
read_gender <- function() {
  police_df <- read.csv('data/politics/city_council_demographics/hampton_roads_police_chief.csv')
  politicans_df <- read.csv('data/politics/city_council_demographics/hampton_roads_politicans.csv')
  return(list(police_df = police_df, politicans_df = politicans_df))
}

# Function to read jail data
read_jail_data <- function(choice) {
  if (choice == "Virginia") {
    # Read data for Virginia
    data <- read.csv('data/politics/incarceration/va_incarceration_trends.csv')
    title <- 'VA state'
  } else if (choice == "Hampton Roads") {
    # Read data for Hampton Roads
    data <- read.csv('data/politics/incarceration/va_hampton_roads_incarceration_trends.csv')
    title <- 'Hampton Roads'
  }
  return(list(data = data, title = title))
}


# Pie plots for jail & pop demographics in server.r

#Prison rates
# Function to read and process prison data
read_process_prison_data <- function(year) {
  # Read data from CSV files and RDS file
  va_hampton_roads_incarceration_trends <- read.csv('data/politics/incarceration/va_hampton_roads_incarceration_trends.csv')
  geo_data <- readRDS('./data/geo_data.rds')
  geo_data <- st_transform(geo_data)
  
  # Process geo_data to prepare for merging
  geo_data$loc_name <- str_to_lower(geo_data$loc_name)
  geo_data$loc_name <- word(geo_data$loc_name, 1) 
  
  # Process prison data for the selected year
  va_prison_plot_df <- va_hampton_roads_incarceration_trends %>%
    filter(year == year) %>%
    select(city_name, total_prison_adm_rate) %>%
    drop_na(total_prison_adm_rate) %>%
    arrange(desc(total_prison_adm_rate)) %>%
    mutate(
      loc_name = str_to_lower(city_name),
      loc_name = word(loc_name, 1),
      total_prison_adm_rate = ifelse(total_prison_adm_rate == 0, NA, total_prison_adm_rate)
    )
  
  # Merge prison data with geographic data
  merged_data <- merge(va_prison_plot_df, geo_data, by = 'loc_name')
  
  return(merged_data)
}

#Gentrification
load_preprocess_homevalue_data <- function() {
  # Load home value data
  data_jim_final <- read.csv("./data/data_jim_final.csv")
  data_jim_final <- na.omit(data_jim_final)
  
  # Preprocess home value data
  data_jim_final <- data_jim_final %>%  
    mutate_at("homevalue", funs(as.character(.))) %>%
    mutate_at("homevalue", funs(gsub(",", "", .))) %>%
    mutate_at("homevalue", funs(as.numeric(.)))
  
  # Create popup information
  data_jim_final <- data_jim_final %>% 
    mutate(popup_info = paste("Zipcode", zipcode, "<br/>", "Homevalue", homevalue, "<br/>", 
                              "% White Population", Percentofpopulation, "<br/>", 
                              "% Black Population", percent_of_population))
  
  return(data_jim_final)
}