library(readxl)
library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(leaflet)
library(ggtext)
library(plotly)
setwd("C:/Users/Tim/Desktop/School/cmda4864/testrepo/testing")

## ALL RACE & AGE DEMOGRAPHICS FOUND IN THE 5-YEAR DP05 TABLE 
## IN THE AMERICAN CENSUS SURVEY TABLES

# Extracts all necessary demographics info for the race/age heatmap
# 
# Params: 
#   filename - .csv file to be processed
# Returns: 
#   small dataframe with the locality, age and race estimates/percentages
get_demographics <- function(filename) {
  df <- read.csv(filename) %>%
    `colnames<-`(c("label", "estimate", "moe", "pct_estimate", "pct_moe")) %>%
    mutate(., across(.cols = everything(), tolower)) %>%
    mutate_all(str_replace_all, " ", "_") %>%
    mutate_all(str_replace_all, " |[()]", "") %>%
    mutate_all(str_trim) %>%
    filter(label %in% c("total_population", "median_age_years", "black_or_african_american")) %>%
    slice(c(1, 2, 6)) %>%
    mutate(loc = str_extract(filename, "(?<=/)[^.]+(?=\\.)"), .before = 1) %>%
    select(-c(moe, pct_estimate, pct_moe)) # Remove unnecessary columns
  
  # Reshape the data
  df <- pivot_wider(data = df, names_from = label, values_from = estimate)
  
  return(df)
}

# Test the function
filenames <- list.files("data", pattern = "*.csv", full.names = TRUE)
race_age_data <- lapply(filenames, get_demographics)
race_age_data <- bind_rows(race_age_data)

# Print the result
print(race_age_data)
# figuring out heatmap stuff

geo_data <- readRDS('./data/geo_data.rds')
counties <- counties(state = "Virginia", cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(st_crs(geo_data))

ggplot(data = geo_data) + geom_sf()

leaflet() %>% # Add county polygons
  #addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = geo_data,
    fillColor = "red",
    color = "black",
    weight = 1,
    popup = paste(
      "<b>County:</b>", race_age_data$loc,
      "<br><b>Median Age:</b>", race_age_data$median_age_years,
      "<br><b>Number of Black People:</b>", race_age_data$black_or_african_american
    )
  )



# there's a likelihood you're gonna need to change the function because i believe that the 
# percent estimates for the black population are NOT numeric -- that's a simple fix

# try to create a heatmap for the race, if you can do age that's even better
# we just want a prototype that works to show blair and co. before tomorrow