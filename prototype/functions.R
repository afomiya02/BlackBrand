library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
# setwd("C:/Users/afomiyaa/Desktop/testrepo/prototype")

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
    mutate_all(str_replace_all, " |[()]|,", "") %>%
    mutate_all(str_trim) %>%
    filter(label %in% c("total_population", "median_age_years", "black_or_african_american")) %>%
    slice(c(1, 2, 6)) %>% # duplicate 
    mutate(loc = str_extract(filename, "(?<=/)[^.]+(?=\\.)"), .before = 1) %>%
    select(-c(moe, pct_estimate, pct_moe)) %>% # Remove unnecessary columns 
    mutate(estimate = as.numeric(estimate)) %>% # make numeric data numeric
  
  # Reshape the data
  df <- pivot_wider(data = df, names_from = label, values_from = estimate)
  
  return(df)
}

# Creates a basic leaflet heatmap with all Hampton Roads data
# hoping this can work with leaflet piping
#
# Returns:
#   Preprocessed Hampton Roads grographical data
preprocess_geo_data <- function() {
  df <- readRDS('./data/geo_data.rds') %>%
    # preprocess column data for merging
    mutate(loc_name = str_replace_all(loc_name, ", Virginia", "")) %>%
    mutate(loc_name = str_replace_all(loc_name, "city", "")) %>%
    mutate(loc_name = tolower(loc_name)) %>%
    mutate(loc_name = str_trim(loc_name)) %>%
    mutate(loc_name = str_replace_all(loc_name, " ", "_")) %>%
    st_transform(st_crs("WGS84")) # warning popped up asking for this transformation so KEEP it
  return(df)
}

# Test the function
filenames <- list.files("data", pattern = "*.csv", full.names = TRUE)
race_age_data <- lapply(filenames, get_demographics)
race_age_data <- bind_rows(race_age_data)

# Print the result
print(race_age_data)

geo_data <- preprocess_geo_data()

heatmap_data <- merge(geo_data, race_age_data, by.x = "loc_name", by.y = "loc") %>%
  # postprocess merged data on heatmap
  mutate(loc_name = str_replace_all(loc_name, "_", " ")) %>%
  mutate(loc_name = str_to_title(loc_name)) %>%
  mutate(pct_black = round(black_or_african_american / total_population, 3) * 100, .before = geometry)

pal <- colorBin("YlOrRd", heatmap_data$pct_black)
leaflet() %>%
  addPolygons(
    data = heatmap_data,
    fillColor = pal(heatmap_data$pct_black),
    color = "black",
    weight = 1,
    fillOpacity = 0.75,
    popup = paste("<h1>", heatmap_data$loc, "</h1>",
      "<b>Median Age (years):</b>", heatmap_data$median_age_years,
      "<br><b>Black Population (%):</b>", heatmap_data$pct_black,
      "<br><b>Total Population:</b>", heatmap_data$total_population
    )
  ) %>%
  addLegend("bottomright",
            pal = pal,
            values = heatmap_data$pct_black,
            title = "Black Population (%)") %>%
  addTiles()
