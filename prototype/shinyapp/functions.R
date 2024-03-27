library(tidyverse)
library(sf)
# set working directory in 
# RStudio > Session > Set Working Directory > To Source File Location

## ALL RACE & AGE DEMOGRAPHICS FOUND IN THE 5-YEAR DP05 TABLE 
## IN THE AMERICAN CENSUS SURVEY TABLES

# Extracts all necessary demographics info for the race/age heatmap
# 
# Params: 
#   filename - .csv file to be processed
# Returns: 
#   small dataframe with the locality, age and race estimates/percentages
preprocess_sodem_data <- function(filename) {
  df <- read.csv(filename) %>%
    `colnames<-`(c("label", "estimate", "moe", "pct_estimate", "pct_moe")) %>%
    mutate_all(str_trim) %>%# trim whitespace (tolower gave off weird symbols to whitespace)
    mutate(., across(.cols = everything(), tolower)) %>%
    mutate_all(str_replace_all, " ", "_") %>%
    mutate_all(str_replace_all, " |[()]|,", "") %>%
    filter(label %in% c("total_population", "median_age_years", "black_or_african_american")) %>%
    slice(c(1, 2, 6)) %>% # duplicate 
    mutate(loc = gsub("\\..*", "", basename(filename)), .before = 1) %>%
    select(-c(moe, pct_estimate, pct_moe)) %>% # Remove unnecessary columns 
    mutate(estimate = as.numeric(estimate)) # make numeric data numeric
  
  # Reshape the data
  df <- pivot_wider(data = df, names_from = label, values_from = estimate)
}

# Creates a basic leaflet heatmap with all Hampton Roads data
# hoping this can work with leaflet piping
#
# Returns:
#   Preprocessed Hampton Roads grographical data
preprocess_geo_data <- function(path) {
  df <- readRDS(path) %>%
    # preprocess column data for merging
    mutate(loc_name = str_replace_all(loc_name, ", Virginia", "")) %>%
    mutate(loc_name = str_replace_all(loc_name, "city", "")) %>%
    mutate(loc_name = tolower(loc_name)) %>%
    mutate(loc_name = str_trim(loc_name)) %>%
    mutate(loc_name = str_replace_all(loc_name, " ", "_")) %>%
    st_transform(st_crs("WGS84")) # warning popped up asking for this transformation so KEEP it
  return(df)
}