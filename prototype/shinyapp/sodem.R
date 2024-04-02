library(readxl)
library(tidyverse)
library(sf)
# set working directory in 
# RStudio > Session > Set Working Directory > To Source File Location

## This file processes all sociodemographic data for the dashboard

# A vectorized list of all localities in Hampton Roads
# lowercase and with underscores for data cleaning purposes
hampton_roads_localities <- c(
  "chesapeake",
  "franklin",
  "gloucester_county",
  "hampton",
  "isle_of_wight_county",
  "james_city_county",
  "mathews_county",
  "newport_news",
  "norfolk",
  "poquoson",
  "portsmouth",
  "southampton_county",
  "suffolk",
  "virginia_beach",
  "williamsburg",
  "york_county"
)

## ALL RACE & AGE DEMOGRAPHICS FOUND IN THE 5-YEAR DP05 TABLE 
## IN THE AMERICAN CENSUS SURVEY TABLES

## ALL EDUCATIONAL DATA FOUND IN THE VDOE STATISTICS TAB
## https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304 FOR STUDENT DATA
## https://www.doe.virginia.gov/teaching-learning-assessment/teaching-in-virginia/education-workforce-data-reports FOR EDUCATOR DATA


# Preprocesses all sociodemographic data for grouping purposes
# 
# Params: 
#   path - .csv file to be processed
# Returns: 
#   small dataframe with the locality, age and race estimates/percentages
preprocess_sodem_data <- function(path) {
  df <- read.csv(path) %>%
    `colnames<-`(c("label", "estimate", "moe", "pct_estimate", "pct_moe")) %>%
    mutate_all(str_trim) %>%# trim whitespace (tolower gave off weird symbols to whitespace)
    mutate(., across(.cols = everything(), tolower)) %>%
    mutate_all(str_replace_all, " ", "_") %>%
    mutate_all(str_replace_all, " |[()]|,", "") %>%
    filter(label %in% c("total_population", "median_age_years", "black_or_african_american")) %>%
    slice(c(1, 2, 6)) %>% # duplicate 
    mutate(loc = gsub("\\..*", "", basename(path)), .before = 1) %>%
    select(-c(moe, pct_estimate, pct_moe)) %>% # Remove unnecessary columns 
    mutate(estimate = as.numeric(estimate)) # make numeric data numeric
  
  # Reshape the data
  df <- pivot_wider(data = df, names_from = label, values_from = estimate)
}

# Preprocesses geographic data for grouping
#
# Params: 
#   path - .rds file to be processed
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

# gets all 2022 ACS DP05 data for median age + black population choropleth
# check functions.R for more info
filenames <- list.files("data/ACS_DP05", pattern = "*.csv", full.names = TRUE)
sodem_data <- lapply(filenames, preprocess_sodem_data)
sodem_data <- bind_rows(sodem_data)

# gets preprocessed geo data for hampton roads
# check functions.R for more info
geo_data <- preprocess_geo_data("data/geo_data.rds")

# create choropleth data for leaflet creation
heatmap_data <-
    merge(geo_data, sodem_data, by.x = "loc_name", by.y = "loc") %>%
    # postprocess merged data on heatmap
    mutate(loc_name = str_replace_all(loc_name, "_", " ")) %>%
    mutate(loc_name = str_to_title(loc_name)) %>%
    mutate(
        pct_black = round(black_or_african_american / total_population, 3) * 100,
        .before = geometry
    )