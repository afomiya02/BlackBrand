library(readxl)
library(tidyverse)
library(sf)
library(tigris)
setwd("C:/Users/Tim/Desktop/School/cmda4864/testing")

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
    `colnames<-`(c("label", "estimate", "moe", "pct_estimate", "pct_moe")) %>% # rename column names for automation purpose
    mutate(., across(.cols = everything(), tolower)) %>% # make everything lowercase
    mutate_all(str_replace_all, " ", "_") %>% # make everything lowercase
    mutate_all(str_replace_all, " |[()]", "") %>% # remove parentheses
    mutate_all(str_trim) %>% # trim whitespace
    # NOTE: IF YOU WANT TO ADD MORE COLUMNS YOU NEED TO MANUALLY CHANGE THE COLUMN NAMES
    filter(label %in% c("total_population", "median_age_years", "black_or_african_american")) %>% # filter median age and % of blacks in population
    # MANUALLY UPDATE THIS TOO
    slice(c(1, 2, 6)) %>% # median age, % black (one or two races)
    mutate(loc = str_extract(filename, "(?<=/)[^.]+(?=\\.)"), .before = 1) # slices everything before .csv
  return(df)
}

# loop that merges all 16 hampton roads census data
filenames <- list.files("data", pattern = "*.csv", full.names = TRUE)
race_age_data <- list()
for (i in filenames) {
  temp <- get_demographics(i)
  race_age_data[[i]] <- temp
}
race_age_data <- bind_rows(race_age_data)


# figuring out heatmap stuff

geo_data <- readRDS('./data/geo_data.rds')
counties <- counties(state = "Virginia", cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(st_crs(geo_data))

ggplot(data = geo_data) + geom_sf()

## READ ME:
# there's a likelihood you're gonna need to change the function because i believe that the 
# percent estimates for the black population are NOT numeric -- that's a simple fix

# try to create a heatmap for the race, if you can do age that's even better
# we just want a prototype that works to show blair and co. before tomorrow