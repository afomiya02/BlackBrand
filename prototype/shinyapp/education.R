library(readxl)
library(tidyverse)
library(sf)
# set working directory in 
# RStudio > Session > Set Working Directory > To Source File Location

# A vectorized list of all localities in Hampton Roads
# lowercase and with underscores for data cleaning purposes
# williamsburg and james city county merged together for VDOE data only
hampton_roads_edu_localities <- c(
  "chesapeake",
  "franklin",
  "gloucester_county",
  "hampton",
  "isle_of_wight_county",
  "mathews_county",
  "newport_news",
  "norfolk",
  "poquoson",
  "portsmouth",
  "southampton_county",
  "suffolk",
  "virginia_beach",
  "williamsburg-james_city_county",
  "york_county"
)

## ALL EDUCATIONAL DATA FOUND IN THE VDOE STATISTICS TAB
## https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304 FOR STUDENT DATA
## https://www.doe.virginia.gov/teaching-learning-assessment/teaching-in-virginia/education-workforce-data-reports FOR EDUCATOR DATA


# Preprocesses educator count data for grouping purposes
# 
# Params: 
#   path - .xlsx file to be processed
# Returns: 
#   small dataframe with the locality, age and race estimates/percentages
preprocess_educator_count_data <- function(path) {
  df <- read_excel(path) %>%
    slice(-c(1:2, 138:140)) %>% # slice first two and last two rows containing no data
    set_names(unlist(.[1,])) %>% slice(-1) %>% # rename column names with first row
    rename_all(tolower) %>% rename_all(str_replace_all, " ", "_") %>% # rename all COLUMNS to lower
    mutate(across(total_counts:not_specified, as.numeric)) %>% # make numeric columns numeric
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% # replace NA with zero
    mutate(across(everything(), tolower)) %>% # make string columns to lower
    mutate(across(everything(), str_replace_all, " ", "_")) %>% # replace spaces w/ underscores for data cleaning
    mutate(division_name = str_replace(division_name, "_public_schools", "")) %>% # remove public schools from div name
    mutate(division_name = str_replace(division_name, "_city", "")) %>% # remove city if in name
    # for whatever reason williamsburg and james city county are merged together so
    # rename williamsburg-james city county to williamsburg for data grouping purposes
    mutate(division_name = ifelse( 
      division_name == "williamsburg-james_county", "james_city_county", division_name
    )) %>% 
    filter(division_name %in% hampton_roads_localities) %>% # filter only hampton roads
    select(-division_no.)
  return(df)
}

# Preprocesses student count data for grouping purposes
# 
# Params: 
#   path - .csv file to be processed
# Returns: 
#   small dataframe with the locality and total count
preprocess_student_count_data <- function(path) {
  df <- read.csv(path) %>%
    mutate(across(everything(), str_trim)) %>% # trim whitespace
    select(c(3, 7)) %>% # select division name, and total count of division
    `colnames<-`(c("division_name", "total_count")) %>%
    mutate(across(everything(), tolower)) %>% # make everything lowercase
    mutate(across(everything(), str_replace_all, ",", "")) %>%
    mutate(across(total_count, as.numeric)) %>% # name numeric columns numeric
    mutate(across(division_name, str_replace_all, " ", "_")) %>% # replace spaces w/ underscores for data cleaning
    # rename williamsburg-james city county to williamsburg for data grouping purposes
    mutate(division_name = str_replace(division_name, "_city", "")) %>% # remove city if in name
    mutate(division_name = ifelse( 
      division_name == "williamsburg-james_county", "williamsburg", division_name
    )) %>% 
    filter(division_name %in% hampton_roads_localities)
  return(df)
}