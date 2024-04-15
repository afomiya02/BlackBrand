library(readxl)
library(tidyverse)
# set working directory in 
# RStudio > Session > Set Working Directory > To Source File Location

# A vectorized list of all localities in Hampton Roads
# lowercase and with underscores for data cleaning purposes
# williamsburg and james city county merged together for VDOE data only
hampton_roads_edu_localities <- c(
  "chesapeake_city",
  "franklin_city",
  "gloucester_county",
  "hampton_city",
  "isle_of_wight_county",
  "mathews_county",
  "newport_news_city",
  "norfolk_city",
  "poquoson_city",
  "portsmouth_city",
  "southampton_county",
  "suffolk_city",
  "virginia_beach_city",
  "williamsburg-james_city_county",
  "york_county"
)

## ALL EDUCATIONAL DATA FOUND IN THE VDOE STATISTICS TAB
## FOR STUDENT DATA:
# https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304 
## FOR EDUCATOR DATA:
# https://www.doe.virginia.gov/teaching-learning-assessment/teaching-in-virginia/education-workforce-data-reports 
## FOR STANDARDIZED TESTING DATA:
# https://www.doe.virginia.gov/teaching-learning-assessment/student-assessment/state-assessment-results 
## FOR COHORT GRADUATION RATES:
# https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=305

# Preprocesses educator count data for grouping purposes
educator_count_data <- read_excel("data/VDOE/educator_count.xlsx") %>%
    # clean dataframe body for easier cleaning
    slice(-c(1:2, 138:140)) %>% # slice first two and last two rows containing no data
    set_names(unlist(.[1,])) %>% slice(-c(1, 135)) %>% # rename column names with first row
    # clean column and value names
    rename_all(tolower) %>% rename_all(str_replace_all, " ", "_") %>% # rename all COLUMNS to lower
    mutate(across(everything(), tolower)) %>% # make string columns to lower
    mutate(across(everything(), str_replace_all, " ", "_")) %>% # replace spaces w/ underscores for data cleaning
    select(-division_no.) %>%
    # postprocess data for legibility
    mutate(division_name = str_replace(division_name, "_public_schools", "")) %>% # remove public schools from div name
    filter(division_name %in% hampton_roads_edu_localities) %>% # filter only hampton roads
    mutate(division_name = case_when(str_detect(division_name, "^w") ~ division_name,
                                     TRUE ~ str_replace(division_name, "_city", ""))) %>%
    mutate_if(is.character, str_replace_all, "_", " ") %>%
    mutate_if(is.character, str_to_title) %>%
    mutate(across(total_counts:not_specified, as.numeric)) %>% # make numeric columns numeric
    mutate_all(~tidyr::replace_na(., 0)) %>% # replace NA with zero
    mutate(other = american_indian + hawaiian + two_or_more_races + not_specified) %>%
    # remove unnecessary columns
    select(-c(not_specified, american_indian, hawaiian, two_or_more_races)) # had zero entries across all localities
    
    

# Preprocesses student count data for grouping purposes
student_count_data <- read.csv("data/VDOE/student_count.csv") %>%
    # clean string names
    mutate_if(is.character, trimws) %>%
    mutate_if(is.character, str_replace_all, ",", "") %>%
    # rename column names to match lowerscore and underscores (column_name)
    rename_with(~str_replace_all(., fixed(".."), fixed("."))) %>%
    rename_with(~str_replace_all(., fixed("."), "_")) %>%
    rename_with(~str_replace(., "_$", ""), ends_with("_")) %>% # regex specifies last instance of _
    rename_all(tolower) %>%
    # rename column VALUES to match above naming convention
    mutate(across(everything(), tolower)) %>%
    mutate(across(everything(), str_replace_all, " ", "_")) %>%
    filter(division_name %in% hampton_roads_edu_localities) %>%
    mutate(race = case_when(
        race == "black_not_of_hispanic_origin" ~ "black",
        race == "native_hawaiian__or_pacific_islander" ~ "pacific_islander",
        race == "non-hispanic_two_or_more_races" ~ "multiracial",
        race == "white_not_of_hispanic_origin" ~ "white",
        race == "american_indian_or_alaska_native" ~ "native_american",
        TRUE ~ as.character(race) # default
    )) %>%
    # remove unnecessary columns
    select(-c(contains("all"), school_year, division_number)) %>%
    # pivot data wider such that the column names contain the division names and races
    pivot_wider(names_from = race, values_from = total_count) %>%
    # post-process character strings to title
    # in case of williamsburg
    mutate(division_name = case_when(str_detect(division_name, "^w") ~ division_name,
                                     TRUE ~ str_replace(division_name, "_city", ""))) %>%
    mutate(division_name = str_replace_all(division_name, "_", " ")) %>%
    mutate(division_name = str_to_title(division_name)) %>%
    # mutate numeric columns
    mutate_at(vars(-division_name), as.numeric) %>%
    replace(is.na(.), 0) %>%
    # create "other" category such that it includes native american, 
    # pacific islander and multiracial races
    mutate(other = native_american + pacific_islander + multiracial) %>%
    select(-c(native_american, pacific_islander, multiracial))

# Function to get and clean on-time pass rates per division
cohort_pass_rates <- read.csv("data/VDOE/on_time_graduation_rates.csv") %>%
    # clean string names
    mutate_if(is.character, trimws) %>%
    mutate_if(is.character, str_replace_all, ",", "") %>%
    mutate_if(is.character, tolower) %>%
    mutate_if(is.character, str_replace_all, " ", "_") %>%
    # rename column names to match lowerscore and underscores (column_name)
    rename_with(~str_replace_all(., fixed(".."), fixed("."))) %>%
    rename_with(~str_replace_all(., fixed("."), "_")) %>%
    rename_with(~str_replace(., "_$", ""), ends_with("_")) %>% # regex specifies last instance of _
    rename_all(tolower) %>%
    # filter data to match hampton roads localities
    filter(division_name %in% hampton_roads_edu_localities) %>%
    mutate(race = case_when(
        race == "black_not_of_hispanic_origin" ~ "black",
        race == "native_hawaiian__or_pacific_islander" ~ "pacific_islander",
        race == "non-hispanic_two_or_more_races" ~ "multiracial",
        race == "white_not_of_hispanic_origin" ~ "white",
        race == "american_indian_or_alaska_native" ~ "native_american",
        TRUE ~ as.character(race) # default
    )) %>%
    # get rid of unnecessary cols
    select(c(cohort_year, division_name, race, graduation_rate)) %>%
    mutate(graduation_rate = str_replace_all(graduation_rate, "<|%", "")) %>%
    mutate(graduation_rate = as.numeric(graduation_rate)) %>%
    # post-process character strings to title
    # in case of williamsburg
    mutate(division_name = case_when(str_detect(division_name, "^w") ~ division_name,
                                     TRUE ~ str_replace(division_name, "_city", ""))) %>%
    mutate(across(division_name:race, str_replace_all, "_", " ")) %>%
    mutate(across(division_name:race, str_to_title))

# REGRESSING MISSING VALUES USING LINEAR REGRESSION
# some values may be off since adjr2 = 37%
# aka this only accounts for 37% of data
fit <- lm(graduation_rate ~ ., data = cohort_pass_rates)

cohort_pass_rates <- cohort_pass_rates %>%
    rowwise() %>%
    mutate(graduation_rate = ifelse(is.na(graduation_rate), 
                                    predict(fit, newdata = across(everything())), 
                                    graduation_rate)) %>%
    mutate(graduation_rate = round(graduation_rate, 2)) %>%
    ungroup()
    
# Function to assist inner joining several standardized testing pass rates
#
# Returns:
#   inner-joined data frame
preprocess_subject_pass_rates <- function() {
    # this df will keep all the subgroups together
    df1 <- read_excel("data/VDOE/subject/subject_pass_rates_2013-2016.xlsx") %>%
        set_names(unlist(.[1,])) %>% slice(-1) %>%
        mutate_at("Div Num", as.numeric) %>%
        filter(!grepl("Limited", Subgroup)) # limited english proficient only exists here
    
    # will only keep the pass rates
    df2 <- read_excel("data/VDOE/subject/subject_pass_rates_2016-2019.xlsx") %>%
        mutate_at("Div Num", as.numeric)
    
    # only keeps pass rates and drops 
    df3 <- read_excel("data/VDOE/subject/subject_pass_rates_2020-2023.xlsx") %>%
        filter(!grepl("Remote", Subject)) %>%
        mutate_at("Div Num", as.numeric)
    
    # create inner join that matches all division numbers, subgroups and subjects
    # div num = county name
    df <- inner_join(df1, df2, by = c("Div Num", "Subgroup", "Subject")) %>%
        inner_join(., df3, by = c("Div Num", "Subgroup", "Subject")) %>%
        select_at(vars(-ends_with(c("x", "y", "Level")))) %>%
        mutate_at(vars(ends_with("Pass Rate")), as.numeric) %>%
        # make strings tolower with underscores and no colons
        mutate_if(is.character, tolower) %>%
        mutate_if(is.character, str_replace_all, " ", "_") %>%
        # rename column names
        rename_with(tolower) %>%
        rename_with(~str_replace_all(., " ", "_")) %>%
        # filter everything under hampton roads localities
        # and ONLY compare races
        filter(division_name %in% hampton_roads_edu_localities) %>%
        filter(subgroup %in% c("all students", "asian", "black", "hispanic", "white")) %>%
        select(-div_num) %>%
        # TODO regress NA values
        # next semester's team: Make sure to research why these NAs are happening.
        # for the most part the NA values are because there aren't enough samples to
        # be collected, i.e in Franklin's case there just simply aren't enough samples
        # to be collected. it's a good idea to take a good look
        drop_na()

    return(df)
}

st_data <- preprocess_subject_pass_rates() %>%
    mutate(division_name = case_when(str_detect(division_name, "^w") ~ division_name,
                                     TRUE ~ str_replace(division_name, "_city", ""))) %>%
    mutate_if(is.character, str_replace_all, "_", " ") %>%
    mutate_if(is.character, str_to_title) 
