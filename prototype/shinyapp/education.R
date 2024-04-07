library(readxl)
library(tidyverse)
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
        mutate_if(is.character, str_replace_all, "_city", "") %>%
        # rename column names
        rename_with(tolower) %>%
        rename_with(~str_replace_all(., " ", "_")) %>%
        
        # filter everything under hampton roads localities
        filter(division_name %in% hampton_roads_edu_localities) %>%
        select(-div_num) %>%
        drop_na()  
    return(df)
}

education_data <- preprocess_subject_pass_rates() %>%
    mutate_if(is.character, str_replace_all, "_", " ") %>%
    mutate_if(is.character, str_to_title)

library(plotly)

radar_data <- education_data %>% 
    dplyr::filter(division_name %in% "Chesapeake") %>%
    dplyr::filter(subgroup %in% c("Black", "White")) %>%
    select(c(subject, subgroup, `2022-2023_pass_rate`)) %>%
    # pivot dataset such that subjects are columns and
    # subjects are row names
    pivot_wider(names_from = subject, values_from = `2022-2023_pass_rate`) %>%
    column_to_rownames(., "subgroup")

fig <- plot_ly(
    type = "scatterpolar",
    mode = "lines+markers",
) %>%
    # when adding traces for radar plots data frame has to wrap back around to first entry
    # so i unlisted entire row + 1st value in row
    add_trace(r = as.numeric(unlist(c(radar_data[1, ], radar_data[1, 1]))), 
              theta = unlist(c(colnames(radar_data), colnames(radar_data)[1])),
              name = "Black Students") %>%
    add_trace(r = as.numeric(unlist(c(radar_data[2, ], radar_data[2, 1]))), 
              theta = unlist(c(colnames(radar_data), colnames(radar_data)[1])),
              name = "White Students") %>%
    layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))))

fig

# fig <- plot_ly(
#     data = radar_data,
#     type = "scatterpolar",
#     mode = "lines+markers",
# ) %>%
#     add_trace(r = ~)






