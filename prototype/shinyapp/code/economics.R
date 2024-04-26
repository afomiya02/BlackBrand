library(tidyverse)
library(viridis)
library(leaflet)
library(shiny)

# Median Income line plots  -------------------------------------------------------
process_income_data <- function(year) {
    # VA file paths based on the year
    va_yr <- read.csv(paste0("data/economics/income/va_income", year, ".csv"))
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    
    #median income
    if(year == "2016" || year == "2015" || year == "2014" || year == "2013" || 
       year == "2012" || year == "2011" || year == "2010") {
        va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
    }
    else {
        va_race_income_median <- data.frame(va_yr[c(81, 83), 4])
    }
    va_race_income <-
        data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    
    #Hampton Income
    hamp_yr <- read.csv(paste0("data/economics/income/hampton_income", year, ".csv"))
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    if(year == "2016" || year == "2015" || year == "2014"|| year == "2013" 
       || year == "2012" || year == "2011" || year == "2010") {
        hamp_income3 <- hamp_income2 %>% group_by(NAME) %>% slice(c(31, 33))
    }
    else {
        hamp_income3 <- hamp_income2 %>% group_by(NAME) %>% slice(c(81, 83))
    }
    #This give us overall hampton overall and black median income
    variable <- sample(c("S1903_C03_001", "S1903_C03_003"), 32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% 
        summarize(median(estimate, na.rm = TRUE))
    
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    
    #having all the estimates in the same column
    median_income_year <-
        data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income_year <-
        mutate(median_income_year, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income_year <-
        mutate(median_income_year, demo = rep(c(
            "Total Population", "Black Population"
        ), 2))
    colnames(median_income_year) <-
        c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income_year <-
        transform(median_income_year,
                  `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income_year) <-
        c("Median Income (US Dollars)", "Location", "Demographic")
    median_income_year <- mutate(median_income_year, Year = year)
}

## Homeownership 
# Function to read homeownership data
# wasn't able to find anything with this -marcos
read_homeownership_data <- function() {
    # Read data for black homeowners in 2019
    b_hm_19 <- read_rds("data/economics/homeownership/TableS2502FiveYearEstimates/bhmown2019.rds")
    # Read data for total homeowners in 2019
    tot_hm_19 <- read_rds("data/economics/homeownership/TableS2502FiveYearEstimates/tothmown2019.rds")
    # Read all home data
    all_hm_data <- read_rds("data/economics/homeownership/TableS2502FiveYearEstimates/allhomedata.rds")
    # Rename column for demographic
    colnames(all_hm_data)[2] <- "Demographic"
    
    # Return a list containing the data
    return(list(b_hm_19 = b_hm_19, tot_hm_19 = tot_hm_19, all_hm_data = all_hm_data))
}

## Labor Market 
# Employment By Sector 

# Function to read CSV data, preprocess, and generate plot for a specific year
# TODO 1.) the colors change between years -- that CANNOT happen
# TODO 2.) the data isn't normalized
# Function to read CSV data, preprocess, and generate plot for a specific year
read_and_plot_sectors <- function(year) {
  # Construct file path for the CSV file based on the selected year
  file_path <- paste0("data/economics/labor_market/TableDP03FiveYearEstimates/top2employmentSectors", year, ".csv")
  
  # Check if the file exists
  if (file.exists(file_path)) {
    # Read CSV file
    sectors_data <- read.csv(file_path)
    
    # Rename columns for better readability
    colnames(sectors_data) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
    
    # Data preprocessing: remove "County, Virginia" and "city, Virginia" from the 'Name' column
    sectors_data <- sectors_data %>%
      mutate(Name = str_remove(Name, "County, Virginia")) %>%
      mutate(Name = str_remove(Name, "city, Virginia"))
    
    # Create a vector of unique sectors
    unique_sectors <- unique(sectors_data$Sector)
    
    # Create ggplot object
    plot <- ggplot(sectors_data, aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + 
      geom_col() +  # Plot columns
      theme_minimal() +  # Minimal theme
      labs(title = "", y = "Total Number of People Employed", x = "") +  # Set titles for plot
      theme(axis.text.x = element_text(angle = 40)) +  # Rotate x-axis labels for better readability
      scale_fill_manual(values = viridis::viridis_pal()(length(unique_sectors))) +  # Use viridis color palette
      ggtitle(paste("Top 2 Employment Sectors in", year, "per 10k People")) +  # Add plot title
      xlab("Location") +  # Label for x-axis
      ylab("Number of People Employed") +  # Label for y-axis
      coord_cartesian(ylim = c(0, 10000))  # Set custom limits for y-axis
    
    # Convert ggplot object to plotly object and hide legend for better visualization
    plot <- hide_legend(ggplotly(plot, tooltip = c("x", "y", "Sector")))
    
    return(plot)
  } else {
    # Return NULL if the file does not exist
    return(NULL)
  }
}

# Unemployment Rate
# Function to generate unemployment plot for a specific year
generate_unemployment_plot <- function(year) {
  # Read unemployment data for the specified year
  unemp_data <- read.csv(paste0("data/economics/labor_market/TableS2301FiveYearEstimates/unemployment", year, ".csv"))
  colnames(unemp_data)[2] <- "Locality"
  colnames(unemp_data)[3] <- "Demographic"
  colnames(unemp_data)[4] <- "Unemployment Rate"
  
  # Read Virginia unemployment data for the specified year
  va_unemp_data <- read.csv(paste0("data/economics/labor_market/TableS2301FiveYearEstimates/vaunemployment", year, ".csv"))
  
  # Create the plot
  plot <- unemp_data %>%
    # Format locality names
    mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
    mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
    arrange(desc(Locality)) %>%
    ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
    geom_bar(position = "dodge", stat = "identity") +  # Plot bars
    geom_hline(
      yintercept = va_unemp_data$estimate,  # Add horizontal line for Virginia unemployment rate
      linetype = "dashed",
      color = "red",
      show.legend = TRUE
    ) +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    labs(
      title = "",
      y = "Unemployment Rate (%)",
      x = "",
      caption = "Source: ACS 5 Year Estimate Table S2301"
    ) +
    theme(axis.text.x = element_text(angle = 40)) +  # Rotate x-axis labels for better readability
    scale_fill_manual(values = c("#A9A9A9", "#8B0000"))  # Customize fill colors
  
  # Convert ggplot object to plotly for interactivity
  ggplotly(plot)
}
#Poverty
# Poverty Rates in VA and Hampton Roads

# Function to read and process CSV files
read_process_csv <- function(va_file, hamp_file, row_indices) {
    # Read Virginia CSV file and extract relevant columns
    va_pov <- read.csv(va_file)[, 2:6]
    # Extract poverty percentage for total population and black population
    va_pct_pov <- va_pov[row_indices[1], 4]
    va_pct_pov_blck <- va_pov[row_indices[2], 4]
    va_pov_vector <- rbind(va_pct_pov, va_pct_pov_blck)
    
    # Read Hampton Roads CSV file and extract relevant columns
    hamp_pov <- read.csv(hamp_file)[, 2:6]
    # Calculate poverty percentage for total population in Hampton Roads
    hamp_total <- hamp_pov %>% group_by(NAME) %>% slice(1)
    hamp_total2 <- colSums(hamp_total[, 4])
    hamp_pov2 <- hamp_pov %>% group_by(NAME) %>% slice(62)
    hamp_pov3 <- colSums(hamp_pov2[, 4])
    hamp_overall_pov <- hamp_pov3 / hamp_total2 * 100
    
    # Calculate poverty percentage for black population in Hampton Roads
    hamp_total_blck <- hamp_pov %>% group_by(NAME) %>% slice(14)
    hamp_total_blck2 <- colSums(hamp_total_blck[, 4])
    hamp_pov_blck <- hamp_pov %>% group_by(NAME) %>% slice(75)
    hamp_pov_blck2 <- colSums(hamp_pov_blck[, 4])
    hamp_blck_overall_pov <- hamp_pov_blck2 / hamp_total_blck2 * 100
    
    hamp_pov_vector <- rbind(hamp_overall_pov, hamp_blck_overall_pov)
    
    # Combine Virginia and Hampton Roads data into a single data frame
    pov_pct <- data.frame(va_pov_vector, hamp_pov_vector)
    pov_pct2 <- data.frame(
        Ratio = unlist(pov_pct, use.names = FALSE),
        Location = rep(c("Virginia", "Hampton Roads"), each = 2),
        Demographic = rep(c("Total Population", "Black Population"), 2)
    )
    colnames(pov_pct2) <- c("Percentage (%)", "Location", "Demographic")
    pov_pct2$Location <- factor(pov_pct2$Location, levels = c("Hampton Roads", "Virginia"))
    
    return(pov_pct2)
}

# Hampton counties poverty
# Define a reactive expression to capture the selected year from the dropdown
var_povertyCount <- reactive({
    input$PovertyCountYearDrop
})

# Function to load and process CSV files for selected years
load_process_csv <- function(year) {
    hamp_file <- paste0("data/economics/poverty/hamp_poverty", year, ".csv")
    hamp_pov <- read.csv(hamp_file)[, 2:6]
    
    # Extract data for total population and black population
    hamp_pov_tbl <- hamp_pov %>% group_by(NAME) %>% slice(123)
    hamp_pov_blck_tbl <- hamp_pov %>% group_by(NAME) %>% slice(136)
    hamp_pov_tbl <- hamp_pov_tbl %>% ungroup()
    hamp_pov_blck_tbl <- hamp_pov_blck_tbl %>% ungroup()
    
    # Combine data into a single dataframe
    hamp_pctG <- hamp_pov_tbl[, 4]
    hamp_pctB <- hamp_pov_blck_tbl[, 4]
    hamp_comb <- rbind(hamp_pctG, hamp_pctB)
    
    # Add location and demographic information
    hamp_comb <- mutate(hamp_comb, Location = rep(
        c(
            "Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight",
            "James City", "Mathews", "Newport News", "Norfolk", "Poquoson",
            "Portsmouth", "Southampton", "Suffolk", "Virginia Beach", "Williamsburg", "York"
        ), 2
    ))
    hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16), rep("Black Population", 16)))
    colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
    
    # Filter out data for Poquoson
    hamp_comb <- hamp_comb %>% filter(Location != "Poquoson")
    
    return(hamp_comb)
}

# Function to generate plot based on data
generate_plot <- function(data) {
    # Generate the bar plot using ggplot
    plot <- ggplot(data, aes(Location, y = `Percentage (%)`, fill = Demographic)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_minimal() +
        scale_fill_manual(values = c("#A9A9A9", "#8B0000")) + # Using consistent colors
        theme(
            plot.title = element_text(hjust = 0.5, size = 25),
            legend.title = element_blank(),
            axis.title.x = element_blank()
        ) +
        theme(axis.text.x = element_text(angle = 40, vjust = 0.95, hjust = 1)) +
        labs(caption = "Source: ACS 5 Year Estimate Table S1701")
    
    return(plot)
}

#Health
# Uninsured Population

# Function to load and process uninsured data
load_process_uninsured <- function(year) {
    file_path <- paste0("data/economics/health/TableS2701FiveYearEstimates/uninsured", year, ".csv")
    uninsured_data <- read.csv(file_path)
    colnames(uninsured_data)[1:3] <- c("Locality", "Demographic", "Percent Uninsured")
    uninsured_data <- uninsured_data %>%
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2)) %>%
        mutate(Locality = str_remove_all(Locality, " County, Virginia| city, Virginia"))
    return(uninsured_data)
}

# Function to generate uninsured plot
generate_uninsured_plot <- function(data) {
    plot <- data %>%
        ggplot(aes(fill = Demographic, y = `Percent Uninsured`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(
            title = "",
            y = "Percent Uninsured (%)",
            x = "",
            caption = "Source: ACS 5 Year Estimate Table S2701"
        ) +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#A9A9A9", "#8B0000"))
    return(plot)
}

#Veterans
# Function to read data for the selected year
read_veteran_data <- function(year) {
    vet_data <- readRDS(paste0("data/economics/veterans/TableS2101FiveYearEstimates/bveteran", year, ".rds"))
    military_bases <- readRDS("data/economics/veterans/TableS2101FiveYearEstimates/militarybases.rds")
    list(vet_data = vet_data, military_bases = military_bases)
}
#Business
state_summary_metrics <- read_csv("data/economics/business/success_summary_metrics_state.csv")
metro_summary_metrics <- read_csv("data/economics/business/success_summary_metrics_metro.csv")
state_names <- unique(state_summary_metrics$NAME)
metro_names <- unique(metro_summary_metrics$NAME)
# Mapping of metrics to titles
metric_titles <- c(
    "Total_Avg_Annual_Pay_Total" = "Total Average Annual Pay (Total)",
    "Total_Avg_Annual_Pay_Black_Business" = "Total Average Annual Pay (Black Business)",
    "Total_Avg_Employees_Total" = "Total Average Employees (Total)",
    "Total_Avg_Employees_Black_Business" = "Total Average Employees (Black Business)",
    "Total_Sum_of_Firms_Total" = "Total Sum of Firms (Total)",
    "Total_Sum_of_Firms_Black_Business" = "Total Sum of Firms (Black Business)",
    "Pay_Annual_Per_Employee_Total" = "Pay Annual Per Employee (Total)",
    "Pay_Annual_Per_Employee_Black_Business" = "Pay Annual Per Employee (Black Business)",
    "Percent_Total_Avg_Annual_Pay_BB" = "Percent of Total Average Annual Pay made up by Black Buisness",
    "Percent_Total_Avg_Employees_BB" = "Percent of Total Average Employees made up by Black Buisness",
    "Percent_Total_Sum_of_Firms_BB" = "Percent of Total Sum of Firms made up by Black Buisness"
)
#Household Well-being is in server.r