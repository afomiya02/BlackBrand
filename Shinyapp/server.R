# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
# jscode <- "function getUrlVars() {
#                 var vars = {};
#                 var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
#                     vars[key] = value;
#                 });
#                 return vars;
#             }
#            function getUrlParam(parameter, defaultvalue){
#                 var urlparameter = defaultvalue;
#                 if(window.location.href.indexOf(parameter) > -1){
#                     urlparameter = getUrlVars()[parameter];
#                     }
#                 return urlparameter;
#             }
#             var mytype = getUrlParam('type','Empty');
#             function changeLinks(parameter) {
#                 links = document.getElementsByTagName(\"a\");
#                 for(var i = 0; i < links.length; i++) {
#                    var link = links[i];
#                    var newurl = link.href + '?type=' + parameter;
#                    link.setAttribute('href', newurl);
#                  }
#             }
#            var x = document.getElementsByClassName('navbar-brand');
#            if (mytype != 'economic') {
#              x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
#                               '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
#                               '</a></div>';
#              //changeLinks('dspg');
#            } else {
#              x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
#                               '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
#                               '</a></div>';
#              //changeLinks('economic');
#            }
#            "


# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  # runjs(jscode)
  
  #hampton roads map of counties -------------------------------------------
  #function to load and preprocess data
  preprocess_data <- function() {
    coord_data <- read_rds("data/age/coordinates.rds")
    coord_data <- st_transform(coord_data)
    coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
    coordinates2 <- coordinates1[, 6]
    city_names <- c(
      "Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight",
      "James City", "Mathews", "Newport News", "Norfolk", "Poquoson",
      "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
      "Williamsburg", "York"
    )
    coordinates2 <- mutate(coordinates2, Loc = city_names)
    coordinates2$Loc[coordinates2$Loc == "Franklin"] <- "Franklin City"
    return(coordinates2)
  }
  
  #render the Hampton counties map
  output$hampton_counties_map <- renderPlot({
    coordinates2 <- preprocess_data()
    #Graph
    hampton_counties_map <- ggplot(coordinates2) +
      geom_sf() +
      geom_sf_label(
        aes(label = Loc, geometry = geometry),
        label.padding = unit(.5, "mm"),
        size = 4
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        panel.background = element_blank()
      )
    #plot
    hampton_counties_map
    
  })
  
  # hampton race plots -----------------------------------------------------

  # Define a function for preprocessing data for Virginia race pie chart
  preprocess_hamp_race_data <- function(hamp_race_file_path) {
    hamp_data <- read.csv(hamp_race_file_path)
    hamp_data <- hamp_data[, 2:6]
    
    #combine data from counties
    variable <- sample(
      c(
        "B02001_001", "B02001_002", "B02001_003", "B02001_004", "B02001_005",
        "B02001_006", "B02001_007", "B02001_008", "B02001_009", "B02001_010"
      ),
      160,
      replace = TRUE
    )
    
    hamp_data <- hamp_data %>% 
      group_by(variable) %>% 
      summarize(sum(estimate))
    
    #select the columns and rows needed
    hamp_races <- hamp_data[c(2:8), ]
    
    #graphing with other = Hawaiian/Pacific Islander, American/Alaska Native, Other
    hamp_races3 <- data.frame(t(hamp_data[c(2:8), 2]))
    hamp_races3 <- mutate(hamp_races3, X8 = X3 + X5 + X6)
    hamp_races4 <- data.frame(t(hamp_races3[, c(1, 2, 4, 7, 8)]))
    colnames(hamp_races4) <- "estimate"
    total_pop <- sum(hamp_races4$estimate)
    hamp_races4 <- mutate(hamp_races4, total = total_pop)
    hamp_races4 <- mutate(hamp_races4, pct = estimate / total * 100)
    hamp_races4 <- mutate(
      hamp_races4,
      race = c("White", "Black", "Asian", "Two or more", "Other")
    )
    colnames(hamp_races4) <- c("estimate", "Total", "Percent of Population", "race")
    
    hamp_races5 <- hamp_races4 %>%
      mutate(
        cs = rev(cumsum(rev(`Percent of Population`))),
        pos = `Percent of Population` / 2 + lead(cs, 1),
        pos = if_else(is.na(pos), `Percent of Population` / 2, pos)
      )
    
    return(hamp_races5)
  }
  
  #define reactive variable
  var_hampRace <- reactive({
    input$hampRaceYearDrop
  })
  
  # Render the hamp_pie plot
  output$hamp_pie <- renderPlot({
    # Define color palette
    vir_pal <- c("#FF0000",
                 "#C1CDC1",
                 "#F0FFF0",
                 "#8B7355",
                 "#B22222")
    
    # Load and process data
    hamp_race_file_path <- paste0("data/race/hamp_race", var_hampRace(), ".csv")
    hamp_races_data <- preprocess_hamp_race_data(hamp_race_file_path)
    
    # Plot hamp_pie
    hamp_pie <- ggplot(hamp_races_data, aes(x = "", y = `Percent of Population`, fill = fct_inorder(race))) +
      geom_col(width = 1) +
      coord_polar(theta = "y", start = 0) +  # Specify inner radius for donut
      geom_label_repel(
        aes(y = pos, label = paste0(round(`Percent of Population`, digits = 2), "%")),
        data = hamp_races_data,
        size = 4,
        show.legend = FALSE,
        nudge_x = 1
      ) +
      guides(fill = guide_legend(title = "Key")) +
      scale_fill_manual(values = vir_pal) +  # Use custom color palette
      theme_void() +
      theme(legend.title = element_blank())
    
    
    # Return the plot
    return(hamp_pie)
  })
  

  # VA race plots -----------------------------------------------------
 
  # Define a function for preprocessing data for Virginia race pie chart
   process_va_race_data <- function(va_race_file_path) {
    races <- read.csv(va_race_file_path)
    races <- races[, 2:6]
    total <- races[1, 4]
    
    # Now graphing with other = Hawaiian/PI, American/Alaska Native, Other
    va_races <- data.frame(t(races[c(2:8), 4]))
    va_races <- mutate(va_races, X8 = X3 + X5 + X6)
    va_races2 <- data.frame(t(va_races[, c(1, 2, 4, 7, 8)]))
    colnames(va_races2) <- "estimate"
    va_races2 <- mutate(va_races2, totl = total)
    va_races2 <- mutate(va_races2, pct = estimate / totl * 100)
    va_races2 <- mutate(va_races2, race = c("White", "Black", "Asian", "Two or more", "Other"))
    va_races3 <- va_races2 %>%
      mutate(
        cs = rev(cumsum(rev(pct))),
        pos = pct / 2 + lead(cs, 1),
        pos = if_else(is.na(pos), pct / 2, pos)
      )
    
    return(va_races3)
  }
  
  var_VaRace <- reactive({
    input$VaRaceYearDrop
  })
  
  # Render the va_pie plot
  output$va_pie <- renderPlot({
    # Define color palette
    vir_pal <- c("#FF0000",
                 "#C1CDC1",
                 "#F0FFF0",
                 "#8B7355",
                 "#B22222")
    
    # Load and process data
    va_race_file_path <- paste0("data/race/va_race", var_VaRace(), ".csv")
    va_races_data <- process_va_race_data(va_race_file_path)
    
    # Plot va_pie
    va_pie <- ggplot(va_races_data, aes(x = "", y = pct, fill = fct_inorder(race))) +
      geom_col(width = 1) +
      coord_polar(theta = "y", start = 0) +
      geom_label_repel(
        aes(y = pos, label = paste0(round(pct, digits = 2), "%")),
        data = va_races_data,
        size = 4,
        show.legend = FALSE,
        nudge_x = 1
      ) +
      guides(fill = guide_legend(title = "Key")) +
      scale_fill_manual(values = vir_pal) +
      theme_void() +
      theme(legend.title = element_blank())
    
    # Return the plot
    return(va_pie)
  })
  
  # Hampton age plot-------------------------------------------------
 
   # Define a function for preprocessing data for Hampton age graph
  process_hamp_age_data <- function(file_path) {
    hamp_ages <- read.csv(file_path)
    hamp_ages <- hamp_ages[, 2:6]
    
    # Total population in Hampton Roads (1713267)
    hamp_pop_tbl <- hamp_ages %>%
      group_by(NAME) %>%
      slice(1)
    hamp_pop <- colSums(hamp_pop_tbl[, 4])
    
    # Getting male estimates for each age group (summing every county for that specific male age group)
    hamp_male <- hamp_ages %>%
      group_by(NAME) %>%
      slice(3:25)
    hamp_male2 <- hamp_male %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))
    
    # Getting female estimates for each age group (summing every county for that specific female age group)
    hamp_female <- hamp_ages %>%
      group_by(NAME) %>%
      slice(27:49)
    hamp_female2 <- hamp_female %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))
    
    hamp_gender <- cbind(hamp_male2, hamp_female2)
    hamp_gender <- hamp_gender[, c(2, 4)]
    colnames(hamp_gender) <- c("male", "female")
    hamp_gender <- mutate(hamp_gender, total = male + female)
    
    # Transposing just the estimates
    hamp_ages2 <- data.frame(t(hamp_gender[, 3]))
    
    # Sorting into the age groups
    hamp_ages2 <- mutate(hamp_ages2, Under18 = X1 + X2 + X3 + X4)
    hamp_ages2 <- mutate(hamp_ages2, YoungAdult = X5 + X6 + X7 + X8 + X9)
    hamp_ages2 <- mutate(hamp_ages2, Adult = X10 + X11 + X12)
    hamp_ages2 <- mutate(hamp_ages2, MiddleAge = X13 + X14 + X15 + X16 + X17)
    hamp_ages2 <- mutate(hamp_ages2, Senior = X18 + X19 + X20 + X21 + X22 + X23)
    
    # Using just the 5 age group data that was just sorted
    hamp_ages3 <- hamp_ages2[, 24:28]
    row.names(hamp_ages3) <- "General Estimate"
    hamp_ages3 <- data.frame(t(hamp_ages3))
    
    # Getting the percentage
    hamp_ages3 <- mutate(hamp_ages3, TotalPopulation = hamp_pop)
    hamp_ages3 <- mutate(hamp_ages3, PctPop = General.Estimate / TotalPopulation * 100)
    hamp_ages3 <- mutate(hamp_ages3, Labels = c("Under 18", "18 to 29", "30 to 44", "45 to 64", "65 and Older"))
    
    # Ordering the age groups
    hamp_ages3$Labels <- factor(hamp_ages3$Labels, levels = c("Under 18", "18 to 29", "30 to 44", "45 to 64", "65 and Older"))
    
    return(hamp_ages3)
  }
  
  # Define reactive variable
  var_hampAge <- reactive({
    input$HampAgeYearDrop
  })
  
  # Render the hamp_graph plot
  output$hamp_graph <- renderPlot({
    # Define color palette
    vir_pal <- c("#FF0000",
                 "#C1CDC1",
                 "#F0FFF0",
                 "#8B7355",
                 "#B22222")
    
    # Load and process data
    hamp_age_file_path <- paste0("data/age/hamp_age", var_hampAge(), ".csv")
    hamp_ages_data <- process_hamp_age_data(hamp_age_file_path)
    
    # Plot hamp_graph
    hamp_graph <- ggplot(hamp_ages_data, aes(x = "", y = PctPop, fill = Labels)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar("y", start = 0) +
      theme_void() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13)
      ) +
      geom_text(
        aes(label = paste0(round(PctPop), "%")),
        position = position_stack(vjust = 0.5),
        size = 5,
        color = "black"
      ) +
      scale_fill_manual(values = vir_pal)
    
    # Return the plot
    return(hamp_graph)
  })
  
  # Va age plot----------------------------------------------------
  # Preprocess Virginia age data function
  preprocess_va_age_data <- function(file_path) {
    age_data <- read.csv(file_path)
    age_data <- age_data[, 2:6]
    
    va_total_pop <- age_data[1, 4]
    
    va_male_age <- age_data[3:25, ]
    va_female_age <- age_data[27:49, ]
    
    va_male_age <- tibble::rowid_to_column(va_male_age, "ID")
    va_female_age <- tibble::rowid_to_column(va_female_age, "ID")
    
    ages <- merge(va_female_age, va_male_age, by = "ID")
    ages <- mutate(ages, total = estimate.x + estimate.y)
    
    va_ages1 <- data.frame(t(ages[, 12]))
    va_ages1 <- mutate(va_ages1, Under18 = X1 + X2 + X3 + X4)
    va_ages1 <- mutate(va_ages1, YoungAdult = X5 + X6 + X7 + X8 + X9)
    va_ages1 <- mutate(va_ages1, Adult = X10 + X11 + X12)
    va_ages1 <- mutate(va_ages1, MiddleAge = X13 + X14 + X15 + X16 + X17)
    va_ages1 <- mutate(va_ages1, Senior = X18 + X19 + X20 + X21 + X22 + X23)
    
    va_ages2 <- va_ages1[, 24:28]
    row.names(va_ages2) <- "Estimate"
    va_ages2 <- data.frame(t(va_ages2))
    va_ages2 <- mutate(va_ages2, TotalPopulation = va_total_pop)
    
    va_ages2 <- mutate(va_ages2, PctPop = Estimate / TotalPopulation * 100)
    va_ages2 <- mutate(va_ages2, labels = c("Under 18", "18 to 29", "30 to 44", "45 to 64", "65 and Older"))
    colnames(va_ages2) <- c("Estimate", "Total Population", "Percent of Population", "Labels")
    va_ages2[, 4] <- factor(va_ages2[, 4], levels = c("Under 18", "18 to 29", "30 to 44", "45 to 64", "65 and Older"))
    
    return(va_ages2)
  }
  
  # Define reactive variable
  var_VaAge <- reactive({
    input$VaAgeYearDrop
  })
  
  # Render the va_graph plot
  output$va_graph <- renderPlot({
    # Define color palette
    vir_pal <- c("#FF0000",
                 "#C1CDC1",
                 "#F0FFF0",
                 "#8B7355",
                 "#B22222")
    
    # Load and preprocess Virginia age data
    va_age_file_path <- paste0("data/age/va_age", var_VaAge(), ".csv")
    va_ages_data <- preprocess_va_age_data(va_age_file_path)
    
    # Plot va_graph
    va_graph <- ggplot(va_ages_data , aes(x = "", y = `Percent of Population`, fill = Labels)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar("y", start = 0) +
      theme_void() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13)
      ) +
      geom_text(
        aes(label = paste0(round(`Percent of Population`), "%")),
        position = position_stack(vjust = 0.5),
        size = 5,
        color = "black"
      ) +
      scale_fill_manual(values = vir_pal)
    
    # Return the plot
    return(va_graph)
  })
  
  # Hampton Counties Map ------------------------------------------
  # Preprocess Hampton age group by localities data function
  preprocess_hamp_counties_age <- function(file_path) {
    hamp_ages <- read.csv(file_path)
    hamp_ages <- hamp_ages[, 2:6]
    
    county_pop <- hamp_ages %>% group_by(NAME) %>% slice(1)
    county_pop <- county_pop[, 4]
    
    county_male <- hamp_ages %>% group_by(NAME) %>% slice(3:25)
    county_female <- hamp_ages %>% group_by(NAME) %>% slice(27:49)
    
    county_male <- tibble::rowid_to_column(county_male, "ID")
    county_female <- tibble::rowid_to_column(county_female, "ID")
    
    county_ages <- merge(county_female, county_male, by = "ID")
    county_ages <- mutate(county_ages, total = estimate.x + estimate.y)
    
    county_under <- county_ages %>% group_by(NAME.y) %>% slice(1:4)
    county_under2 <- county_under %>% group_by(NAME.y) %>% summarise(x = sum(total))
    county_under2 <- county_under2[, 2]
    
    county_ya <- county_ages %>% group_by(NAME.y) %>% slice(5:9)
    county_ya2 <- county_ya %>% group_by(NAME.y) %>% summarise(x = sum(total))
    county_ya2 <- county_ya2[, 2]
    
    county_adult <- county_ages %>% group_by(NAME.y) %>% slice(10:12)
    county_adult2 <- county_adult %>% group_by(NAME.y) %>% summarise(x = sum(total))
    county_adult2 <- county_adult2[, 2]
    
    county_ma <- county_ages %>% group_by(NAME.y) %>% slice(13:17)
    county_ma2 <- county_ma %>% group_by(NAME.y) %>% summarise(x = sum(total))
    county_ma2 <- county_ma2[, 2]
    
    county_senior <- county_ages %>% group_by(NAME.y) %>% slice(18:23)
    county_senior2 <- county_senior %>% group_by(NAME.y) %>% summarise(x = sum(total))
    county_senior2 <- county_senior2[, 2]
    
    counties_label <- c(
      "Chesapeake", "Franklin", "Hampton", "Newport News", "Norfolk",
      "Poquoson", "Portsmouth", "Suffolk", "Virginia Beach", "Williamsburg",
      "Gloucester", "Isle of Wight", "James City", "Mathews", "Southampton", "York"
    )
    
    lat <- c(
      36.690473, 36.683540, 37.046933, 37.123232, 36.903378, 37.130348, 36.878493,
      36.714941, 36.792042, 37.267284, 37.405450, 36.901637, 37.311197, 37.470724,
      36.720152, 37.242246
    )
    
    lon <- c(
      -76.297654, -76.940148, -76.390236, -76.523771, -76.248186, -76.357799, -76.380289,
      -76.626346, -76.053855, -76.708205, -76.519133, -76.708161, -76.804677, -76.375820,
      -77.114512, -76.566393
    )
    
    general_county_alt <- cbind(
      county_under2, county_ya2, county_adult2, county_ma2, county_senior2, county_pop
    )
    
    colnames(general_county_alt) <- c("a", "b", "c", "d", "e", "total")
    
    general_county_alt <- mutate(general_county_alt, under = a / total * 100)
    general_county_alt <- mutate(general_county_alt, ya = b / total * 100)
    general_county_alt <- mutate(general_county_alt, adult = c / total * 100)
    general_county_alt <- mutate(general_county_alt, ma = d / total * 100)
    general_county_alt <- mutate(general_county_alt, senior = e / total * 100)
    
    general_county_alt2 <- general_county_alt[, 7:11]
    general_county_alt2 <- mutate(general_county_alt2, county = counties_label)
    general_county_alt2 <- cbind(general_county_alt2, lon, lat)
    
    colnames(general_county_alt2) <- c("A", "B", "C", "D", "E", "county", "lon", "lat")
    
    return(general_county_alt2)
  }
  
  # Define reactive variable
  var_hampCountiesAge <- reactive({
    input$HampCountAgeYearDrop
  })
  
  # Render the age_map plot
  output$age_map <- renderPlot({
    # Load and preprocess Hampton counties age data
    hamp_counties_age_file_path <- paste0("data/age/hamp_age", var_hampCountiesAge(), ".csv")
    hamp_counties_age_data <- preprocess_hamp_counties_age(hamp_counties_age_file_path)
    
    # Getting map data for counties in Hampton roads
    coord_data <- read_rds("data/age/coordinates.rds")
    coord_data <- st_transform(coord_data)
    coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
    coordinates2 <- coordinates1[, 6]
    city <- c(
      "Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
      "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
      "Suffolk", "Virginia Beach", "Williamsburg", "York"
    )
    
    coordinates2 <- mutate(coordinates2, Loc = city)
    coordinates2$Loc[coordinates2$Loc == "Franklin"] <- "Franklin City"
    
    # Plotting the age map
    age_map <- ggplot(coordinates2) +
      geom_sf() +
      geom_scatterpie(
        aes(x = lon, y = lat, group = county, r = 0.05),
        data = hamp_counties_age_data,
        cols = LETTERS[1:5]
      ) +
      geom_sf_label(
        aes(label = Loc, geometry = geometry),
        label.padding = unit(0.5, "mm"),
        size = 4,
        nudge_x = 0.05,
        nudge_y = 0.05
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13)
      ) +
      scale_fill_viridis_d(option = "F",
        labels = c(
          "Under 18", "18 to 29", "30 to 44",
          "45 to 64", "65 and Older"
        )
      )
    
    # Displaying the plot
    age_map
  })
  
  # Total Population Educational Attainment ---------------------------------
  
  # Define reactive variable
  var_genEducationalAttainment <- reactive({
    input$genEdAttainmentYearDrop
  })
  
  output$genEdAttainmentPlots <- renderPlotly({
    # Read CSV files based on the selected year
    data_year <- switch(var_genEducationalAttainment(),
                        "2019" = "2019",
                        "2018" = "2018",
                        "2017" = "2017",
                        "2016" = "2016",
                        "2015" = "2015",
                        "2014" = "2014",
                        "2013" = "2013",
                        "2012" = "2012",
                        "2011" = "2011",
                        "2010" = "2010")
    
    # Read general educational attainment data
    generalEducationalAttainment <- read.csv(paste0("data/TableS1501FiveYearEstimates/generalEducationalAttainment", data_year, ".csv"))
    
    # Read general black educational attainment data
    generalBlackEducationalAttainment <- read.csv(paste0("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment", data_year, ".csv"))
    
    # Rename columns
    colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
    colnames(generalBlackEducationalAttainment) <- c("Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
    
    # Set variables for population types
    generalEducationalAttainment$Variable <- rep(c("Total Population"), 16)
    generalBlackEducationalAttainment$Variable <- rep(c("Black Population"), 16)
    
    # Combine data frames
    generalTotal <- rbind(generalEducationalAttainment, generalBlackEducationalAttainment[, c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")])
    
    # Rename columns
    colnames(generalTotal) <- c("Name", "Demographic", "Bachelor or Higher as Highest Attainment %")
    
    # Convert percentages to numeric
    generalTotal$`Bachelor or Higher as Highest Attainment %` <- round(as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`), 1)
    
    # Plot
    va_tot_education_bar <- generalTotal %>%
      mutate(Name = str_remove(Name, "County, Virginia")) %>%
      mutate(Name = str_remove(Name, "city, Virginia")) %>%
      arrange(desc(Name)) %>%
      ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      labs(title = "",
           y = "Percent (%)",
           x = "") + 
      theme(axis.text.x = element_text(angle = 40)) +
      scale_fill_manual(values = c("#A9A9A9", "#8B0000"))
    
    ggplotly(va_tot_education_bar)
  })
  
  
  # Teacher Demographics------------------------------------------------------
  # Define a function to read CSV file and process data based on selected race
  process_teacher_race <- function(race) {
    teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
    # Rename column names for readability
    colnames(teacherByRace) <- c(
      "Division Number",
      "Name",
      "Total Counts",
      "American Indian",
      "Asian",
      "Black",
      "Hispanic",
      "White",
      "Hawaiian",
      "Two or More Races",
      "Not Specified",
      "% of Black Teachers",
      "% of Asian Teachers",
      "% of Hispanic Teachers",
      "% of White Teachers",
      "% of American Indian Teachers",
      "% of Two Or More Races Teachers",
      "% of Hawaiian Teachers"
    )
    # Get column name for percentage based on selected race
    percentage_column <- switch(race,
                                "Black" = "% of Black Teachers",
                                "Asian" = "% of Asian Teachers",
                                "White" = "% of White Teachers",
                                "Hispanic" = "% of Hispanic Teachers",
                                "American Indian" = "% of American Indian Teachers",
                                "Two or More Races" = "% of Two Or More Races Teachers",
                                "Hawaiian" = "% of Hawaiian Teachers")
    # Plot the data
    teacherByRace <- teacherByRace %>%
      ggplot(aes(x = Name, y = !!sym(percentage_column), fill = Name)) + 
      geom_col() +
      labs(title = paste(race, "Teacher Breakdown"), y = "Percentage (%)", x = "") + 
      theme(axis.text.x = element_text(angle = 40)) +
      scale_fill_viridis_d(option="rocket")
    # Return the plot
    return(hide_legend(ggplotly(teacherByRace, tooltip = c("x", "y"))))
  }
  
  # Define reactive expression for selecting teacher race breakdown
  var_teacherRaces <- reactive({
    input$teacherRaceBreakdown
  })
  
  # Render plotly plot for teacher race breakdown
  output$teacherRacePlots <- renderPlotly({
    # Check selected teacher race and call function accordingly
    var_teacher_race <- var_teacherRaces()
    plot <- process_teacher_race(var_teacher_race)
    return(plot)
  })
  

  # suspension line graph-------------------------------------------------------
  # Read the suspension data from the Excel file
  suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
  
  # Define the years of interest
  years <- c("2018-2019", "AY 2017-2018", "AY 2016-2017", "AY 2015-2016", "AY 2014-2015")
  
  # Initialize an empty list to store processed data for each year
  suspension_data_list <- list()
  
  # Loop through each year and process the data
  for (year in years) {
    # Extract numeric year from the string
    numeric_year <- as.numeric(str_extract(year, "\\d{4}"))
    
    # Filter the data for Virginia and the current year
    suspension_va <- suspension_data %>%
      filter(Location == "Virginia") %>%
      filter(TimeFrame == year)
    
    # Filter by race and data format (Black, Hispanic, White)
    va_blck <- suspension_va %>%
      filter(Race == "Black") %>%
      filter(DataFormat == "Percent")
    va_hisp <- suspension_va %>%
      filter(Race == "Hispanic") %>%
      filter(DataFormat == "Percent")
    va_white <- suspension_va %>%
      filter(Race == "White") %>%
      filter(DataFormat == "Percent")
    
    # Combine the percentages for Black, Hispanic, and White races
    va_suspension_race <- rbind(va_blck[, 6], va_hisp[, 6], va_white[, 6])
    
    # Convert the data to numeric and scale it to percentages
    va_suspension_race$Data <- as.numeric(va_suspension_race$Data) * 100
    
    # Add race and year columns
    va_suspension_race <- mutate(va_suspension_race, race = c("Black", "Hispanic", "White"))
    va_suspension_race <- mutate(va_suspension_race, year = numeric_year)
    
    # Store the processed data for the current year
    suspension_data_list[[numeric_year]] <- va_suspension_race
  }
  
  # Combine data for all years
  suspension_line <- do.call(rbind, suspension_data_list)
  
  # Rename columns
  colnames(suspension_line) <- c("Percent Suspended", "Race", "Year")
  
  # Create the ggplot object
  suspension_line_graph <- ggplot(suspension_line,
                                  aes(x = Year, y = `Percent Suspended`, group = Race, color = Race)) +
    geom_line(position = "identity", size = 1.5) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 13),
      legend.title = element_blank(),
      legend.text = element_text(size = 13),
      axis.text = element_text(size = 13)
    ) +
    labs(y = "Percent (%)") +
    scale_color_viridis_d(option="rocket") +
    scale_x_continuous(breaks = seq(2015, 2019, by = 1)) + # Set x-axis breaks to numeric years
    scale_y_continuous(limits = c(2, 14), breaks = seq(0, 14, by = 2))
  
  # Convert ggplot to plotly
  suspension_line_plotly <- ggplotly(suspension_line_graph, tooltip = c("x", "y", "group")) %>%
    layout(legend = list(y = 0.5))
  
  # Render the plot
  output$suspension_line_graph <- renderPlotly({
    suspension_line_plotly
  })

  
  

  #suspension gap line graph ------------------------------------------------
  output$suspensionGap <- renderPlotly({
    # Read the gap data from the CSV file
    gap_data <- read.csv("data/suspension/suspensionGap.csv")
    
    # Map the year labels to a simpler format
    gap_data$year[gap_data$year == "2018-2019"] <- "2019"
    gap_data$year[gap_data$year == "2017-2018"] <- "2018"
    gap_data$year[gap_data$year == "2016-2017"] <- "2017"
    gap_data$year[gap_data$year == "2015-2016"] <- "2016"
    gap_data$year[gap_data$year == "2014-2015"] <- "2015"
    
    # Rename columns
    colnames(gap_data) <- c("x", "Location", "Percent Difference", "Year")
    
    # Create the bar graph plot
    susGapPlot <- ggplot(gap_data,
                         aes(x = Year, y = `Percent Difference`, fill = Location)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) + # Use geom_bar for bar graph
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 13)
      )+ scale_fill_manual(values = c(
        "Chesapeake" = "black",
        "Gloucester" = "burlywood",
        "Hampton" = "red",
        "Isle of Wight" = "grey",
        "Newport News" = "antiquewhite",
        "Norfolk" = "cornsilk4",
        "Portsmouth" = "firebrick",
        "Southampton" = "azure",
        "Suffolk" = "indianred2",
        "Virginia Beach" = "slategray2",
        "Williamsburg-James City" = "tan4",
        "York" = "rosybrown"
      ))+
      labs(y = "Percent Difference (%)", fill = "Location") + # Set axis labels and legend title
      ylim(0, 16) # Set y-axis limits
    
    # Convert ggplot to plotly
    suspensionGap <- ggplotly(susGapPlot, tooltip = c("x", "y", "fill")) %>%
      layout(legend = list(y = 0.5))
    
  })

  
  
  
  # Suspension rate by race (counties) ---------------------------------
  
  var_BWsuspension <- reactive({
    input$BWsuspensionYearDrop
  })
  
  output$BW_map <- renderPlotly({
    if (var_BWsuspension() == "2019") {
      year <- "2018-2019"
      suspension_data <-
        read_excel("data/suspension/kidsCountSuspension.xlsx")
      city <-
        c(
          "Chesapeake",
          "Franklin City",
          "Gloucester",
          "Hampton",
          "Isle of Wight",
          "James City",
          "Mathews",
          "Newport News",
          "Norfolk",
          "Poquoson",
          "Portsmouth",
          "Southampton",
          "Suffolk",
          "Virginia Beach",
          "Williamsburg",
          "York"
        )
      suspension_counties <-
        filter(suspension_data, Location %in% city)
      
      pct_white <-
        suspension_counties %>% filter(Race == "White") %>%
        filter(DataFormat == "Percent")
      pct_white2 <- pct_white %>% filter(TimeFrame == year)
      pct_white2$Location[pct_white2$Location == "Williamsburg"] <-
        "Williamsburg-James City"
      pct_white2 <- pct_white2[c(1:5, 7:16), ]
      #putting NAs and Ss in a table
      pct_white2$Data[is.na(pct_white2$Data)] <- "NA"
      display_tbl_white <-
        pct_white2 %>% filter(Data %in% c("NA", "S", "<", "*"))
      display_tbl_white2 <- display_tbl_white[, c(2, 3, 6)]
      pct_white2$Data[pct_white2$Data == "NA"] <- 0
      pct_white2$Data[pct_white2$Data == "S"] <- 0
      pct_white2$Data[pct_white2$Data == "<"] <- 0
      pct_white2$Data[pct_white2$Data == "*"] <- 0
      #adding estimates by 100 (need to convert to numeric first)
      pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
      pct_white2 <- mutate(pct_white2, pct = Data * 100)
      pct_white2$pct <- na_if(pct_white2$pct, 0.00000)
      as.numeric(pct_white2$pct, na.rm = TRUE)
      #labeling
      pct_white3 <- pct_white2[, c(2, 7)]
      colnames(pct_white3) <- c("Location", "Percent (%)")
      #black data
      suspension_pct <-
        suspension_counties %>% filter(Race == "Black") %>%
        filter(DataFormat == "Percent")
      suspension_pct2 <-
        suspension_pct %>% filter(TimeFrame == year)
      suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <-
        "Williamsburg-James City"
      suspension_pct2 <- suspension_pct2[c(1:5, 7:16), ]
      #make a table w/ NA a S
      suspension_pct2$Data[is.na(suspension_pct2$Data)] <- "NA"
      display_tbl_black <-
        suspension_pct2 %>% filter(Data %in% c("NA", "S", "<", "*"))
      display_tbl_black2 <- display_tbl_black[, c(2, 3, 6)]
      suspension_pct2$Data[suspension_pct2$Data == "NA"] <- 0
      suspension_pct2$Data[suspension_pct2$Data == "S"] <- 0
      suspension_pct2$Data[suspension_pct2$Data == "<"] <- 0
      suspension_pct2$Data[suspension_pct2$Data == "*"] <- 0
      #convert data column to numeric so we can multiply by 100
      suspension_pct2$Data <-
        sapply(suspension_pct2$Data, as.numeric)
      suspension_pct2 <- mutate(suspension_pct2, pct = Data * 100)
      suspension_pct2$pct <- na_if(suspension_pct2$pct, 0.00000)
      as.numeric(suspension_pct2$pct, na.rm = TRUE)
      pct_blck <- suspension_pct2[, c(2, 7)]
      colnames(pct_blck) <- c("Location", "Percent (%)")
      sus <- rbind(pct_blck, pct_white3)
      num <- nrow(sus) / 2
      sus <-
        mutate(sus, Race = c(rep("Black", num), rep("White", num)))
      #bar graph
      suspension_counties_plot <-
        ggplot(sus , aes(Location, y = `Percent (%)`, fill = Race)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 25),
          legend.key.size = unit(1, 'cm'),
          legend.key.height = unit(0.3, 'cm'),
          legend.key.width = unit(0.3, 'cm'),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title.y = element_text(size = 13),
          axis.title = element_text(size = 17),
          axis.title.x = element_blank()
        ) +
        theme(axis.text.x = element_text(
          angle = 40,
          vjust = 0.95,
          hjust = 1
        )) +
        scale_fill_manual(values = c("#8B1A1A", "#C1CDCD")) +
        labs(x = "Location")
      
      #plot
      BW_map <- suspension_counties_plot
      ggplotly(BW_map)
      
    }
    else if (var_BWsuspension() %in% c("2018", "2017", "2016", "2015")) {
      if (var_BWsuspension() == "2018") {
        year <- "AY 2017-2018"
      }
      else if (var_BWsuspension() == "2017") {
        year <- "AY 2016-2017"
      }
      else if (var_BWsuspension() == "2016") {
        year <- "AY 2015-2016"
      }
      else if (var_BWsuspension() == "2015") {
        year <- "AY 2014-2015"
      }
      suspension_data <-
        read_excel("data/suspension/kidsCountSuspension.xlsx")
      city <-
        c(
          "Chesapeake",
          "Franklin City",
          "Gloucester",
          "Hampton",
          "Isle of Wight",
          "James City",
          "Mathews",
          "Newport News",
          "Norfolk",
          "Poquoson",
          "Portsmouth",
          "Southampton",
          "Suffolk",
          "Virginia Beach",
          "Williamsburg",
          "York"
        )
      suspension_counties <-
        filter(suspension_data, Location %in% city)
      
      pct_white <-
        suspension_counties %>% filter(Race == "White") %>%
        filter(DataFormat == "Percent")
      pct_white2 <- pct_white %>% filter(TimeFrame == year)
      pct_white2$Location[pct_white2$Location == "James City"] <-
        "Williamsburg-James City"
      pct_white2$Location[pct_white2$Location == "Williamsburg"] <-
        "Williamsburg-James City"
      #putting NAs and Ss in a table
      pct_white2$Data[is.na(pct_white2$Data)] <- "NA"
      display_tbl_white <-
        pct_white2 %>% filter(Data %in% c("NA", "S", "<", "*"))
      display_tbl_white2 <- display_tbl_white[, c(2, 3, 6)]
      pct_white2$Data[pct_white2$Data == "NA"] <- 0
      pct_white2$Data[pct_white2$Data == "S"] <- 0
      pct_white2$Data[pct_white2$Data == "<"] <- 0
      pct_white2$Data[pct_white2$Data == "*"] <- 0
      #adding estimates by 100 (need to convert to numeric first)
      pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
      pct_white2 <- mutate(pct_white2, pct = Data * 100)
      pct_white2$pct <- na_if(pct_white2$pct, 0.00000)
      as.numeric(pct_white2$pct, na.rm = TRUE)
      #labeling
      pct_white3 <- pct_white2[, c(2, 7)]
      colnames(pct_white3) <-
        c("Location", "Percentage of Students (%)")
      #black data
      suspension_pct <-
        suspension_counties %>% filter(Race == "Black") %>%
        filter(DataFormat == "Percent")
      suspension_pct2 <-
        suspension_pct %>% filter(TimeFrame == year)
      suspension_pct2$Location[suspension_pct2$Location == "James City"] <-
        "Williamsburg-James City"
      suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <-
        "Williamsburg-James City"
      #make a table w/ NA a S
      suspension_pct2$Data[is.na(suspension_pct2$Data)] <- "NA"
      display_tbl_black <-
        suspension_pct2 %>% filter(Data %in% c("NA", "S", "<", "*"))
      display_tbl_black2 <- display_tbl_black[, c(2, 3, 6)]
      suspension_pct2$Data[suspension_pct2$Data == "NA"] <- 0
      suspension_pct2$Data[suspension_pct2$Data == "S"] <- 0
      suspension_pct2$Data[suspension_pct2$Data == "<"] <- 0
      suspension_pct2$Data[suspension_pct2$Data == "*"] <- 0
      #convert data column to numeric so we can multiply by 100
      suspension_pct2$Data <-
        sapply(suspension_pct2$Data, as.numeric)
      suspension_pct2 <- mutate(suspension_pct2, pct = Data * 100)
      suspension_pct2$pct <- na_if(suspension_pct2$pct, 0.00000)
      as.numeric(suspension_pct2$pct, na.rm = TRUE)
      pct_blck <- suspension_pct2[, c(2, 7)]
      colnames(pct_blck) <-
        c("Location", "Percentage of Students (%)")
      sus <- rbind(pct_blck, pct_white3)
      num <- nrow(sus) / 2
      sus <-
        mutate(sus, Race = c(rep("Black", num), rep("White", num)))
      #bar graph
      suspension_counties_plot <-
        ggplot(sus ,
               aes(Location, y = `Percentage of Students (%)`, fill = Race)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_minimal() +
        ylab("Percent (%)") +
        theme(
          plot.title = element_text(hjust = 0.5, size = 25),
          legend.key.size = unit(1, 'cm'),
          legend.key.height = unit(0.3, 'cm'),
          legend.key.width = unit(0.3, 'cm'),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 13),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 17),
          axis.title.x = element_blank()
        ) +
        theme(axis.text.x = element_text(
          angle = 40,
          vjust = 0.95,
          hjust = 1
        )) +
        scale_fill_manual(values = c("#8B1A1A", "#C1CDCD")) +
        labs(x = "Location")
      #plot
      BW_map <- suspension_counties_plot
      ggplotly(BW_map)
    }
  })
  
  
  # On Time Graduation Rates -----------------------------------------------------------

  # Load the data
  rates <- read_csv("data/on_time_graduation.csv")
  rates <- rates %>%
    na.omit(rates)
  
  # Create a basemap
  basemap <- leaflet(width = "100%", height = "400px") %>%
    addProviderTiles("CartoDB.Positron")
  
  # Define colors for the minicharts
  colors <- c("#A9A9A9", "#8B0000")
  
  # Render the leaflet map
  output$dropout_map <- renderLeaflet({
    # Add minicharts to the basemap
    grad_map <- basemap %>%
      addMinicharts(
        rates$lon,
        rates$lat,
        chartdata = round(rates[, c("Black Students", "All Students")], 1), # Round the numbers to 1 decimal place
        time = rates$`Cohort Year`,
        colorPalette = colors
      )
  }) 
  
  # var_dropoutrate <- reactive({
  #   input$DropoutDropdown
  # })
  #
  # output$dropout_map <- renderLeaflet({
  #   if(var_dropoutrate() == "2020") {
  #     dropout_20 <- read.csv("data/dropout2020.csv")
  #     mapping <- read.csv("data/dropoutmapdata.csv")
  #     colors <- c("#0072B2", "#D55E00")
  #     dropout_20_map <- leaflet() %>%
  #       addProviderTiles("CartoDB.Voyager") %>%
  #       addMinicharts(
  #         mapping$lon, mapping$lat,
  #         chartdata = dropout_20,
  #         colorPalette = colors,
  #         width = 45, height = 45
  #       )
  #   }
  # })
  
  # Median Income plots: Working on it --------------------------------
  # var_medianIncome <- reactive({
  #   input$MedianIncomeYearDrop
  # })
  # 
  # output$income_plot <- renderPlot({
  #   if (var_medianIncome() %in% c("2019", "2018", "2017")) {
  #     if (var_medianIncome() == "2019") {
  #       va_yr <- read.csv("data/income/va_income2019.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2019.csv")
  #     }
  #     else if (var_medianIncome() == "2018") {
  #       va_yr <- read.csv("data/income/va_income2018.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2018.csv")
  #     }
  #     else if (var_medianIncome() == "2017") {
  #       va_yr <- read.csv("data/income/va_income2017.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2017.csv")
  #     }
  #     va_yr <- va_yr[2:6]
  #     race_names <- c("Total", "Black")
  #     #VA income
  #     va_race_income_median <- data.frame(va_yr[c(81, 83), 4])
  #     va_race_income <-
  #       data.frame(cbind(race_names, va_race_income_median))
  #     colnames(va_race_income) <- c("Race", "Median Income")
  #     #Hampton Income
  #     hamp_yr <- hamp_yr[2:6]
  #     #getting the name, variable and estimate
  #     hamp_income2 <- hamp_yr[, 2:4]
  #     hamp_income3 <- hamp_income2 %>%
  #       group_by(NAME) %>%
  #       slice(c(81, 83))
  #     variable <-
  #       sample(c("S1903_C03_001", "S1903_C03_003"), 32, replace = TRUE)
  #     hamp_race_income_median <-
  #       hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
  #     #Putting them together
  #     median_income <-
  #       cbind(va_race_income, hamp_race_income_median)
  #     median_income <- median_income[, c(2, 4)]
  #     #having all the estimates in the same column
  #     median_income2 <-
  #       data.frame(median = c(median_income[, 1], median_income[, 2]))
  #     #labeling
  #     median_income2 <-
  #       mutate(median_income2, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
  #     median_income2 <-
  #       mutate(median_income2, demo = rep(c(
  #         "Total Population", "Black Population"
  #       ), 2))
  #     colnames(median_income2) <-
  #       c("Median Income (US Dollars)", "Location", "Demographic")
  #     #making them all numeric
  #     median_income2 <-
  #       transform(median_income2,
  #                 `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
  #     colnames(median_income2) <-
  #       c("Median Income (US Dollars)", "Location", "Demographic")
  #     median_income2$Location <-
  #       factor(median_income2$Location,
  #              levels = c("Hampton Roads", "Virginia"))
  #     #Graph
  #     income_plot <-
  #       ggplot(
  #         median_income2,
  #         aes(x = Location, y = `Median Income (US Dollars)`, fill = Demographic)
  #       ) +
  #       geom_bar(stat = "identity", position = position_dodge()) +
  #       geom_text(
  #         aes(label = paste0(round(
  #           `Median Income (US Dollars)`
  #         ))),
  #         vjust = 1.5,
  #         color = "white",
  #         position = position_dodge(0.9),
  #         size = 5
  #       ) +
  #       theme_minimal() +
  #       theme(
  #         plot.title = element_text(hjust = 0.5, size = 25),
  #         legend.key.size = unit(1, 'cm'),
  #         legend.title = element_blank(),
  #         legend.key.height = unit(1, 'cm'),
  #         legend.key.width = unit(1, 'cm'),
  #         legend.text = element_text(size = 14),
  #         axis.text = element_text(size = 15),
  #         axis.title = element_text(size = 17),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_text(margin = margin(
  #           t = 0,
  #           r = 20,
  #           b = 0,
  #           l = 0
  #         ))
  #       ) +
  #       scale_fill_manual(values = c("#D55E00", "#0072B2"))
  #     #plot
  #     income_plot
  #   }
  #   else if (var_medianIncome() %in% c("2016", "2015", "2014", "2013", "2012", "2011", "2010")) {
  #     if (var_medianIncome() == "2016") {
  #       va_yr <- read.csv("data/income/va_income2016.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2016.csv")
  #     }
  #     else if (var_medianIncome() == "2015") {
  #       va_yr <- read.csv("data/income/va_income2015.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2015.csv")
  #     }
  #     else if (var_medianIncome() == "2014") {
  #       va_yr <- read.csv("data/income/va_income2014.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2014.csv")
  #     }
  #     else if (var_medianIncome() == "2013") {
  #       va_yr <- read.csv("data/income/va_income2013.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2013.csv")
  #     }
  #     else if (var_medianIncome() == "2012") {
  #       va_yr <- read.csv("data/income/va_income2012.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2012.csv")
  #     }
  #     else if (var_medianIncome() == "2011") {
  #       va_yr <- read.csv("data/income/va_income2011.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2011.csv")
  #     }
  #     else if (var_medianIncome() == "2010") {
  #       va_yr <- read.csv("data/income/va_income2010.csv")
  #       hamp_yr <- read.csv("data/income/hampton_income2010.csv")
  #     }
  #     
  #     va_yr <- read.csv("data/income/va_income2016.csv")
  #     va_yr <- va_yr[, 2:6]
  #     #income by Race
  #     race_names <- c("Total", "Black")
  #     #median income
  #     va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
  #     va_race_income <-
  #       data.frame(cbind(race_names, va_race_income_median))
  #     colnames(va_race_income) <- c("Race", "Median Income")
  #     #Hampton Income
  #     #getting the name, variable and estimate
  #     hamp_income2 <- hamp_yr[, 2:6]
  #     hamp_income3 <- hamp_income2 %>%
  #       group_by(NAME) %>%
  #       slice(c(31, 33))
  #     variable <-
  #       sample(c("S1903_C02_001", "S1903_C02_003"), 32, replace = TRUE)
  #     hamp_race_income_median <-
  #       hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
  #     #Putting them in the same dataset
  #     median_income <-
  #       cbind(va_race_income, hamp_race_income_median)
  #     median_income <- median_income[, c(2, 4)]
  #     #having all the estimates in the same column
  #     median_income2 <-
  #       data.frame(median = c(median_income[, 1], median_income[, 2]))
  #     median_income2 <-
  #       mutate(median_income2, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
  #     median_income2 <-
  #       mutate(median_income2, demo = rep(c(
  #         "Total Population", "Black Population"
  #       ), 2))
  #     colnames(median_income2) <-
  #       c("Median Income (US Dollars)", "Location", "Demographic")
  #     #making them all numeric
  #     median_income2 <-
  #       transform(median_income2,
  #                 `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
  #     colnames(median_income2) <-
  #       c("Median Income (US Dollars)", "Location", "Demographic")
  #     median_income2$Location <-
  #       factor(median_income2$Location,
  #              levels = c("Hampton Roads", "Virginia"))
  #     #Hampton and VA graph
  #     income_plot <-
  #       ggplot(
  #         median_income2,
  #         aes(x = Location, y = `Median Income (US Dollars)`, fill = Demographic)
  #       ) +
  #       geom_bar(stat = "identity", position = position_dodge()) +
  #       geom_text(
  #         aes(label = paste0(round(
  #           `Median Income (US Dollars)`
  #         ))),
  #         vjust = 1.5,
  #         color = "white",
  #         position = position_dodge(0.9),
  #         size = 5
  #       ) +
  #       theme_minimal() +
  #       theme(
  #         plot.title = element_text(hjust = 0.5, size = 25),
  #         legend.key.size = unit(1, 'cm'),
  #         legend.title = element_blank(),
  #         legend.key.height = unit(1, 'cm'),
  #         legend.key.width = unit(1, 'cm'),
  #         legend.text = element_text(size = 14),
  #         axis.text = element_text(size = 15),
  #         axis.title = element_text(size = 17),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_text(margin = margin(
  #           t = 0,
  #           r = 20,
  #           b = 0,
  #           l = 0
  #         ))
  #       ) +
  #       scale_fill_manual(values = c("#D55E00", "#0072B2"))
  #     #plot
  #     income_plot
  #   }
  #   
  #   
  # })
  
  # Median Income line plots -------------------------------------------------
  process_income_data <- function(year) {
    # VA file paths based on the year
    va_yr <- read.csv(sprintf("data/income/va_income%d.csv", year))
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    
    #median income
    if(year == "2016" || year == "2015" || year == "2014" || year == "2013" || 
       year == "2012" || year == "2011" || year == "2010") {
      va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
    }else {
      va_race_income_median <- data.frame(va_yr[c(81, 83), 4])
    }
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    
    #Hampton Income
    hamp_yr <- read.csv(sprintf("data/income/hampton_income%d.csv", year))
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    if(year == "2016" || year == "2015" || year == "2014"|| year == "2013" 
       || year == "2012" || year == "2011" || year == "2010") {
      hamp_income3 <- hamp_income2 %>% group_by(NAME) %>% slice(c(31, 33))
    }else {
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
    
    ### CREATE BIG GRAPH SHOWCASING TRENDS IN MEDIAN INCOME ####################
  output$medianTimeGraph <- renderPlot ({
    income_data_list <- list()
    years <- 2010:2019
    # loop through the years from 2010 to 2019
    for(year in years) {
      income_data_list[[as.character(year)]] <- process_income_data(year)
    }
    # combine all the data frames from the list into one data frame
    income_years <- do.call(rbind, income_data_list)
    
    ### CREATE BIG GRAPH SHOWCASING TRENDS IN MEDIAN INCOME ####################
    va_total <- income_years %>% filter(Location == "Virginia" & Demographic == "Total Population")
    hr_total <- income_years %>% filter(Location == "Hampton Roads" & Demographic == "Total Population")
    va_black <- income_years %>% filter(Location == "Virginia" & Demographic == "Black Population")
    hr_black <- income_years %>% filter(Location == "Hampton Roads" & Demographic == "Black Population")
    LINES <- c("Virginia" = "solid", "Hampton Roads" = "dashed")
    
    incomeGraph <- ggplot(income_years, aes(x = Year, y = `Median Income (US Dollars)`, color = Demographic, group = Location, linetype = Location)) + 
      geom_line(data = va_total, size = 1.3, aes(linetype = Location)) +
      geom_line(data = va_black, size = 1.3, aes(linetype = Location)) +
      geom_line(data = hr_total, size = 1.3, aes(linetype = Location)) +
      geom_line(data = hr_black, size = 1.3, aes(linetype = Location)) +
      scale_color_manual(name = "Population", values = c("black", "#800404")) +
      scale_linetype_manual(name = "Location", values = c("dashed", "solid")) +
      scale_x_continuous(breaks = unique(income_years$Year), 
                         labels = unique(as.integer(income_years$Year))) +
      theme_minimal() + theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12)
      ) +
      labs(x = "Year", y = "Median Income (US Dollars)")
    incomeGraph
  })
  
  # Home Ownership Map -------------------------------------------------------
  # Reactive expression to get the selected value from the HomeOwnSlider input
  var_hmown <- reactive({
    input$HomeOwnSlider
  })
  
  # Render Leaflet map
  output$homeownership_map <- renderLeaflet({
    # Read data for black homeowners in 2019
    b_hm_19 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2019.rds")
    # Read data for total homeowners in 2019
    tot_hm_19 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2019.rds")
    # Read all home data
    all_hm_data <- read_rds("data/TableS2502FiveYearEstimates/allhomedata.rds")
    # Rename column for demographic
    colnames(all_hm_data)[2] <- "Demographic"
    
    # Function to create line plots for each locality
    pick_n <- function(Locality) {
      # Filter data for the selected locality
      dataFiltered <- filter(all_hm_data, NAME == Locality)
      
      # Create ggplot line plot
      hm_line <- ggplot(dataFiltered,
                        aes(
                          x = Year,
                          y = Percent,
                          color = Demographic,
                          group = Demographic
                        )) +
        geom_line(position = "identity") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_discrete(name = "",
                            labels = c("Black Home Owners", "White Home Owners")) +
        scale_fill_manual(values = c("#A9A9A9", "#8B0000")) +
        theme(legend.position = "bottom") +
        labs(title = Locality)
      
      # Return ggplot object
      return(hm_line)
    }
    
    # Apply pick_n function to each locality in the data
    r <- lapply(1:length(unique(b_hm_19$NAME)), function(i) {
      pick_n(b_hm_19$NAME[i])
    })
    
    # Create color palette for choropleth map
    pal <- colorNumeric(palette = "Reds",
                        domain = b_hm_19$Percent,
                        reverse = TRUE)
    
    # Create Leaflet map object
    b_hmown_leaf_19 <- b_hm_19 %>%
      leaflet(options = leafletOptions(
        minZoom = 5,
        maxZoom = 15,
        drag = FALSE
      )) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(
        data = b_hm_19,
        color = ~ pal(Percent),
        weight = 0.5,
        fillOpacity = 0.7,
        smoothFactor = 0,
        highlightOptions = highlightOptions(
          bringToFront = TRUE,
          opacity = 1.5,
          weight = 3
        ),
        label = ~ paste0(NAME,  " Black Homeowners: ", Percent, "%"),
        group = "Black Home Owners",
        popup = popupGraph(r)
      ) %>%
      addPolygons(
        data = tot_hm_19,
        color = ~ pal(Percent),
        weight = 0.5,
        fillOpacity = 0.7,
        smoothFactor = 0,
        highlightOptions = highlightOptions(
          bringToFront = TRUE,
          opacity = 1.5,
          weight = 3
        ),
        label = ~ paste0(NAME,  " Total Homeowners: ", Percent, "%"),
        group = "Total Home Owners",
        popup = popupGraph(r)
      ) %>%
      addLayersControl(
        baseGroups = c("Total Home Owners"),
        overlayGroups = c("Black Home Owners"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Black Home Owners") %>%
      addLegend(
        "topleft",
        pal = pal,
        values = ~ Percent,
        title = "Home Owners",
        labFormat = labelFormat(suffix = "%"),
        opacity = 1
      )
  })
  
  # Employment By Sector ----------------------------------------------------
  
  # Function to read CSV data, preprocess, and generate plot for a specific year
  read_and_plot_sectors <- function(year) {
    # Construct file path for the CSV file based on the selected year
    file_path <- paste0("data/TableDP03FiveYearEstimates/top2employmentSectors", year, ".csv")
    
    # Check if the file exists
    if (file.exists(file_path)) {
      # Read CSV file
      sectors_data <- read.csv(file_path)
      
      # Rename columns for better readability
      colnames(sectors_data) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      
      # Data preprocessing: remove "County, Virginia" and "city, Virginia" from the 'Name' column
      sectors_data <- sectors_data %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        
        # Create ggplot object
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + 
        geom_col() +  # Plot columns
        theme_minimal() +  # Minimal theme
        labs(title = "", y = "Total Number of People Employed", x = "") +  # Set titles for plot
        theme(axis.text.x = element_text(angle = 40)) +  # Rotate x-axis labels for better readability
        scale_fill_viridis_d(option="F")  # Use Viridis color scale for better visualization
      
      # Further customization of plot aesthetics
      sectors_data <- sectors_data +
        ggtitle(paste("Top 2 Employment Sectors in", year)) +  # Add plot title
        xlab("Location") +  # Label for x-axis
        ylab("Number of People Employed")  # Label for y-axis
      
      # Convert ggplot object to plotly object and hide legend for better visualization
      return(hide_legend(ggplotly(sectors_data, tooltip = c("x", "y", "Sector"))))
    } else {
      # Return NULL if the file does not exist
      return(NULL)
    }
  }
  # Reactive expression to get the selected year from the input dropdown
  var_sectorEmployment <- reactive({
    input$SectorEmploymentYearDrop
  })
  
  # Render the plotly plot
  output$sector_plot <- renderPlotly({
    # Call the read_and_plot_sectors function with the selected year from the reactive expression
    read_and_plot_sectors(var_sectorEmployment())
  })
  
  # Unemployment Rate -------------------------------------------------------
  # Function to generate unemployment plot for a specific year
  generate_unemployment_plot <- function(year) {
    # Read unemployment data for the specified year
    unemp_data <- read.csv(paste0("data/TableS2301FiveYearEstimates/unemployment", year, ".csv"))
    colnames(unemp_data)[2] <- "Locality"
    colnames(unemp_data)[3] <- "Demographic"
    colnames(unemp_data)[4] <- "Unemployment Rate"
    
    # Read Virginia unemployment data for the specified year
    va_unemp_data <- read.csv(paste0("data/TableS2301FiveYearEstimates/vaunemployment", year, ".csv"))
    
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
  
  # Reactive expression for selected unemployment rate year
  var_unemploymentRate <- reactive({
    input$UnemploymentRateSlider  # Retrieve selected year from slider input
  }) 
  
  # Render the unemployment plot using plotly
  output$unemployment_plot <- renderPlotly({
    year <- var_unemploymentRate()  # Get the selected year
    generate_unemployment_plot(year)  # Generate the plot for the selected year
  })
  
  
  
  # Poverty Rates in VA and Hampton Roads-------------------------------------
 
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
  
  # Define a reactive expression to capture the selected year from the dropdown
  var_poverty <- reactive({
    input$PovertyYearDrop
  })
  
  
  # Render plot based on selected year
  output$pov_plot <- renderPlot({
    # Check the selected year and load corresponding data
    if (var_poverty() %in% c("2019", "2018", "2017", "2016", "2015")) {
      year <- var_poverty()
      va_file <- paste0("data/poverty/va_poverty", year, ".csv")
      hamp_file <- paste0("data/poverty/hamp_poverty", year, ".csv")
      pov_data <- read_process_csv(va_file, hamp_file, c(123, 136))
    } else if (var_poverty() %in% c("2014", "2013", "2012")) {
      year <- var_poverty()
      va_file <- paste0("data/poverty/va_poverty", year, ".csv")
      hamp_file <- paste0("data/poverty/hamp_poverty", year, ".csv")
      pov_data <- read_process_csv(va_file, hamp_file, c(93, 102))
    }
    
    # Generate the plot using ggplot2
    pov_plot <- ggplot(pov_data, aes(x = Location, y = `Percentage (%)`, fill = Demographic)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = paste0(round(`Percentage (%)`, digits = 2), "%")),
                vjust = 1.5, color = "white", position = position_dodge(0.9), size = 5) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 25),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
      ) +
      scale_fill_manual(values = c("#A9A9A9", "#8B0000")) +
      labs(title = paste("Poverty Rate (%) - Year", year)) # Add title with selected year
    
    pov_plot # Render the plot
  })
  
 
  
  
  # Hampton counties poverty -------------------------------------------------------------
  # Define a reactive expression to capture the selected year from the dropdown
  var_povertyCount <- reactive({
    input$PovertyCountYearDrop
  })
  
  # Function to load and process CSV files for selected years
  load_process_csv <- function(year) {
    hamp_file <- paste0("data/poverty/hamp_poverty", year, ".csv")
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
  
  # Render Plotly plot based on selected year
  output$counties_pov <- renderPlotly({
    # Load and process data based on selected year
    hamp_data <- switch(var_povertyCount(),
                        "2019" = load_process_csv("2019"),
                        "2018" = load_process_csv("2018"),
                        "2017" = load_process_csv("2017"),
                        "2016" = load_process_csv("2016"),
                        "2015" = load_process_csv("2015"),
                        "2014" = load_process_csv("2014"),
                        "2013" = load_process_csv("2013"),
                        "2012" = load_process_csv("2012")
    )
    
    # Generate plot
    plot <- generate_plot(hamp_data)
    
    # Convert ggplot to Plotly plot
    ggplotly(plot)
  })
  
  
  
  # Uninsured Population ----------------------------------------------------
  
  # Function to load and process uninsured data
  load_process_uninsured <- function(year) {
    file_path <- paste0("data/TableS2701FiveYearEstimates/uninsured", year, ".csv")
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
  
  # Define a reactive expression to retrieve the selected year from the UninsuredPctSlider input
  var_uninsuredpct <- reactive({
    input$UninsuredPctSlider
  })
  
   # Render Plotly plot based on selected year
  output$uninsured_plot <- renderPlotly({
    # Load and process data based on selected year
    unins_data <- load_process_uninsured(var_uninsuredpct())
    
    # Generate plot
    plot <- generate_uninsured_plot(unins_data)
    
    # Convert ggplot to Plotly plot
    ggplotly(plot)
  })
  
  
  
  # Veteran Status ----------------------------------------------------------
  # Define a reactive expression to retrieve the selected year from the VeteranSlider input
  var_veteran <- reactive({
    input$VeteranSlider
  })
  
  # Render Leaflet map based on the selected year
  output$veteran_map <- renderLeaflet({
    # Check the selected year and load corresponding data
    if (var_veteran() %in% c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")) {
      # Load data for the selected year
      vet_data <- readRDS(paste0("data/TableS2101FiveYearEstimates/bveteran", var_veteran(), ".rds"))
      military_bases <- readRDS("data/TableS2101FiveYearEstimates/militarybases.rds")
      # Define color palette
      pal <- colorNumeric(
        palette = "Reds",
        domain = vet_data$Percent,
        reverse = TRUE
      )
      # Create Leaflet map
      veteran_map <- vet_data %>%
        leaflet(options = leafletOptions(minZoom = 8)) %>%
        addProviderTiles("CartoDB.PositronNoLabels") %>%
        addPolygons(
          color = ~ pal(Percent),
          weight = 0.5,
          fillOpacity = 0.7,
          smoothFactor = 0,
          highlightOptions = highlightOptions(
            bringToFront = TRUE,
            opacity = 1.5,
            weight = 3
          ),
          label = ~ paste0(NAME,  " Black Veterans: ", Percent, "%"),
          group = "Veteran Status"
        ) %>%
        addMarkers(
          data = military_bases,
          popup = ~ paste0("Base: ", base_name, " Branch: ", branch),
          group = "Military Bases"
        ) %>%
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Military Bases") %>%
        addLegend(
          "topleft",
          pal = pal,
          values = ~ Percent,
          title = "Black Veterans",
          labFormat = labelFormat(suffix = "%"),
          opacity = 1
        )
      return(veteran_map)
    }
  })
  
  
     
  
  # Combine two dashboards ----------------------------------------------------------
  state_summary_metrics <- read_csv("data/success_summary_metrics_state.csv")
  metro_summary_metrics <- read_csv("data/success_summary_metrics_metro.csv")
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
  
  # Dynamic UI for state or metro selection
  output$dynamicSelectInput <- renderUI({
    if(input$selectionType == "state") {
      selectInput("state", "Select State:", choices = state_names)
    } else {
      selectInput("metroArea", "Select Metropolitan Area:", choices = metro_names)
    }
  })
  
  # Reactive expression to filter data based on selection
  filtered_data <- reactive({
    if(input$selectionType == "state") {
      selected_state <- input$state
      state_summary_metrics %>% filter(NAME == selected_state)
    } else {
      selected_metro <- input$metroArea
      metro_summary_metrics %>% filter(NAME == selected_metro)
    }
  })
  
  output$plot <- renderPlot({
    req(input$metrics)  # Ensure that a metric is selected
    data_to_plot <- filtered_data()
    
    selected_metric <- input$metrics
    
    # Define y-axis labels based on the selected metric
    y_label <- switch(selected_metric,
                      "Total_Avg_Annual_Pay_Total" = "Average Annual payroll ($1,000) per a firm",
                      "Total_Avg_Annual_Pay_Black_Business" = "Average Annual payroll ($1,000) per a firm",
                      "Total_Avg_Employees_Total" = "Average Employee per a firm",
                      "Total_Avg_Employees_Black_Business" = "Average Employee per a firm",
                      "Total_Sum_of_Firms_Total" = "Number of firms",
                      "Total_Sum_of_Firms_Black_Business" = "Number of firms",
                      "Pay_Annual_Per_Employee_Total" = "Average take home pay for an employee",
                      "Pay_Annual_Per_Employee_Black_Business" = "Average take home pay for an employee",
                      "Percent_Total_Avg_Annual_Pay_BB" = "Percent Black Business in Total Statistic",
                      "Percent_Total_Avg_Employees_BB" = "Percent Black Business in Total Statistic",
                      "Percent_Total_Sum_of_Firms_BB" = "Percent Black Business in Total Statistic")
    
    
    title <- metric_titles[selected_metric]  # Get the title from the mapping
    if(selected_metric %in% c("Pay_Annual_Per_Employee_Total", "Pay_Annual_Per_Employee_Black_Business")) {
      data_to_plot <- data_to_plot %>%
        mutate(Pay_Annual_Per_Employee_Total = Total_Avg_Annual_Pay_Total / Total_Avg_Employees_Total,
               Pay_Annual_Per_Employee_Black_Business = Total_Avg_Annual_Pay_Black_Business / Total_Avg_Employees_Black_Business)
      
    }
    # Handle new percentage metrics
    if(selected_metric == "Percent_Total_Avg_Annual_Pay_BB") {
      data_to_plot <- data_to_plot %>%
        mutate(Percent_Total_Avg_Annual_Pay_BB = (Total_Avg_Annual_Pay_Black_Business / Total_Avg_Annual_Pay_Total) * 100)
    } else if(selected_metric == "Percent_Total_Avg_Employees_BB") {
      data_to_plot <- data_to_plot %>%
        mutate(Percent_Total_Avg_Employees_BB = (Total_Avg_Employees_Black_Business / Total_Avg_Employees_Total) * 100)
    } else if(selected_metric == "Percent_Total_Sum_of_Firms_BB") {
      data_to_plot <- data_to_plot %>%
        mutate(Percent_Total_Sum_of_Firms_BB = (Total_Sum_of_Firms_Black_Business / Total_Sum_of_Firms_Total) * 100)
    }
    
    # # Check if rate of change is selected
    # if(input$rateOfChange) {
    #   # Calculate rate of change
    #   data_to_plot <- data_to_plot %>%
    #     arrange(Year) %>%
    #     mutate(RateOfChange = c(NA, diff(get(selected_metric))))
    #   
    #   plot_title <- paste(title, "- Rate of Change")
    #   y_label <- "Rate of Change"
    #   plot_metric <- "RateOfChange"
    # } else {
    #   plot_title <- title
    #   y_label <- selected_metric
    #   plot_metric <- selected_metric
    # }
    selected_area <- if(input$selectionType == "state") input$state else input$metroArea
    plot_title <- paste(title, "\nin", selected_area)
    
    # Calculate the slope (difference) between consecutive points
    data_to_plot <- data_to_plot %>%
      arrange(Year) %>%
      mutate(Slope = c(NA, diff(as.numeric(get(selected_metric)))),
             SlopeType = ifelse(Slope > 0, "Positive", ifelse(Slope < 0, "Negative", "Flat")))
    
    # Shift the SlopeType column up by one row
    data_to_plot$SlopeType <- c(data_to_plot$SlopeType[-1], NA)
    
    # Create the plot with trend line
    p <- ggplot(data_to_plot, aes_string(x = "Year", y = selected_metric, group = "1")) +
      geom_line(aes(color = SlopeType)) +  # Color lines based on slope type
      scale_color_manual(values = c("Positive" = "black", "Negative" = "red", "Flat" = "grey")) +
      labs(title = plot_title, x = "Year", y = y_label) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),  # Center the title
            plot.margin = margin(10, 10, 10, 10),  # Adjust margins if needed
            legend.position = "bottom")  # Position the legend at the bottom
    
    
    # Custom function to format y-axis as percentages
    percent_format <- function(x) {
      paste0(format(x, nsmall = 1), "%")
    }
    
    # Apply custom percent format to y-axis for percentage metrics
    if(selected_metric %in% c("Percent_Total_Avg_Annual_Pay_BB", "Percent_Total_Avg_Employees_BB", "Percent_Total_Sum_of_Firms_BB")) {
      p <- p + scale_y_continuous(labels = percent_format)
    }
    
    
    
    
    # Render the plot
    p
  })


  
  # People & Values ---------------------------------------------------------
  #Family Dynamic ---------------------------------------------------------
  generateMap <- function(data, title, labelSuffix = "%") {
    pal <- colorNumeric(palette = "Reds", domain = data$Percent, reverse = TRUE)
    data %>% 
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                  highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                  label = ~ paste0(NAME, " - ", title, ": ", Percent, labelSuffix)) %>%
      addLegend("topleft",pal = pal,values = ~ Percent,title = title,
                labFormat = labelFormat(suffix = labelSuffix),opacity = 1)
  }
  
  # use reactive for selected family
  var_fam <- reactive({
    input$select_family
  })
  
  # render the map based on the selected family
  output$family_maps <- renderLeaflet({
    req(var_fam()) # ensure that var_fam is not NULL
    # define a list to map selection values to file names
    fileMap <- list(
      "Percent of Black Children under 18 in Female Head of Household" = "fml.rds",
      "Percent of Black Grandparents who are Guardians" = "grand.rds",
      "Percent of Married Black Population 15 years and over" = "married.rds"
    )
    # get the file name based on the selection
    fileName <- fileMap[[var_fam()]]
    if (!is.null(fileName)) {
      data <- read_rds(paste0("data/", fileName))
      data <- na.omit(data)
      colnames(data)[4] <- "Percent"
      
      # set title based on the variable chosen
      title <- ifelse(var_fam() == "Percent of Black Children under 18 in Female Head of Household",
                      "Female HOH",
                      ifelse(var_fam() == "Percent of Black Grandparents who are Guardians",
                             "Grandparent Guardian",
                             "Married"))
      # generate the map
      generateMap(data, title)
    }
  })
  
  var_famtext <- reactive({
    input$select_family
  })
  
  output$description_famtext <- renderText({
    if(var_famtext() == "Percent of Black Children under 18 in Female Head of Household"){
      "Percentage of Black Children under the age of 18 that live in a female-headed household. We included this indicator as research has shown that female-headed households have
    a greater risk of poverty and are more likely to be food-insecure. In Hampton Roads, regardless of location, majority of Black households with children under 18 have a female as the head.  For 7 of the 11 areas, over 50% of Black households 
    for which data is available, is led by a female. This may suggest some family instability for half of the black children in the Hampton Roads region."
    }
    
    else if(var_famtext() == "Percent of Black Grandparents who are Guardians"){
      "Percent distribution of Black grandparents who live with grandchildren who are responsible for said grandchildren.
    Grandparents becoming principal guardians for their grandchildren suggest economic distress for families, as such, we included this indicator in our analysis.
    There are some differences in this distribution across the cities and counties in Hampton Roads.  For example, of those Black grandparents who live 
    with their own grandchildren, 80% of them are responsible for their grandchildren in Franklin, whereas in Gloucester, the rate is a low 4.8%."   
      
    }
    
    else if(var_famtext() == "Percent of Married Black Population 15 years and over"){
      "The percentage of the Black population 15 years and over who are married. The literature shows that married households tend to be less impoverished and are more 
    economically stable and stable. Except for York, Gloucester, Chesapeake (about 50%), there is a low marriage rate among the Black population. Marriage rates range from as low as 
    20% (Norfolk) to 51% (Chesapeake), with the average rate being around 35%. "
      
    }
  })
  # End Family Dynamic ---------------------------------------------------------
  
  # Religion ---------------------------------------------------------
  var_religion <- reactive({
    input$select_rel
  })
  
  output$religion <- renderLeaflet({
    geo_data <- readRDS('./data/geo_data.rds')
    geo_data <- st_transform(geo_data)
    
    geo_data$loc_name <- str_to_lower(geo_data$loc_name)
    geo_data$loc_name <- word(geo_data$loc_name, 1) 
    
    religion_data <- read.csv('./data/capstone_religious_adh.csv') 
    colnames(religion_data)[1] <- 'loc_name' 
    
    religion_data <- religion_data %>%
      mutate(city_name = loc_name,
             loc_name = str_to_lower(loc_name),
             loc_name = word(loc_name, 1)
      ) 
    merged_data2 <- merge(religion_data, geo_data, by = 'loc_name') %>%
      pivot_longer(cols = 2:6, names_to = 'religion', values_to = 'value')
    # merged_data2$geometry <- st_transform(merged_data2$geometry)
    
    # Filter by a Religion
    plot_data <- merged_data2 %>%
      mutate(value = ifelse(value == 0, NA, value)) %>%
      filter(religion == var_religion())%>%
      mutate(value = round(value,0))
    
    labs <- lapply(seq(nrow(plot_data)), function(i) {
      paste0( '<p>', plot_data[i, "city_name"], '<p></p>', 'Religion: ', 
              plot_data[i, "religion"], '</p>', 'Percent Adherence: ', '</p>',
              plot_data[i, "value"])
    })
    pal2 <- colorNumeric(palette = "Reds", plot_data$value)
    religion <- plot_data %>%
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(data = plot_data$geometry, color= pal2(plot_data$value),
                  weight = 0.5,
                  fillOpacity = 0.7, smoothFactor = 0,
                  highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                  label = lapply(labs, htmltools::HTML)) %>%
      addLegend(pal = pal2, values = ~plot_data$value, title = 'Percent Adherence', opacity = .75)
    religion
  })
  # End of Religion section ---------------------------------------------------------
  
  # Food Banks ---------------------------------------------------------
  output$foodBanksLeaflet <- renderLeaflet({
    foodBankLoc <- read.csv("./data/foodBank/FoodBanks.csv")
    countyOutlines <- read_sf(dsn = "./data/countyOutlines/countyOutlines.shp")
    labs <- paste0(foodBankLoc$name, "<br></br><a href='", foodBankLoc$url,"'>View</a>")
    pal <- colorFactor(palette = 'Reds', foodBankLoc$county)
    foodBank.map <- foodBankLoc %>%
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 17, drag=FALSE)) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addCircleMarkers(~lng, ~lat, popup=lapply(labs, htmltools::HTML), radius = 5, fillOpacity = 1.0, weight = 1,  color = ~pal(foodBankLoc$county)) %>%
      addPolylines(data = countyOutlines, color = "black", weight = 1.2, smoothFactor = .5,
                   fillOpacity = 0, fillColor = "transparent")
    
    foodBank.map
  })
  
  output$numFoodBanksLocalities <- renderPlotly({
    foodBankLoc <- read.csv("./data/foodBank/FoodBanks.csv")
    gg.table <- as.data.frame(table(foodBankLoc$county))
    colnames(gg.table) <- c("Locality", "Food Banks")
    gg.table$Locality <- as.character(gg.table$Locality)
    gg.table <- rbind(gg.table, c(Locality = "Poquoson", `Food Banks` = 0))
    gg.table <- rbind(gg.table, c(Locality = "Southampton", `Food Banks` = 0))
    gg.table$`Food Banks` <- as.numeric(gg.table$`Food Banks`)
    gg.table$Locality <- factor(gg.table$Locality, levels = gg.table$Locality[order(-gg.table$`Food Banks`)])
    
    # Then, use this reordered factor in your ggplot call without using reorder()
    foodBank.ggplot <- ggplot(gg.table, aes(x = Locality, y = `Food Banks`)) + 
      ggtitle("# of Food Banks in Locality") + 
      geom_col(fill = "firebrick") + 
      theme(legend.position="none", 
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 15)) + 
      geom_text(aes(label = `Food Banks`), 
                vjust = 0.5, # Adjust this value as needed to prevent overlap
                color = 'black') + ylim(0, 12)
    ggplotly(foodBank.ggplot)
  })
  # End of Food Banks section ---------------------------------------------------------
  
  
  # Food Insecurity -----------------------------------------------------
  # function to generate a leaflet map
  generateFoodAccessMap <- function(data, valueColumn, titleSuffix) {
    value <- round(as.numeric(data[[valueColumn]]), 2)
    county <- word(data$County, 1, -2)
    pal <- colorNumeric(palette = "Reds", domain = as.double(value), reverse = TRUE)
    
    foodAccessMap <- leaflet(data, options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal(as.double(value)), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                  highlightOptions = highlightOptions(bringToFront = TRUE, sendToBack = TRUE, opacity = 1.5, weight = 3),
                  label = paste0(county, ": ", value, "%")) %>%
      addPolylines(data = countyOutlines, color = "black", weight = 1.2, smoothFactor = .5,
                   fillOpacity = 0, fillColor = "transparent") %>%
      addLegend(position = "topright", pal = pal, values = as.double(value), opacity = .9, title = paste(titleSuffix, "(%)"))
    foodAccessMap
  }
  # reactive value for input
  var_LowAcc <- reactive({
    input$LowAccIn
  })
  
  # data and outlines
  lowAccDF <- read_sf("./data/foodAtlas/masterData.shp") %>% filter(year == 2019)
  countyOutlines <- read_sf(dsn = "./data/countyOutlines/countyOutlines.shp")
  
  # render the maps for each output
  output$povertyRateMap <- renderLeaflet({
    generateFoodAccessMap(lowAccDF, "PvrtyRt", "Poverty Rate")
  })
  output$lowAccessAF1 <- renderLeaflet({
    generateFoodAccessMap(lowAccDF, "lblck1s", "Low Market Access 1 Mile")
  })
  output$lowAccessAF <- renderLeaflet({
    generateFoodAccessMap(lowAccDF, "lblckhlfs", "Low Market Access 1/2 Mile")
  })
  # End of Food Insecurity to Food section -----------------------------------------------------
  
  # Financial Literacy -----------------------------------------------------
  output$financial_literacy <- renderPlot({
    # load financial literacy data 
    df <- read.csv("./data/public2020.csv")
    df <- df[df$ppstaten == "VA",]
    df<- df[, c("ppstaten", "ppracem", "ppfs1482", "ppfs0596", "ppwork", 
                "pprent", "CFPB_score", "atleast_okay", "DOV_FL","K20", "EF1", "ppcm0062")]
    df[df==""] <- NA
    df <- na.omit(df)
    
    positions <- c("2+ races", "Asian", "Black or African American", "White")
    my_xlab <- paste(positions,"\n(N=",table(df$ppracem),")",sep="")
    
    # plot
    financial_literacy <- ggplot(df, aes(x=ppracem, y=CFPB_score, fill=ppracem)) +
      geom_boxplot(varwidth = TRUE, alpha=0.5) +
      theme(legend.position="none") +
      scale_x_discrete(labels=my_xlab, limits = positions)+
      #scale_x_discrete(limits = positions) + 
      labs(x = "Race") +
      labs(y = "Financial Literacy Score") +
      ylim(0, 100) +
      scale_fill_manual(values = c(
        "Asian" = "red",
        "White" = "black",
        "2+ races" = "firebrick4",
        "Black or African American" = "burlywood4"
      ))+
      theme(text = element_text(size = 15))
    financial_literacy
  })
  
  output$credit_scores <- renderPlotly({
    # load financial literacy data 
    df <- read.csv("./data/public2020.csv")
    df <- df[df$ppstaten == "VA",]
    df<- df[, c("ppstaten", "ppracem", "ppfs1482", "ppfs0596", "ppwork", 
                "pprent", "CFPB_score", "atleast_okay", "DOV_FL","K20", "EF1", "ppcm0062")]
    df[df==""] <- NA
    df <- na.omit(df)
    
    #credit score
    agg <- count(df, ppfs1482, ppracem, atleast_okay, DOV_FL)
    positions <- c("Excellent", "Good", "Fair", "Poor", "Very poor", "Refused")
    credit_scores <- ggplot(agg) +
      geom_col(aes(x = ppfs1482, y = n, fill = ppracem)) +
      scale_x_discrete(limits = positions) + 
      ylim(0, 110) +
      labs(x = "Credit Score Ranking") +
      labs(y = "Count") +
      ggtitle("Credit Score Ranking by Race") +
      labs(fill = "Race")
    credit_scores <- ggplotly(credit_scores) 
    
  })
  
  output$dont_know <- renderPlotly({
    # load financial literacy data 
    df <- read.csv("./data/public2020.csv")
    df <- df[df$ppstaten == "VA",]
    df<- df[, c("ppstaten", "ppracem", "ppfs1482", "ppfs0596", "ppwork", 
                "pprent", "CFPB_score", "atleast_okay", "DOV_FL","K20", "EF1", "ppcm0062")]
    df[df==""] <- NA
    df <- na.omit(df)
    
    agg <- count(df, ppfs1482, ppracem, atleast_okay, DOV_FL)
    
    dont_know <- ggplot(agg) +
      geom_col(aes(x = DOV_FL, y = n, fill = ppracem)) +
      labs(x = "Answered 'Do not know' during survey") +
      labs(y = "Count") +
      scale_fill_manual(values = c("White" = "red",
                                   "Black or African American" = "azure3",
                                   "Asian" = "black",
                                   "2+ races" = "firebrick4"))+
      labs(fill = "Race")
    dont_know <- ggplotly(dont_know)
  })
  # end of Financial Literacy section -----------------------------------------------------
  
  # Media and Entertainment graphs ------------------------------------------
  # Internet Coverage 2015 VS 2020
  # Reactive expression to get the selected coverage year
  var_coverage <- reactive({
    input$select_coverage
  })
  
  # Render Leaflet map based on selected coverage year
  output$internet_coverage_maps <- renderLeaflet({
    # Check if the selected coverage year is 2015
    if (var_coverage() == "2015") {
      # Load zipcode geojson file
      zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
      # Read broadbandnow data
      dat <- read.csv("./data/broadbandnow_data.csv")
      dat <- filter(dat, State == "Virginia")
      dat <- dat[dat$County %in% c(
        "Chesapeake",
        "Franklin",
        "Gloucester",
        "Hampton",
        "Isle of Wight",
        "James City",
        "Mathews",
        "Newport News",
        "Norfolk",
        "Poquoson",
        "Portsmouth",
        "Southampton",
        "Suffolk",
        "Virginia Beach",
        "Williamsburg",
        "York"
      ),]
      dat <- na.omit(dat)
      dat <- select(dat, -State)
      
      # Join the data to the geojson
      joined_zips <- geo_join(zips, dat, by_sp = "name", by_df = "Zip", how = "inner")
      
      # Load redlining data
      norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
      cols <- c("green", "blue", "yellow", "red")
      pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
    
      # Create map details for 2015
    pal <- colorNumeric(palette = "Reds", joined_zips$AllProviderCount_2015)
    labs <- sprintf(
      "<strong>%s</strong><br/>Zip: %i<br/>Num. Providers: %i",
      joined_zips$County,
      joined_zips$Zip,
      joined_zips$AllProviderCount_2015
    ) %>%
      lapply(htmltools::HTML)
    
    # Create Leaflet map for 2015
    internet_coverage_maps <- joined_zips %>% leaflet() %>% 
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(joined_zips$AllProviderCount_2015),
        color = ~ pal(joined_zips$AllProviderCount_2015),
        fillOpacity = 0.7,
        weight = 0.9,
        smoothFactor = 0.2,
        stroke = TRUE,
        highlightOptions = highlightOptions(
          opacity = 0.7,
          weight = 3
        ),
        label = ~ labs) %>%
      addPolygons(
        data = norfolk,
        color = ~ pal2(norfolk$holc_grade),
        opacity = 0.9,
        stroke = F
      ) %>%
      addLegend(
        pal = pal,
        values = ~ joined_zips$AllProviderCount_2015,
        position = "topleft",
        title = "Num. Providers",
        opacity = 0.75
      ) 
    }
    # Check if the selected coverage year is 2020
    else if (var_coverage() == "2020") {
      # Load zipcode geojson file  # load zipcode geojson file
      zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
      
      # Read broadbandnow data
      dat <- read.csv("./data/broadbandnow_data.csv")
      dat <- filter(dat, State == "Virginia")
      dat <- dat[dat$County %in% c(
        "Chesapeake",
        "Franklin",
        "Gloucester",
        "Hampton",
        "Isle of Wight",
        "James City",
        "Mathews",
        "Newport News",
        "Norfolk",
        "Poquoson",
        "Portsmouth",
        "Southampton",
        "Suffolk",
        "Virginia Beach",
        "Williamsburg",
        "York"
      ),]
      dat <- na.omit(dat)
      dat <- select(dat, -State)
      
      # Join population characteristics data
      
      jim_dat <- read.csv("./data/jim_data.csv")
      new_dat <- jim_dat %>% select(zipcode, black.people, percent_of_population)
      joined <- inner_join(dat, new_dat, by = c("Zip" = "zipcode"))
      
      # Join the data to the geojson# join the data to the geojson
      joined_zips <- geo_join(zips, joined, by_sp = "name", by_df = "Zip", how = "inner")
      
      # Load redlining data
      norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
      cols <- c("green", "blue", "yellow", "red")
      pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
      
      # Create map details for 2020
      pal <- colorNumeric(palette = "Reds", joined_zips$AllProviderCount_2020)
      labs <- sprintf(
        "<strong>%s</strong><br/>Zip: %i<br/>Num. Providers: %i<br/>Black Population: %i<br/>Percent Black: %.2f%%",
        joined_zips$County,
        joined_zips$Zip,
        joined_zips$AllProviderCount_2020,
        joined_zips$black.people,
        joined_zips$percent_of_population
      ) %>%
        lapply(htmltools::HTML)
      
      # Create Leaflet map for 2020
      internet_coverage_maps <- joined_zips %>% leaflet() %>% 
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal(joined_zips$AllProviderCount_2020),
          color = ~ pal(joined_zips$AllProviderCount_2020),
          fillOpacity = 0.7,
          weight = 0.9,
          smoothFactor = 0.2,
          stroke = TRUE,
          highlightOptions = highlightOptions(
            opacity = 0.7,
            weight = 3
          ),
          label = ~ labs) %>%
        addPolygons(
          data = norfolk,
          color = ~ pal2(norfolk$holc_grade),
          opacity = 0.8,
          stroke = F
        ) %>%
        addLegend(
          pal = pal,
          values = ~ joined_zips$AllProviderCount_2020,
          position = "topleft",
          title = "Num. Providers",
          opacity = 0.75
        ) 
    }
  })
  
  # Internet Quality 2015 VS 2020
  
  # Define reactive function to retrieve selected quality year
  var_quality <- reactive({
    input$select_quality
  })
  
  output$internet_quality_maps <- renderLeaflet({
    # Get selected quality year
    quality_year <- var_quality()
    
    if (quality_year == "2015") {
      # Load zipcode geojson file
      zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
      
      # Read broadbandnow data
      dat <- read.csv("./data/broadbandnow_data.csv")
      dat <- filter(dat, State == "Virginia")
      dat <- dat[dat$County %in% c(
        "Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City", 
        "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", 
        "Suffolk", "Virginia Beach", "Williamsburg", "York"
      ),]
      dat <- na.omit(dat)
      dat <- select(dat, -State)
      
      # Join the data to the geojson
      joined_zips <- geo_join(zips, dat, by_sp = "name", by_df = "Zip", how = "inner")
      
      # Load redlining data
      norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
      cols <- c("green", "blue", "yellow", "red")
      pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
      
      # Create map details for 2015
      pal <- colorNumeric(palette = "Reds", joined_zips$All100_3.1)
      labs <- sprintf(
        "<strong>%s</strong><br/>Zip: %i<br/>Num. Providers: %i",
        joined_zips$County,
        joined_zips$Zip,
        joined_zips$All100_3.1
      ) %>%
        lapply(htmltools::HTML)
      
      # Create Leaflet map for 2015
      internet_quality_maps <- joined_zips %>% leaflet() %>% 
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal(joined_zips$All100_3.1),
          color = ~ pal(joined_zips$All100_3.1),
          fillOpacity = 0.7,
          weight = 0.9,
          smoothFactor = 0.2,
          stroke = TRUE,
          highlightOptions = highlightOptions(
            opacity = 0.7,
            weight = 3
          ),
          label = ~ labs
        ) %>%
        addPolygons(
          data = norfolk,
          color = ~ pal2(norfolk$holc_grade),
          opacity = 0.8,
          stroke = FALSE
        ) %>%
        addLegend(
          pal = pal,
          values = ~ joined_zips$All100_3.1,
          position = "topleft",
          title = "Num. Providers",
          opacity = 0.75
        ) 
    } else if (quality_year == "2020") {
      # Load zipcode geojson file
      zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
      
      # Read broadbandnow data
      dat <- read.csv("./data/broadbandnow_data.csv")
      dat <- filter(dat, State == "Virginia")
      dat <- dat[dat$County %in% c(
        "Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City", 
        "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", 
        "Suffolk", "Virginia Beach", "Williamsburg", "York"
      ),]
      dat <- na.omit(dat)
      dat <- select(dat, -State)
      
      # Join to population characteristic data
      jim_dat <- read.csv("./data/jim_data.csv")
      new_dat <- jim_dat %>% select(zipcode, black.people, percent_of_population)
      joined <- inner_join(dat, new_dat, by = c("Zip" = "zipcode"))
      
      # Join the data to the geojson
      joined_zips <- geo_join(zips, joined, by_sp = "name", by_df = "Zip", how = "inner")
      
      # Load redlining data
      norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
      cols <- c("green", "blue", "yellow", "red")
      pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
      
      # Create map details for 2020
      pal <- colorNumeric(palette = "Reds", joined_zips$All100_3)
      labs <- sprintf(
        "<strong>%s</strong><br/>Zip: %i<br/>Num. Providers: %i<br/>Black Population: %i<br/>Percent Black: %.2f%%",
        joined_zips$County,
        joined_zips$Zip,
        joined_zips$All100_3,
        joined_zips$black.people,
        joined_zips$percent_of_population
      ) %>%
        lapply(htmltools::HTML)
      
      # Create Leaflet map for 2020
      internet_quality_maps <- joined_zips %>% leaflet() %>% 
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal(joined_zips$All100_3),
          color = ~ pal(joined_zips$All100_3),
          fillOpacity = 0.7,
          weight = 0.9,
          smoothFactor = 0.2,
          stroke = TRUE,
          highlightOptions = highlightOptions(
            opacity = 0.7,
            weight = 3
          ),
          label = ~ labs
        ) %>%
        addPolygons(
          data = norfolk,
          color = ~ pal2(norfolk$holc_grade),
          opacity = 0.8,
          stroke = FALSE
        ) %>%
        addLegend(
          pal = pal,
          values = ~ joined_zips$All100_3,
          position = "topleft",
          title = "Num. Providers"
        ) 
    }
  }) 
  
  # var_news <- reactive({
  #   input$select_news
  # })
  
  # Define the renderPlot function
  output$anch_plots <- renderPlot({
    anch <- read.csv("./data/news_anchors.csv", stringsAsFactors = TRUE)
    
    # Plot 1: Pi Chart
    plt1 <- anch %>%
      group_by(Ethnicity) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = "", y = count, fill = Ethnicity)) +
      geom_bar(stat = "identity", width = 1) + 
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = c("peachpuff4", "grey", "brown", "azure", "red"))+
      geom_text(aes(
        y = count,
        label = paste(round(count / sum(count) * 100, 1), "%"),
        x = 1.3
      ), position = position_stack(vjust = 0.5)) + 
      theme(legend.text=element_text(size=15))
    
    # Plot 2: Bar Chart of Gender and Ethnicity Distributions
    plt2 <- anch %>%
      group_by(Ethnicity, Gender) %>%
      summarise(count = n()) %>%
      ggplot(aes(Ethnicity, count, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") + theme_fivethirtyeight() +
      theme(axis.title = element_text()) + ylab('Count') +
      scale_fill_manual(values = c("F"="red", "M"="black"))+
      theme(axis.title.y = element_text(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text( size = 15),
            legend.text=element_text(size=20)) +
      xlab('Ethnicity')
    
    # Plot 3: Bar Chart of Roles vs. Ethnicity
    plt3 <- anch %>%
      group_by(Ethnicity, Role) %>%
      summarise(count = n()) %>%
      ggplot(aes(Ethnicity, count, fill = Role)) +
      geom_bar(stat = "identity", position = "dodge") + theme_fivethirtyeight() +
      theme(axis.title = element_text()) + ylab('Count') + 
      scale_fill_manual(values = c("Anchor" = "black", 
                                   "Reporter" = "red", 
                                   "Traffic" = "peachpuff4",
                                   "Other" = "firebrick4",
                                   "Sports" = "azure4",
                                   "Weather" = "burlywood1"))+
      theme(axis.title.y = element_text(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text( size = 15),
            legend.text=element_text(size=20)) +
      xlab('Ethnicity')
    
    # Plot 4: Bar Chart of Channel vs. Ethnicity
    plt4 <- anch %>%
      group_by(Ethnicity, Channel) %>%
      summarise(count = n()) %>%
      ggplot(aes(Ethnicity, count, fill = Channel)) +
      geom_bar(stat = "identity", position = "dodge") + theme_fivethirtyeight() +
      theme(axis.title = element_text()) + ylab('Count') +
      scale_fill_manual(values = c("Channel 13"="black", "WAVY" = "peachpuff4", "WTKR" = "red"))+
      theme(axis.title.y = element_text(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text( size = 15),
            legend.text=element_text(size=20)) +
      xlab('Ethnicity')
    
    # Arrange the plots in a 2x2 grid
    anch_plots <- ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2)
    
    # Return the arranged plots
    anch_plots
  }, height = 1200) 
  
  
  # Radio stations
  output$radio <- renderLeaflet({
    # Read geo data
    geo_data <- readRDS('./data/geo_data.rds')
    geo_data <- st_transform(geo_data)
    
    # Preprocess location names
    geo_data$loc_name <- str_to_lower(geo_data$loc_name)
    geo_data$loc_name <- word(geo_data$loc_name, 1) 
    
    # Read radio station data and preprocess
    radio_df <- read.csv('./data/radio_stations.csv')  %>%
      filter(State == 'VA') %>%
      mutate(loc_name = str_to_lower(Community.of.License),
             loc_name = word(loc_name, 1),
             city_name = Community.of.License) %>%
      group_by(loc_name) %>%
      summarise(formats = paste0(Format, collapse = ', '), count = n(),
                city_name = city_name) %>%
      distinct(loc_name, .keep_all = TRUE) %>%
      mutate(formats1 = ifelse(loc_name == 'norfolk', 
                               'religious, urban/variety contemporary, Rhythmic/urban Adult Contemporary' , formats),
             formats2 =ifelse(loc_name == 'norfolk', 
                              'new/public affairs/npr, classical, country, active rock, christian chr', '')) %>%
      mutate(formats1 = ifelse(loc_name == 'virginia', 
                               'modern adult contemporary, big band/ nostalgia/old time radio,' , formats1),
             formats2 =ifelse(loc_name == 'virginia', 
                              'christian contemporaty/preaching, album adult alternative, Christian contemporary hit radio', ''))
    
    # Merge geo data with radio data
    merged_data <- merge(radio_df, geo_data, by = 'loc_name')
    
    # Create labels for markers
    labs <- lapply(seq(nrow(merged_data)), function(i) {
      paste0('<p>', merged_data[i, "city_name"], '</p>',
             '<p>Count of Stations: ', merged_data[i, "count"], '</p>',
             '<p>Type/Formats: </p>',
             '<p>', merged_data[i, "formats1"], '</p>',
             '<p>', merged_data[i, "formats2"], '</p>')
    })
    
    # Define color palette for markers
    pal <- colorNumeric(palette = "Reds", merged_data$count)
    
    # Create Leaflet map
    radio_map <- leaflet(merged_data, options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(fillColor = pal, 
        color = pal,
        weight = 0.5,
        fillOpacity = 0.7,
        smoothFactor = 0,
        highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
        label = lapply(labs, htmltools::HTML)
      ) %>%
      addLegend(pal = pal, values = ~count, title = 'Number of Stations', opacity = 0.75)
    
    radio_map
  })
  
 #Graphs for Headquarter Locations, Ratio of Sentiment and Diversity Vs Pos and Neg
  # Function to load data from CSV file
  load_data <- function() {
    article_dat <- read.csv("data/articledata.csv")
    # Fixing year format for one specific value
    article_dat$year[167] <- 2022
    # Removing empty rows
    article_dat <- article_dat[-(104:110),]
    # Converting columns to factors
    article_dat$hqlocation <- as.factor(article_dat$hqlocation)
    article_dat$source <- as.factor(article_dat$source)
    article_dat
  }
  
  # Function to summarize data by year
  summarize_data <- function(data) {
    pos_sum <- aggregate(pos_len ~ year, data = data, FUN = sum)
    neg_sum <- aggregate(neg_len ~ year, data = data, FUN = sum)
    div_sum <- aggregate(div_len ~ year, data = data, FUN = sum)
    tot_sum <- aggregate(text_len ~ year, data = data, FUN = sum)
    
    sub_dat <- merge(pos_sum, neg_sum, all=TRUE, no.dups = TRUE)
    sub_dat <- merge(sub_dat, div_sum, all=TRUE, no.dups = TRUE)
    sub_dat <- merge(sub_dat, tot_sum, all=TRUE, no.dups = TRUE)
    
    sub_dat$pos_ratio <- sub_dat$pos_len/sub_dat$text_len
    sub_dat$neg_ratio <- sub_dat$neg_len/sub_dat$text_len
    sub_dat$div_ratio <- sub_dat$div_len/sub_dat$text_len
    sub_dat$year <- as.character(sub_dat$year)
    sub_dat
  }
  
  # Function to calculate ratios for each article
  calculate_ratios <- function(data) {
    data$pos_ratio <- data$pos_len/data$text_len
    data$neg_ratio <- data$neg_len/data$text_len
    data$div_ratio <- data$div_len/data$text_len
    data
  }
  
  # Function to create the headquarters plot
  create_headquarters_plot <- function(data) {
    ggplot(data, aes(x = hqlocation)) +
      geom_bar(fill = "firebrick4") + 
      xlab("City") +
      ylab("Number of Articles") +
      ggtitle("Headquarters Locations of Media Companies") +
      theme(axis.text.x = element_text(color = "grey", size = 12, angle = 30),
            axis.text.y = element_text(color = "grey", size = 12), 
            axis.title = element_text(size = 17),
            plot.title = element_text(size = 19))
  }
  
  # Function to create the sentiment by year plot
  create_sentiment_by_year_plot <- function(data, year) {
    sentiment_sub_dat <- subset(data, year == year)
    dat_for_bar_graph <- data.frame(Ratios = c(sentiment_sub_dat$pos_ratio[1], 
                                               sentiment_sub_dat$neg_ratio[1], 
                                               sentiment_sub_dat$div_ratio[1]),
                                    Sentiments = c("Positive", "Negative", "Diversity"))
    ggplot(data = dat_for_bar_graph, aes(x = Sentiments, y = Ratios)) + 
      geom_bar(stat = "identity", fill = "firebrick4") +
      ylab("Proportion to Total Words")
  }
  
  # Function to create the diversity by positive and negative sentiment plot
  create_div_by_pos_and_neg_plot <- function(data) {
    ratios <- melt(data[, c("pos_ratio", "neg_ratio", "div_ratio")], id.vars = "div_ratio")
    ggplot(ratios, aes(x = div_ratio, y = value, color = variable)) +  
      geom_point() + 
      scale_colour_manual(values = c("black", "red")) +
      ylab("Positive and Negative Ratios") + 
      xlab("Diversity Word Ratio") + 
      ggtitle("Positive and Negative Sentiment Ratios versus Diversity Ratio") +
      theme(axis.text.x = element_text(color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10), 
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18))
  }
  
  # Reactive expression for selecting sentiment year
  sentiment_year <- reactive({
    input$select_sent_year
  })
  
  # Output for headquarters plot
  output$headquarters_graph <- renderPlot({
    headquarters_data <- load_data()
    create_headquarters_plot(headquarters_data)
  })
  
  # Output for sentiment by year plot
  output$sentiment_by_year <- renderPlotly({
    sentiment_data <- calculate_ratios(load_data())
    create_sentiment_by_year_plot(sentiment_data, sentiment_year())
  })
  
  # Output for diversity by positive and negative sentiment plot
  output$div_by_pos_and_neg <- renderPlot({
    sentiment_data <- calculate_ratios(load_data())
    create_div_by_pos_and_neg_plot(sentiment_data)
  })
  
  # Link to Word Bags
  url <- a("Link to Word Bags", href="https://tinyurl.com/4ym9njb7")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
  
  
 
  
  ## Policy and Justice -------------------------------------------------------
  # Traffic stops
  
  # Race count
  output$trafficRace <- renderPlot({
    # Read the CSV data
    data <- read.csv("./data/hampton_trafficstop.csv")
    
    # Analyze the data
    print(is.data.frame(data))  # Check if data is a data frame
    print(ncol(data))            # Print the number of columns
    print(nrow(data))            # Print the number of rows
    
    # Create the Race Count plot
    trafficRace <- ggplot(data, aes(x = RACE)) +  
      theme_fivethirtyeight() +                    # Set theme to fivethirtyeight
      geom_bar(fill = "firebrick4") +          # Add bar plot with blue fill
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "grey") +  # Add count labels
      labs(x = "Race", y = "Count", title = "Demographics of Traffic Stops") +  # Add axis and title labels
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),       # Set x-axis text properties
            axis.text.y = element_text(hjust = 1, size = 15),                   # Set y-axis text properties
            plot.title = element_text(color = "black", size = 24)) +             # Set plot title properties
      ylim(0, 10500) +                                                          # Set y-axis limits
      theme(panel.grid.major.x = element_blank())                               # Remove vertical grid lines
    
    # Render the plot
    trafficRace
  })
  
  
  # Race and Jurisdiction
  
  # Render plot for Race vs Jurisdiction
  output$jurisdiction <- renderPlot({
    # Read the CSV data
    data <- read.csv("./data/hampton_trafficstop.csv")
    
    # Create the Race vs Jurisdiction plot
    jurisdiction <- ggplot(data, aes(x = JURISDICTION, fill = RACE)) +
      geom_bar(position = "dodge") +                   # Add dodged bar plot
      theme_fivethirtyeight() +                         # Set theme to fivethirtyeight
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),       # Set x-axis text properties
            axis.text.y = element_text(hjust = 1, size = 15),                   # Set y-axis text properties
            plot.title = element_text(color = "black", size = 24, face = "bold"),  # Set plot title properties
            legend.text=element_text(size=15)) +# Set legend text properties
      scale_fill_manual(values = c("ASIAN/PACIFIC ISLANDER" = "black",
                                   "BLACK OR AFRICAN AMERICAN" = "red",
                                   "AMERICAN INDIAN" = "firebrick4",
                                   "WHITE" = "azure4"))+
      ylim(0, 2000)                                  # Set y-axis limits
    
    jurisdiction
  })
  
  # Reactive variable for selected stop
  var_stop <- reactive(input$select_stop)
  
  # Render plot for Race vs Age for selected jurisdiction
  output$jurisdiction2 <- renderPlot({
    # Read the CSV data
    data <- read.csv("./data/hampton_trafficstop.csv")
    
    # Filter data based on selected jurisdiction
    jurisdiction2 <- data %>% 
      filter(JURISDICTION == var_stop()) %>%         # Filter data based on selected jurisdiction
      ggplot(aes(y = RACE, x = AGE, color = RACE)) + # Create scatter plot
      theme_fivethirtyeight() +                      # Set theme to fivethirtyeight
      geom_boxplot(size = 1, outlier.shape = 1,      # Add boxplot with specified properties
                   outlier.color = "black",
                   outlier.size = 3) +
      geom_jitter(alpha = 0.25, width = .2) +        # Add jittered points
      labs(title = "Traffic Stops Data") +           # Set plot title
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),       # Set x-axis text properties
            axis.text.y = element_text(hjust = 1, size = 15),                   # Set y-axis text properties
            plot.title = element_text(color = "black", size = 24, face = "bold"),  # Set plot title properties
            legend.position = "none") + # Remove legend
      coord_flip()                                   # Flip coordinates to horizontal
    
    jurisdiction2
  })
  
  
  # City council demographics Race
  # Function to read data
  read_data <- function() {
    police_df <- read.csv('./data/hampton_roads_police_chief.csv')
    politicans_df <- read.csv('./data/hampton_roads_politicans.csv')
    return(list(police_df = police_df, politicans_df = politicans_df))
  }
  
  # City council demographics Race
  output$cityd <- renderPlot({
    # Read data
    data <- read_data()
    police_df <- data$police_df
    politicans_df <- data$politicans_df
    
    # Calculate total counts of Black and White individuals in police department
    police_sum <- data.frame('black' = sum(police_df$Black, na.rm = TRUE),
                             'white' = sum(police_df$White, na.rm = TRUE))
    
    # Plot City Council demographics by race
    cityd <- politicans_df %>%
      pivot_longer(4:7, names_to = 'demographic') %>%       # Reshape data for plotting
      mutate(demographic = str_sub(demographic,  14)) %>%   # Extract demographic information
      select(-c(Mayor, Vice.Mayor)) %>%                    # Remove Mayor and Vice Mayor columns
      filter(demographic == 'White' | demographic == 'Black') %>%  # Filter for White and Black demographics
      ggplot(aes(x = City, y = value, fill = demographic)) +   # Set up plot aesthetics
      geom_bar(stat = 'identity', position = 'dodge') +       # Add dodged bar plot
      theme_fivethirtyeight() +                               # Set theme to fivethirtyeight
      theme(axis.title.y = element_text(),                    # Set y-axis title properties
            axis.title = element_text(),                      # Set x-axis title properties
            axis.text.x = element_text(angle = 45, size = 13, hjust = 1),  # Set x-axis text properties
            axis.text.y = element_text(hjust = 1, size = 15),  # Set y-axis text properties
            legend.text = element_text(size = 20),            # Set legend text properties
            legend.title = element_blank()) +                  # Remove legend title
      ggtitle('City Council Demographics by Race 2021') +      # Set plot title
      scale_fill_manual(values = c("Black" = "grey", "White" = "firebrick"))+
      ylab('count')                                           # Set y-axis label
    
    cityd  # Return the plot
  })
  
  
  # City council demographics Gender
  # Function to read data
  read_data <- function() {
    police_df <- read.csv('./data/hampton_roads_police_chief.csv')
    politicans_df <- read.csv('./data/hampton_roads_politicans.csv')
    return(list(police_df = police_df, politicans_df = politicans_df))
  }
  
  # City council demographics Gender
  output$cityd2 <- renderPlot({
    # Read data
    data <- read_data()
    politicans_df <- data$politicans_df
    
    # Plot City Council demographics by gender
    cityd2 <- politicans_df %>%
      pivot_longer(4:7, names_to = 'demographic') %>%       # Reshape data for plotting
      mutate(demographic = str_sub(demographic,  14)) %>%   # Extract demographic information
      select(-c(Mayor, Vice.Mayor)) %>%                    # Remove Mayor and Vice Mayor columns
      filter(demographic == 'Female' | demographic == 'Male') %>%  # Filter for Female and Male demographics
      ggplot(aes(x = City, y = value, fill = demographic)) +   # Set up plot aesthetics
      geom_bar(stat = 'identity', position = 'dodge') +       # Add dodged bar plot
      theme_fivethirtyeight() +                               # Set theme to fivethirtyeight
      theme(axis.title.y = element_text(),                    # Set y-axis title properties
            axis.title = element_text(),                      # Set x-axis title properties
            axis.text.x = element_text(angle = 45, size = 13, hjust = 1),  # Set x-axis text properties
            axis.text.y = element_text(size = 15),            # Set y-axis text properties
            legend.text = element_text(size = 20),            # Set legend text properties
            legend.title = element_blank()) +                  # Remove legend title
      ggtitle('City Council Demographics by Gender 2021') +     # Set plot title
      scale_fill_manual(values = c("Female" = "grey", "Male" = "firebrick"))+
      ylab('count')                                           # Set y-axis label
    
    cityd2  # Return the plot
  })
  
  
  
 
  # Jail plots
  var_jailChoice <- reactive({
    input$select_jailChoice
  })
  
  output$jail <- renderPlot({
    if (var_jailChoice() == "Virginia") {
      # Read data for Virginia
      va_incarceration_trends <- read.csv('./data/va_incarceration_trends.csv')
      data <- va_incarceration_trends
      title <- 'VA state'
    } else if (var_jailChoice() == "Hampton Roads") {
      # Read data for Hampton Roads
      va_hampton_roads_incarceration_trends <- read.csv('./data/va_hampton_roads_incarceration_trends.csv')
      data <- va_hampton_roads_incarceration_trends
      title <- 'Hampton Roads'
    }
    
    # Plotting graph
    jail <- data %>%
      group_by(year) %>%
      summarise(black = sum(black_jail_pop, na.rm = TRUE) / sum(black_pop_15to64, na.rm = TRUE) * 100000,
                white = sum(white_jail_pop, na.rm = TRUE) / sum(white_pop_15to64, na.rm = TRUE) * 100000,
                asian.pi = sum(aapi_jail_pop, na.rm = TRUE) / sum(aapi_pop_15to64, na.rm = TRUE) * 100000,
                latinx = sum(latinx_jail_pop, na.rm = TRUE) / sum(latinx_pop_15to64, na.rm = TRUE) * 100000,
                native.amer = sum(native_jail_pop, na.rm = TRUE) / sum(native_pop_15to64, na.rm = TRUE) * 100000) %>%
      pivot_longer(cols = 2:6, names_to = 'race.ethnicity', values_to = 'jail.rate.per.100k') %>%
      ungroup() %>%
      arrange(desc(year), desc(jail.rate.per.100k)) %>%
      mutate(label = ifelse(year == 2018, race.ethnicity, '')) %>%
      ggplot() +
      geom_line(aes(year, jail.rate.per.100k, col = race.ethnicity), size = 2.5) +
      theme_fivethirtyeight() +
      scale_colour_viridis_d(option = "F") +
      ggtitle(paste('Jail Rate per 100,000 ages 15-64 for', title)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
      theme(legend.text = element_text(size = 20), legend.title = element_blank(),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15)) +
      xlim(1990, 2020) + ylim(0, 2000)
    
    jail
  })
  

  

  
  # Pie plots
  # Reactive expression to get the selected year from the input
  var_pieYear <- reactive({
    input$select_pieYear
  })
  
  # Render pie plot for Jail Population
  output$pie_plots1 <- renderHighchart({
    # Read data from CSV file
    va_hampton_roads_incarceration_trends <- read.csv('./data/va_hampton_roads_incarceration_trends.csv')
    
    # Filter data for the selected year
    col_plot <- va_hampton_roads_incarceration_trends %>%
      filter(year == var_pieYear()) %>% # Filter by selected year
      select(year, black_jail_pop, black_pop_15to64, latinx_jail_pop, latinx_pop_15to64,
             native_jail_pop, native_pop_15to64, white_jail_pop, white_pop_15to64,
             aapi_jail_pop, aapi_pop_15to64, other_race_jail_pop, total_jail_pop, total_pop_15to64) %>%
      summarise(
        black_jail = round(sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        black_pop = round(sum(black_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        native_jail = round(sum(native_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        native_pop = round(sum(native_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        aapi_jail = round(sum(aapi_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        aapi_pop = round(sum(aapi_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        white_jail = round(sum(white_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        white_pop = round(sum(white_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        latinx_jail = round(sum(latinx_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        latinx_pop = round(sum(latinx_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        other_jail = round(sum(other_race_jail_pop, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0)
      ) %>%
      pivot_longer(cols = 1:10, values_to = 'prop', names_to = 'race.ethnicity') %>%
      mutate(
        class = case_when(
          race.ethnicity == 'black_jail' | race.ethnicity == 'black_pop' ~ 'black',
          race.ethnicity == 'white_jail' | race.ethnicity == 'white_pop' ~ 'white',
          race.ethnicity == 'aapi_jail' | race.ethnicity == 'aapi_pop' ~ 'asian.pi',
          race.ethnicity == 'latinx_jail' | race.ethnicity == 'latinx_pop' ~ 'latinx',
          race.ethnicity == 'native_jail' | race.ethnicity == 'native_pop' ~ 'native',
          race.ethnicity == 'other_jail' ~ 'other'
        ),
        type = case_when(
          str_sub(race.ethnicity, -4) == 'jail' ~ 'Jail Population',
          str_sub(race.ethnicity, -3) == 'pop' ~ 'Total Population'
        )
      ) %>%
      arrange(desc(type)) %>%
      ungroup() %>%
      group_by(race.ethnicity) %>%
      mutate(label.pos = prop / 2)
    
    
    # Filter data for Jail Population and create pie chart
    pie_plots1 <- col_plot %>%
      filter(type == 'Jail Population') %>%
      mutate(race.ethnicity = str_sub(race.ethnicity, 1, -6)) %>%
      hchart(
        "pie", hcaes(x = race.ethnicity, y = prop),
        name = paste0("Percentage of Jail Population ", var_pieYear())
      )
    
    pie_plots1
  })
  
  # Render pie plot for Total Population
  output$pie_plots2 <- renderHighchart({
    # Read data from CSV file
    va_hampton_roads_incarceration_trends <- read.csv('./data/va_hampton_roads_incarceration_trends.csv')
    
    # Filter data for the selected year
    col_plot <- va_hampton_roads_incarceration_trends %>%
      filter(year == var_pieYear()) %>% # Filter by selected year
      select(year, black_jail_pop, black_pop_15to64, latinx_jail_pop, latinx_pop_15to64,
             native_jail_pop, native_pop_15to64, white_jail_pop, white_pop_15to64,
             aapi_jail_pop, aapi_pop_15to64, other_race_jail_pop, total_jail_pop, total_pop_15to64) %>%
      summarise(
        black_jail = round(sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        black_pop = round(sum(black_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        native_jail = round(sum(native_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        native_pop = round(sum(native_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        aapi_jail = round(sum(aapi_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        aapi_pop = round(sum(aapi_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        white_jail = round(sum(white_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        white_pop = round(sum(white_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        latinx_jail = round(sum(latinx_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE) * 100, 0),
        latinx_pop = round(sum(latinx_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0),
        other_jail = round(sum(other_race_jail_pop, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE) * 100, 0)
      ) %>%
      pivot_longer(cols = 1:10, values_to = 'prop', names_to = 'race.ethnicity') %>%
      mutate(
        class = case_when(
          race.ethnicity == 'black_jail' | race.ethnicity == 'black_pop' ~ 'black',
          race.ethnicity == 'white_jail' | race.ethnicity == 'white_pop' ~ 'white',
          race.ethnicity == 'aapi_jail' | race.ethnicity == 'aapi_pop' ~ 'asian.pi',
          race.ethnicity == 'latinx_jail' | race.ethnicity == 'latinx_pop' ~ 'latinx',
          race.ethnicity == 'native_jail' | race.ethnicity == 'native_pop' ~ 'native',
          race.ethnicity == 'other_jail' ~ 'other'
        ),
        type = case_when(
          str_sub(race.ethnicity, -4) == 'jail' ~ 'Jail Population',
          str_sub(race.ethnicity, -3) == 'pop' ~ 'Total Population'
        )
      ) %>%
      arrange(desc(type)) %>%
      ungroup() %>%
      group_by(race.ethnicity) %>%
      mutate(label.pos = prop / 2)
    
    # Filter data for Total Population and create pie chart
    pie_plots2 <- col_plot %>%
      filter(type == 'Total Population') %>%
      mutate(race.ethnicity = str_sub(race.ethnicity, 1, -5)) %>%
      hchart(
        "pie", hcaes(x = race.ethnicity, y = prop),
        name = paste0("Percentage of Total Population ", var_pieYear())
      )
    
    pie_plots2
  })
  
  
  # Prison
  # Reactive expression to get the selected year from the input
  var_prisonYear <- reactive({
    input$select_prisonYear
  })
  
  # Render Leaflet map for prison data
  output$prison <- renderLeaflet({
    # Read data from CSV files and RDS file
    va_hampton_roads_incarceration_trends <- read.csv('./data/va_hampton_roads_incarceration_trends.csv')
    geo_data <- readRDS('./data/geo_data.rds')
    geo_data <- st_transform(geo_data)
    
    # Process geo_data to prepare for merging
    geo_data$loc_name <- str_to_lower(geo_data$loc_name)
    geo_data$loc_name <- word(geo_data$loc_name, 1) 
    
    # Process prison data for the selected year
    va_prison_plot_df <- va_hampton_roads_incarceration_trends %>%
      filter(year == var_prisonYear()) %>%
      select(city_name, total_prison_adm_rate) %>%
      drop_na(total_prison_adm_rate) %>%
      arrange(desc(total_prison_adm_rate)) %>%
      mutate(
        loc_name = str_to_lower(city_name),
        loc_name = word(loc_name, 1),
        total_prison_adm_rate = ifelse(total_prison_adm_rate == 0, NA, total_prison_adm_rate)
      )
    
    # Merge prison data with geographic data
    merged_data2 <- merge(va_prison_plot_df, geo_data, by = 'loc_name')
    
    # Create labels for map markers
    labs <- lapply(seq(nrow(merged_data2)), function(i) {
      paste0(
        '<p>', merged_data2[i, "city_name"], '<p></p>', 
        paste(var_prisonYear(), ' Prison Admission Rate Per 100k: '), 
        merged_data2[i, "total_prison_adm_rate"], '</p>'
      ) 
    })
    
    # Define color palette for map markers
    pal2 <- colorNumeric(palette = "Reds", merged_data2$total_prison_adm_rate)
    
    # Create Leaflet map
    prison <- merged_data2 %>%
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(
        data = merged_data2$geometry, 
        color = pal2(merged_data2$total_prison_adm_rate),
        weight = 0.5,
        fillOpacity = 0.7, 
        smoothFactor = 0,
        highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
        label = lapply(labs, htmltools::HTML)
      ) %>%
      addLegend(pal = pal2, values = ~merged_data2$total_prison_adm_rate, title = 'Admission Rate Per 100k', opacity = .75)  
    
    prison
  })
  
  
  # Gentrification (map home values)
  output$map_homevalues <- renderLeaflet({
    # Load zipcode geojson file
    zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
    
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
    
    # Join zipcode data with home value data
    joined_jim <- geo_join(zips, data_jim_final, by_sp = "name", by_df = "zipcode", how = "inner")
    
    # Define color palette for home values
    pal <- colorNumeric(palette = "Reds", joined_jim$homevalue)
    
    # Load redlining data
    norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
    cols <- c("green", "blue", "yellow", "red")
    
    # Define color factor for redlining grades
    pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
    
    # Create Leaflet map
    map_homevalues <- joined_jim %>% 
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(
        data = norfolk,
        color = ~ pal2(norfolk$holc_grade),
        opacity = 0.9,
        stroke = F
      ) %>%
      addPolygons(
        fillColor = ~ pal(joined_jim$homevalue),
        color = ~ pal(joined_jim$homevalue),
        fillOpacity = 0.5,
        weight = 0.9,
        smoothFactor = 0.2,
        stroke = TRUE,
        popup = ~ popup_info
      ) %>%
      addLegend(
        pal = pal,
        values = ~ joined_jim$homevalue,
        position = "topleft",
        title = "Median Home Value",
        opacity = 0.75
      )
    
    map_homevalues
  })
  
  
 
  # Radio stations
  output$radio <- renderLeaflet({
    # Read geo data
    geo_data <- readRDS('./data/geo_data.rds')
    geo_data <- st_transform(geo_data)
    
    # Convert loc_name to lowercase and extract the first word
    geo_data$loc_name <- str_to_lower(geo_data$loc_name)
    geo_data$loc_name <- word(geo_data$loc_name, 1) 
    
    # Read radio station data, filter by VA, and preprocess
    radio_df <- read.csv('./data/radio_stations.csv') %>%
      filter(State == 'VA') %>%
      mutate(loc_name = str_to_lower(Community.of.License),
             loc_name = word(loc_name, 1),
             city_name = Community.of.License) %>%
      group_by(loc_name) %>%
      summarise(formats = paste0(Format, collapse = ', '), count = n(),
                city_name = city_name) %>%
      distinct(loc_name, .keep_all = TRUE) %>%
      mutate(formats1 = ifelse(loc_name == 'norfolk', 
                               'religious, urban/variety contemporary, Rhythmic/urban Adult Contemporary' , formats)) %>%
      mutate(formats2 = ifelse(loc_name == 'norfolk', 
                               'new/public affairs/npr, classical, country, active rock, christian chr', '')) %>%
      mutate(formats1 = ifelse(loc_name == 'virginia', 
                               'modern adult contemporary, big band/ nostalgia/old time radio,' , formats1)) %>%
      mutate(formats2 = ifelse(loc_name == 'virginia', 
                               'christian contemporaty/preaching, album adult alternative, Christian contemporary hit radio', ''))
    
    # Merge geo data with radio station data
    merged_data2 <- merge(radio_df, geo_data, by = 'loc_name')
    
    # Create labels for popups
    labs <- lapply(seq(nrow(merged_data2)), function(i) {
      paste0( '<p>', merged_data2[i, "city_name"], '<p></p>', 'Count of Stations: ', 
              merged_data2[i, "count"], '</p>', 'Type/Formats: ', '</p>',
              merged_data2[i, "formats1"],'</p>', 
              merged_data2[i, "formats2"], '</p>' ) 
    })
    
    # Define color palette for legend
    pal2 <- viridis_pal()(11)
    pal2 <- colorNumeric(palette = "Reds", merged_data2$count)
    
    # Create leaflet map
    radio <- merged_data2 %>%
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(data = merged_data2$geometry, color = viridis_pal()(11)[merged_data2$count],
                  weight = 0.5,
                  fillOpacity = 0.7, smoothFactor = 0,
                  highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                  label = lapply(labs, htmltools::HTML)) %>%
      addLegend(pal = pal2, values = ~merged_data2$count, title = 'Number of Stations', opacity = .75)
    
    # Return the leaflet map
    radio
  })
  
  
  # Household Wellbeing -----------------------------------------------------
  var_well <- reactive({
    input$select_wellbeing
  })
  
  output$wellbeing_maps <- renderLeaflet({
    # Function to create leaflet map for each variable
    create_leaflet_map <- function(data_file, col_names, legend_title) {
      data <- read_rds(data_file)
      colnames(data)[4] <- "Percent"
      colnames(data)[3] <- col_names
      data_pal <- colorNumeric(palette = "Reds", domain = data$Percent, reverse = TRUE)
      
      map <- data %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ data_pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", col_names, ": ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = data_pal,
                  values = ~ Percent,
                  title = legend_title,
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
      
      return(map)
    }
    
    # Get selected variable
    selected_variable <- var_welltext()
    
    # Create leaflet map based on selected variable
    if (selected_variable == "Percent of Black Households Receiving Foodstamps/SNAP Benefits") {
      map <- create_leaflet_map("data/foodstmp.rds", "Food Stamps", "Food Stamps")
    } else if (selected_variable == "Percent of Black County Migration") {
      map <- create_leaflet_map("data/mobile.rds", "Intra-County Migration", "County Migration")
    } else if (selected_variable == "Percent of Black Population that uses car/truck/van to get to work") {
      map <- create_leaflet_map("data/priv_trans.rds", "Private Transport", "Private Transportation")
    } else if (selected_variable == "Percent of Black Population that uses public transportation to get to work") {
      map <- create_leaflet_map("data/pub_trans.rds", "Public Transport", "Public Transportation")
    } else if (selected_variable == "Percent of Black Households with a computer with broadband internet") {
      map <- create_leaflet_map("data/compin.rds", "Computer and Internet", "Computer with Internet Access")
    } else if (selected_variable == "Percent of Black Households without a computer") {
      map <- create_leaflet_map("data/nocomp.rds", "No Computer", "No Computer Access")
    }
    
    return(map)
  })
  
  
  var_welltext <- reactive({
    input$select_wellbeing
  })
  
  output$description_text <- renderText({
    
    if (var_welltext() == "Percent of Black Households Receiving Foodstamps/SNAP Benefits") {
      "The percentage of Black Households receiving Food stamps or SNAP Benefits across the localities of Hampton Roads.
     More than half (54.3%) of the total Black population in Hampton Roads receive one of these welfare programs. There are also considerable
    variabilities across localities - in Franklin, approximately 92% received food stamps/SNAP benefits, in contrast to 12% in Gloucester
    (the lowest number of recipients in Hampton Roads). "
    }
    
    else if (var_welltext() == "Percent of Black County Migration") {
      "Percent of Black population that moved within state but from a different county. There seems to be low mobility across counties and cities in the Hampton Roads region.
    Migration rates ranged from as low as 0.4% to a high of 14.7%. "
    }
    
    else if (var_welltext() == "Percent of Black Population that uses car/truck/van to get to work") {
      "Percent distribution of the Black population that uses a car/truck/van to get to work. We included this indicator as reliable transportation
     can improve economic efficiency (due to access to more jobs, childcare facilities, etc.) and even access to healthcare (e.g., appointments, emergency care).
     Approximately 50% of the Black population uses private transportation to get to work  in Portsmouth. This is relatively high compared to Gloucester,
    where only 8.4% uses similar transportation for work. "
    }
    
    else if (var_welltext() == "Percent of Black Population that uses public transportation to get to work") {
      "Percent distribution of the Black population that uses public transportation to work. We included this indicator as public transportation at times may be unreliable,
    increasing economic inequality. Majority of the Black population in Hampton Roads uses public transportation in order to get work. However, there are some differences
    across localities - a high of 94% in Southampton to a low of 14% in Gloucester."
    }
    
    else if (var_welltext() == "Percent of Black Households with a computer with broadband internet") {
      "Percent of the Black population with a computer that has a broadband internet subscription. The internet and digital devices have become an
   essential component of everyday life. Such access can exacerbate educational and economic inequality,
   thus it is important to understand how the Black community in Hampton Roads engages in these technologies. Despite the rapid usage of technology,
    there are some significant disparities with the Hampton Roads region.
    For instance, over 90% of Black households had a computer with a broadband internet subscription in York compared to a low of 66% in James City."
    }
    
    else if (var_welltext() == "Percent of Black Households without a computer") {
      "Percent of the Black population without a computer. The internet and digital devices have become an
   essential component of everyday life. Such access can exacerbate educational and economic inequality,
   thus it is important to understand how the Black community in Hampton Roads engages in these technologies. While the percent of Black households in Hampton Roads
   without a computer is low, the percentage is surprising given the rapid usage of technology. In 2019, the average White household without a computer was 6%, however, for
   the same period 11 of the 16 localities had a greater percentage of Black households without access - with a high of 76%, 39%, and 16% in Poquoson, Mathews, and
   Franklin, respectively."
    }
  })
  
  # Ranked Graphs -----------------------------------------------------------
  
  dat <- read_csv("data/countyrankings.csv")
  
  var_rank <- reactive({
    input$select_rank
  })
  
  output$ranked_chart <- renderPlotly({
    if (var_rank() == "Black Median Income") {
      ranked_median <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties, med.income),
          y = med.income,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        coord_flip() +
        labs(title = "Black Median Income",
             x = "", y = "Median Income") +
        theme_bw()
      
      ggplotly(ranked_median)
    }
    
    else if (var_rank() == "Black Poverty rate") {
      ranked_pov <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties,
                      desc(pov.rate)),
          y = pov.rate,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Black Poverty rate",
             x = "", y = "%") +
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_pov)
    }
    
    else if (var_rank() == "Black Unemployment rate") {
      ranked_unemp <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties,
                      desc(unemp)),
          y = unemp,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Black Unemployment rate",
             x = "", y = "%") +
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_unemp)
    }
    
    else if (var_rank() == "Percent of Uninsured (Health) Black population") {
      ranked_unins <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties,
                      desc(uninsured)),
          y = uninsured,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percent of Uninsured (Health) Black population",
             x = "", y = "%") +
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_unins)
    }
    
    else if (var_rank() == "Percentage of Black HomeOwners") {
      ranked_hmown <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties,
                      (homeownership)),
          y = homeownership,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black HomeOwners",
             x = "", y = "%") +
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_hmown)
    }
    
    else if (var_rank() == "Percentage of Black Students Suspended") {
      ranked_sus <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties,
                      desc(suspension)),
          y = suspension,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Students Suspended",
             x = "", y = "%") +
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_sus)
    }
    
    else if (var_rank() == "Percentage of Black Students 25 yrs and over that have Bachelor's Degree or Higher") {
      ranked_bach <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties,
                      (bachelor)),
          y = bachelor,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Students 25 yrs and over that have Bachelor's Degree or Higher",
             x = "", y = "%") +
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_bach)
    }
    
    else if (var_rank() == "Percentage of Black Children under 18 in Female Headed Household") {
      ranked_fhoh <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties,
                      desc(femalehead)),
          y = femalehead,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Children under 18 in Female Headed Household",
             x = "", y = "%") +
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_fhoh)
    }
    
    else if (var_rank() == "Percent of Black Households without a computer") {
      ranked_comp <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties,
                      desc(nocomp)),
          y = nocomp,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Households without a computer",
             x = "", y = "%") +
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_comp)
    }
    
    else if (var_rank() == "Percentage of Black Population that uses car/truck/van for work") {
      ranked_car <- dat %>%
        ggplot(data = ., aes(
          x = reorder(Counties,
                      (car)),
          y = car,
          fill = Counties
        )) +
        geom_bar(stat = "identity",
                 width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Population that uses car/truck/van for work",
             x = "", y = "%") +
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_car)
    }
  })
  
  
}

return(server)
shinyApp(ui = ui, server = server)
