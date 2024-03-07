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
  output$hampton_counties_map <- renderPlot({
    coord_data <- read_rds("data/age/coordinates.rds")
    coord_data <- st_transform(coord_data)
    coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
    coordinates2 <- coordinates1[, 6]
    city <-
      c(
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
      )
    coordinates2 <- mutate(coordinates2, Loc = city)
    coordinates2$Loc[coordinates2$Loc == "Franklin"] <-
      "Franklin City"
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
  var_hampRace <- reactive({
    input$hampRaceYearDrop
  })
  
  output$hamp_pie <- renderPlot({
    #pal
    vir_pal <-
      c("#33638DFF",
        "#1F968BFF",
        "#29AF7FFF",
        "#73D055FF",
        "#FDE725FF")
    
    if (var_hampRace() == 2019) {
      hamp_data <- read.csv("data/race/hamp_race2019.csv")
    }
    if (var_hampRace() == 2018) {
      hamp_data <- read.csv("data/race/hamp_race2018.csv")
    }
    if (var_hampRace() == 2017) {
      hamp_data <- read.csv("data/race/hamp_race2017.csv")
    }
    if (var_hampRace() == 2016) {
      hamp_data <- read.csv("data/race/hamp_race2016.csv")
    }
    if (var_hampRace() == 2015) {
      hamp_data <- read.csv("data/race/hamp_race2015.csv")
    }
    if (var_hampRace() == 2014) {
      hamp_data <- read.csv("data/race/hamp_race2014.csv")
    }
    if (var_hampRace() == 2013) {
      hamp_data <- read.csv("data/race/hamp_race2013.csv")
    }
    if (var_hampRace() == 2012) {
      hamp_data <- read.csv("data/race/hamp_race2012.csv")
    }
    if (var_hampRace() == 2011) {
      hamp_data <- read.csv("data/race/hamp_race2011.csv")
    }
    if (var_hampRace() == 2010) {
      hamp_data <- read.csv("data/race/hamp_race2010.csv")
    }
    hamp_data <- hamp_data[, 2:6]
    #combining data from counties
    variable <-
      sample(
        c(
          "B02001_001",
          "B02001_002",
          "B02001_003",
          "B02001_004",
          "B02001_005",
          "B02001_006",
          "B02001_007",
          "B02001_008",
          "B02001_009",
          "B02001_010"
        ),
        160,
        replace = TRUE
      )
    hamp_data <-
      hamp_data %>% group_by(variable) %>% summarize(sum(estimate))
    #select the column and rows we want
    hamp_races <- hamp_data[c(2:8), ]
    #Now graphing with other = hawaiin/pi, america/alask naive. other
    hamp_races3 <- data.frame(t(hamp_data[c(2:8), 2]))
    hamp_races3 <- mutate(hamp_races3, X8 = X3 + X5 + X6)
    hamp_races4 <- data.frame(t(hamp_races3[, c(1, 2, 4, 7, 8)]))
    colnames(hamp_races4) <- "estimate"
    total_pop = sum(hamp_races4$estimate)
    hamp_races4 <- mutate(hamp_races4, total = total_pop)
    hamp_races4 <- mutate(hamp_races4, pct = estimate / total * 100)
    hamp_races4 <-
      mutate(hamp_races4,
             race = c("White", "Black", "Asian", "Two or more", "Other"))
    colnames(hamp_races4) <-
      c("estimate", "Total", "Percent of Population", "race")
    
    hamp_races5 <- hamp_races4 %>%
      mutate(
        cs = rev(cumsum(rev(
          `Percent of Population`
        ))),
        pos = `Percent of Population` / 2 + lead(cs, 1),
        pos = if_else(is.na(pos), `Percent of Population` / 2, pos)
      )
    
    hamp_pie <-
      ggplot(hamp_races5,
             aes(
               x = "" ,
               y = `Percent of Population`,
               fill = fct_inorder(race)
             )) +
      geom_col(width = 1) +
      coord_polar(theta = "y", start = 0) +
      geom_label_repel(
        aes(y = pos, label = paste0(
          round(`Percent of Population`, digits = 2), "%"
        )),
        data = hamp_races5,
        size = 4,
        show.legend = F,
        nudge_x = 1
      ) +
      guides(fill = guide_legend(title = "Key")) +
      scale_fill_manual(values = vir_pal) +
      theme_void() +
      theme(legend.title = element_blank())
    #plot
    hamp_pie
  })
  
  
  # VA race plots -----------------------------------------------------
  var_VaRace <- reactive({
    input$VaRaceYearDrop
  })
  
  output$va_pie <- renderPlot({
    #pal
    vir_pal <-
      c("#33638DFF",
        "#1F968BFF",
        "#29AF7FFF",
        "#73D055FF",
        "#FDE725FF")
    if (var_VaRace() == 2019) {
      races <- read.csv("data/race/va_race2019.csv")
    }
    if (var_VaRace() == 2018) {
      races <- read.csv("data/race/va_race2018.csv")
    }
    if (var_VaRace() == 2017) {
      races <- read.csv("data/race/va_race2017.csv")
    }
    if (var_VaRace() == 2016) {
      races <- read.csv("data/race/va_race2016.csv")
    }
    if (var_VaRace() == 2015) {
      races <- read.csv("data/race/va_race2015.csv")
    }
    if (var_VaRace() == 2014) {
      races <- read.csv("data/race/va_race2014.csv")
    }
    if (var_VaRace() == 2013) {
      races <- read.csv("data/race/va_race2013.csv")
    }
    if (var_VaRace() == 2012) {
      races <- read.csv("data/race/va_race2012.csv")
    }
    if (var_VaRace() == 2011) {
      races <- read.csv("data/race/va_race2011.csv")
    }
    if (var_VaRace() == 2010) {
      races <- read.csv("data/race/va_race2010.csv")
    }
    races <- races[, 2:6]
    total <- races[1, 4]
    #Now graphing with other = hawaiin/pi, america/alask naive. other
    va_races <-  data.frame(t((races[c(2:8), 4])))
    va_races <- mutate(va_races, X8 = X3 + X5 + X6)
    va_races2 <- data.frame(t(va_races[, c(1, 2, 4, 7, 8)]))
    colnames(va_races2) <- "estimate"
    va_races2 <- mutate(va_races2, totl = total)
    va_races2 <- mutate(va_races2, pct = estimate / totl * 100)
    va_races2 <-
      mutate(va_races2,
             race = c("White", "Black", "Asian", "Two or more", "Other"))
    va_races3 <- va_races2 %>%
      mutate(
        cs = rev(cumsum(rev(pct))),
        pos = pct / 2 + lead(cs, 1),
        pos = if_else(is.na(pos), pct / 2, pos)
      )
    
    va_pie <-
      ggplot(va_races3, aes(
        x = "" ,
        y = pct,
        fill = fct_inorder(race)
      )) +
      geom_col(width = 1) +
      coord_polar(theta = "y", start = 0) +
      geom_label_repel(
        aes(y = pos, label = paste0(round(pct, digits = 2), "%")),
        data = va_races3,
        size = 4,
        show.legend = F,
        nudge_x = 1
      ) +
      guides(fill = guide_legend(title = "Key")) +
      scale_fill_manual(values = vir_pal) +
      theme_void()  +
      theme(legend.title = element_blank())
    #plot
    va_pie
    
  })
  
  # Hampton age plot-------------------------------------------------
  var_hampAge <- reactive({
    input$HampAgeYearDrop
  })
  
  output$hamp_graph <- renderPlot({
    #pal
    vir_pal <-
      c("#33638DFF",
        "#1F968BFF",
        "#29AF7FFF",
        "#73D055FF",
        "#FDE725FF")
    if (var_hampAge() == 2019) {
      hamp_ages  <- read.csv("data/age/hamp_age2019.csv")
    }
    if (var_hampAge() == 2018) {
      hamp_ages  <- read.csv("data/age/hamp_age2018.csv")
    }
    if (var_hampAge() == 2017) {
      hamp_ages  <- read.csv("data/age/hamp_age2017.csv")
    }
    if (var_hampAge() == 2016) {
      hamp_ages  <- read.csv("data/age/hamp_age2016.csv")
    }
    if (var_hampAge() == 2015) {
      hamp_ages  <- read.csv("data/age/hamp_age2015.csv")
    }
    if (var_hampAge() == 2014) {
      hamp_ages  <- read.csv("data/age/hamp_age2014.csv")
    }
    if (var_hampAge() == 2013) {
      hamp_ages  <- read.csv("data/age/hamp_age2013.csv")
    }
    if (var_hampAge() == 2012) {
      hamp_ages  <- read.csv("data/age/hamp_age2012.csv")
    }
    if (var_hampAge() == 2011) {
      hamp_ages  <- read.csv("data/age/hamp_age2011.csv")
    }
    if (var_hampAge() == 2010) {
      hamp_ages  <- read.csv("data/age/hamp_age2010.csv")
    }
    hamp_ages <- hamp_ages[, 2:6]
    #total population in hampton Roads (1713267)
    hamp_pop_tbl <- hamp_ages %>%
      group_by(NAME) %>%
      slice(1)
    hamp_pop <- colSums(hamp_pop_tbl[, 4])
    #Getting male estimates for each age group (summing every county for that specific male age group)
    hamp_male <- hamp_ages %>%
      group_by(NAME) %>%
      slice(3:25)
    hamp_male2 <-
      hamp_male %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))
    #Getting female estimates for each age group (summing every county for that specific female age group)
    hamp_female <- hamp_ages %>%
      group_by(NAME) %>%
      slice(27:49)
    hamp_female2 <-
      hamp_female %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))
    hamp_gender <- cbind(hamp_male2, hamp_female2)
    hamp_gender <- hamp_gender[, c(2, 4)]
    #hamp_gender2 <- data.frame(estimate = c(hamp_gender[,1], hamp_gender[,2]))
    colnames(hamp_gender) <- c("male", "female")
    hamp_gender <- mutate(hamp_gender, total = male + female)
    #transposing just the estimates
    hamp_ages2 <- data.frame(t(hamp_gender[, 3]))
    #sorting into the age groups
    hamp_ages2 <- mutate(hamp_ages2, Under18 = X1 + X2 + X3 + X4)
    hamp_ages2 <-
      mutate(hamp_ages2, YoungAdult = X5 + X6 + X7 + X8 + X9)
    hamp_ages2 <- mutate(hamp_ages2, Adult = X10 + X11 + X12)
    hamp_ages2 <-
      mutate(hamp_ages2, MiddleAge = X13 + X14 + X15 + X16 + X17)
    hamp_ages2 <-
      mutate(hamp_ages2, Senior = X18 + X19 + X20 + X21 + X22 + X23)
    #using just the 5 age group data that was just sorted
    hamp_ages3 <- hamp_ages2[, 24:28]
    row.names(hamp_ages3) <- "General Estimate"
    hamp_ages3 <- data.frame(t(hamp_ages3))
    #Getting the percentage
    hamp_ages3 <- mutate(hamp_ages3, TotalPopulation = hamp_pop)
    hamp_ages3 <-
      mutate(hamp_ages3, PctPop = General.Estimate / TotalPopulation * 100)
    hamp_ages3 <-
      mutate(hamp_ages3,
             Labels = c("Under 18", "18 to 29", "30 to 44",
                        "45 to 64", "65 and Older"))
    #ordering the age grpups
    hamp_ages3$Labels <-
      factor(
        hamp_ages3$Labels,
        levels = c("Under 18", "18 to 29", "30 to 44",
                   "45 to 64", "65 and Older")
      )
    #Graph
    
    
    hamp_graph <-
      ggplot(hamp_ages3 , aes(x = "", y = PctPop, fill = Labels)) +
      geom_bar(stat = "identity",
               width = 1,
               color = "white") +
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
        color = "white"
      )  +
      scale_fill_manual(values = vir_pal)
    #plot
    hamp_graph
    
  })
  
  # Va age plot----------------------------------------------------
  var_VaAge <- reactive({
    input$VaAgeYearDrop
  })
  
  output$va_graph <- renderPlot({
    #pal
    vir_pal <-
      c("#33638DFF",
        "#1F968BFF",
        "#29AF7FFF",
        "#73D055FF",
        "#FDE725FF")
    if (var_VaAge() == 2019) {
      age1  <- read.csv("data/age/va_age2019.csv")
    }
    if (var_VaAge() == 2018) {
      age1  <- read.csv("data/age/va_age2018.csv")
    }
    if (var_VaAge() == 2017) {
      age1  <- read.csv("data/age/va_age2017.csv")
    }
    if (var_VaAge() == 2016) {
      age1  <- read.csv("data/age/va_age2016.csv")
    }
    if (var_VaAge() == 2015) {
      age1  <- read.csv("data/age/va_age2015.csv")
    }
    if (var_VaAge() == 2014) {
      age1  <- read.csv("data/age/va_age2014.csv")
    }
    if (var_VaAge() == 2013) {
      age1  <- read.csv("data/age/va_age2013.csv")
    }
    if (var_VaAge() == 2012) {
      age1  <- read.csv("data/age/va_age2012.csv")
    }
    if (var_VaAge() == 2011) {
      age1  <- read.csv("data/age/va_age2011.csv")
    }
    if (var_VaAge() == 2010) {
      age1  <- read.csv("data/age/va_age2010.csv")
    }
    age1 <- age1[, 2:6]
    va_total_pop <- age1[1, 4]
    #Adds the female and male data together to get the population for each age group
    va_male_age <- age1[3:25, ]
    va_female_age <- age1[27:49, ]
    va_male_age <- tibble::rowid_to_column(va_male_age, "ID")
    va_female_age <- tibble::rowid_to_column(va_female_age, "ID")
    #adding the male and female estimates to get the total
    ages <- merge(va_female_age, va_male_age, by = "ID")
    ages <- mutate(ages, total = estimate.x + estimate.y)
    #Getting just the estimates for each age group and transposing it to combining rows easily
    va_ages1 <- data.frame(t(ages[, 12]))
    #Groups: Under 18, 18-30, 30-45, 45-65, 65+
    va_ages1 <- mutate(va_ages1, Under18 = X1 + X2 + X3 + X4)
    va_ages1 <-
      mutate(va_ages1, YoungAdult = X5 + X6 + X7 + X8 + X9)
    va_ages1 <- mutate(va_ages1, Adult = X10 + X11 + X12)
    va_ages1 <-
      mutate(va_ages1, MiddleAge = X13 + X14 + X15 + X16 + X17)
    va_ages1 <-
      mutate(va_ages1, Senior = X18 + X19 + X20 + X21 + X22 + X23)
    #using the 5 age group data
    va_ages2 <- va_ages1[, 24:28]
    row.names(va_ages2) <- "Estimate"
    va_ages2 <- data.frame(t(va_ages2))
    va_ages2 <- mutate(va_ages2, TotalPopulation = va_total_pop)
    #Make Percentage
    va_ages2 <-
      mutate(va_ages2, PctPop = Estimate / TotalPopulation * 100)
    #labeling
    va_ages2 <-
      mutate(va_ages2,
             labels = c("Under 18", "18 to 29", "30 to 44",
                        "45 to 64", "65 and Older"))
    colnames(va_ages2) <-
      c("Estimate",
        "Total Population",
        "Percent of Population",
        "Labels")
    va_ages2[, 4] <-
      factor(va_ages2[, 4],
             levels = c("Under 18", "18 to 29", "30 to 44",
                        "45 to 64", "65 and Older"))
    #Graph
    va_graph <-
      ggplot(va_ages2 , aes(x = "", y = `Percent of Population`, fill = Labels)) +
      geom_bar(stat = "identity",
               width = 1,
               color = "white") +
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
        aes(label = paste0(round(
          `Percent of Population`
        ), "%")),
        position = position_stack(vjust = 0.5),
        size = 5,
        color = "white"
      ) +
      scale_fill_manual(values = vir_pal)
    #plot
    va_graph
    
  })
  
  
  # Hampton Counties Map ------------------------------------------
  var_hampCountiesAge <- reactive({
    input$HampCountAgeYearDrop
  })
  
  output$age_map <- renderPlot({
    if (var_hampCountiesAge() == 2019) {
      hamp_ages  <- read.csv("data/age/hamp_age2019.csv")
    }
    if (var_hampCountiesAge() == 2018) {
      hamp_ages  <- read.csv("data/age/hamp_age2018.csv")
    }
    if (var_hampCountiesAge() == 2017) {
      hamp_ages  <- read.csv("data/age/hamp_age2017.csv")
    }
    if (var_hampCountiesAge() == 2016) {
      hamp_ages  <- read.csv("data/age/hamp_age2016.csv")
    }
    if (var_hampCountiesAge() == 2015) {
      hamp_ages  <- read.csv("data/age/hamp_age2015.csv")
    }
    if (var_hampCountiesAge() == 2014) {
      hamp_ages  <- read.csv("data/age/hamp_age2014.csv")
    }
    if (var_hampCountiesAge() == 2013) {
      hamp_ages  <- read.csv("data/age/hamp_age2013.csv")
    }
    if (var_hampCountiesAge() == 2012) {
      hamp_ages  <- read.csv("data/age/hamp_age2012.csv")
    }
    if (var_hampCountiesAge() == 2011) {
      hamp_ages  <- read.csv("data/age/hamp_age2011.csv")
    }
    if (var_hampCountiesAge() == 2010) {
      hamp_ages  <- read.csv("data/age/hamp_age2010.csv")
    }
    hamp_ages <- hamp_ages[, 2:6]
    county_pop <- hamp_ages %>% group_by(NAME) %>%
      slice(1)
    county_pop <- county_pop[, 4]
    #Getting male estimates for each age group
    county_male <- hamp_ages %>%
      group_by(NAME) %>%
      slice(3:25)
    #Getting female estimates for each age group (summing every county for that specific female age group)
    county_female <- hamp_ages %>%
      group_by(NAME) %>%
      slice(27:49)
    #assigning ID to merge female and male estimates to get overall estimates
    county_male <- tibble::rowid_to_column(county_male, "ID")
    county_female <- tibble::rowid_to_column(county_female, "ID")
    county_ages <- merge(county_female, county_male, by = "ID")
    county_ages <-
      mutate(county_ages, total = estimate.x + estimate.y)
    #get the estimates put in the age groups(map data)
    #under 18
    county_under <- county_ages %>%
      group_by(NAME.y) %>%
      slice(1:4)
    county_under2 <- county_under %>%
      group_by(NAME.y) %>%
      summarise(x = sum(total))
    county_under2 <- county_under2[, 2]
    #young adult
    county_ya <- county_ages %>%
      group_by(NAME.y) %>%
      slice(5:9)
    county_ya2 <- county_ya %>%
      group_by(NAME.y) %>%
      summarise(x = sum(total))
    county_ya2 <- county_ya2[, 2]
    #adult
    county_adult <- county_ages %>%
      group_by(NAME.y) %>%
      slice(10:12)
    county_adult2 <- county_adult %>%
      group_by(NAME.y) %>%
      summarise(x = sum(total))
    county_adult2 <- county_adult2[, 2]
    #middle age
    county_ma <- county_ages %>%
      group_by(NAME.y) %>%
      slice(13:17)
    county_ma2 <- county_ma %>%
      group_by(NAME.y) %>%
      summarise(x = sum(total))
    county_ma2 <- county_ma2[, 2]
    #senior
    county_senior <- county_ages %>%
      group_by(NAME.y) %>%
      slice(18:23)
    county_senior2 <- county_senior %>%
      group_by(NAME.y) %>%
      summarise(x = sum(total))
    county_senior2 <- county_senior2[, 2]
    counties_label <-
      c(
        "Chesapeake",
        "Franklin",
        "Hampton",
        "Newport News",
        "Norfolk",
        "Poquoson",
        "Portsmouth",
        "Suffolk",
        "Virginia Beach",
        "Williamsburg",
        "Gloucester",
        "Isle of Wight",
        "James City",
        "Mathews",
        "Southampton",
        "York"
      )
    #getting coordinates
    lat <-
      c(
        36.690473,
        36.683540,
        37.046933,
        37.123232,
        36.903378,
        37.130348,
        36.878493,
        36.714941,
        36.792042,
        37.267284,
        37.405450,
        36.901637,
        37.311197,
        37.470724,
        36.720152,
        37.242246
      )
    lon <-
      c(
        -76.297654,
        -76.940148,
        -76.390236,
        -76.523771,
        -76.248186,
        -76.357799,
        -76.380289,
        -76.626346,-76.053855,
        -76.708205,
        -76.519133,
        -76.708161,
        -76.804677,
        -76.375820,
        -77.114512,
        -76.566393
      )
    #format
    general_county_alt <-
      cbind(
        county_under2,
        county_ya2,
        county_adult2,
        county_ma2,
        county_senior2,
        county_pop
      )
    colnames(general_county_alt) <-
      c("a", "b", "c", "d", "e", "total")
    general_county_alt <-
      mutate(general_county_alt, under = a / total * 100)
    general_county_alt <-
      mutate(general_county_alt, ya = b / total * 100)
    general_county_alt <-
      mutate(general_county_alt, adult = c / total * 100)
    general_county_alt <-
      mutate(general_county_alt, ma = d / total * 100)
    general_county_alt <-
      mutate(general_county_alt, senior = e / total * 100)
    general_county_alt2 <- general_county_alt[, 7:11]
    general_county_alt2 <-
      mutate(general_county_alt2, county = counties_label)
    general_county_alt2 <- cbind(general_county_alt2, lon, lat)
    colnames(general_county_alt2) <-
      c("A", "B", "C", "D", "E", "county", "lon", "lat")
    #Getting map data for counties in Hampton roads
    coord_data <- read_rds("data/age/coordinates.rds")
    coord_data <- st_transform(coord_data)
    coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
    coordinates2 <- coordinates1[, 6]
    city <-
      c(
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
      )
    coordinates2 <- mutate(coordinates2, Loc = city)
    coordinates2$Loc[coordinates2$Loc == "Franklin"] <-
      "Franklin City"
    #Graph
    age_map <- ggplot(coordinates2) +
      geom_sf() +
      geom_scatterpie(aes(
        x = lon,
        y = lat,
        group = county,
        r = 0.05
      ),
      data = general_county_alt2,
      cols = LETTERS[1:5]) +
      geom_sf_label(
        aes(label = Loc, geometry = geometry),
        label.padding = unit(.5, "mm"),
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
      #geom_scatterpie(aes(x=lon, y=lat, group=county, r =0.05), data=general_county_alt2,
      #             cols=LETTERS[1:5]) +
      scale_fill_viridis_d(labels = c("Under 18", "18 to 29", "30 to 44",
                                      "45 to 64", "65 and Older"))
    #plot
    age_map
    
  })
  
  # Total Population Educational Attainment ---------------------------------
  
  var_genEducationalAttainment <- reactive({
    input$genEdAttainmentYearDrop
  })
  
  output$genEdAttainmentPlots <- renderPlotly({
    if (var_genEducationalAttainment() == "2019") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2019.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2019.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if (var_genEducationalAttainment() == "2018") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2018.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2018.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if (var_genEducationalAttainment() == "2017") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2017.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2017.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if (var_genEducationalAttainment() == "2016") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2016.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2016.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if (var_genEducationalAttainment() == "2015") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2015.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2015.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if (var_genEducationalAttainment() == "2014") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2014.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2014.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if (var_genEducationalAttainment() == "2013") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2013.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2013.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if (var_genEducationalAttainment() == "2012") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2012.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2012.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if (var_genEducationalAttainment() == "2011") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2011.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2011.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    
    else if (var_genEducationalAttainment() == "2010") {
      generalEducationalAttainment <-
        read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2010.csv")
      generalBlackEducationalAttainment <-
        read.csv(
          "data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2010.csv"
        )
      colnames(generalEducationalAttainment) <-
        c("Name",
          "Variable",
          "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <-
        c(
          "Year",
          "Name",
          "Variable",
          "Male",
          "variable2",
          "Female",
          "variable3",
          "Total",
          "Bachelor or Higher as Highest Attainment %"
        )
      generalEducationalAttainment$Variable  <-
        rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <-
        rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <-
        cbind(
          generalBlackEducationalAttainment$Name,
          generalBlackEducationalAttainment$Variable,
          generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      modifiedGeneralEducationalAttainment <-
        cbind(
          generalEducationalAttainment$Name,
          generalEducationalAttainment$Variable,
          generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`
        )
      generalTotal <-
        rbind(
          modifiedGeneralEducationalAttainment,
          modifiedGeneralBlackEducationalAttainment
        )
      colnames(generalTotal)  <-
        c("Name",
          "Demographic",
          "Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal)
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-
        as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
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
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
  })
  
  # Teacher Demographics------------------------------------------------------
  
  var_teacherRaces <- reactive({
    input$teacherRaceBreakdown
  })
  
  
  output$teacherRacePlots <- renderPlotly({
    if (var_teacherRaces() == "Black") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <-
        c(
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
      teacherByRace <- teacherByRace  %>%
        ggplot(aes(x = Name, y = `% of Black Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))  +
        scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip = c("x", "y")))
    }
    
    else if (var_teacherRaces() == "Asian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <-
        c(
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
      teacherByRace <- teacherByRace  %>%
        ggplot(aes(x = Name, y = `% of Asian Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40)) + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip = c("x", "y")))
    }
    
    else if (var_teacherRaces() == "White") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <-
        c(
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
      teacherByRace <- teacherByRace  %>%
        ggplot(aes(x = Name, y = `% of White Teachers`, fill = Name)) + geom_col() +
        labs(title = "White Teacher Breakdown", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))   + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip = c("x", "y")))
    }
    
    else if (var_teacherRaces() == "Hispanic") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <-
        c(
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
      teacherByRace <- teacherByRace  %>%
        ggplot(aes(x = Name, y = `% of Hispanic Teachers`, fill = Name)) + geom_col() +
        labs(title = "Hispanic Teacher Breakdown", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))   + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip = c("x", "y")))
    }
    
    else if (var_teacherRaces() == "American Indian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <-
        c(
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
      teacherByRace <- teacherByRace  %>%
        ggplot(aes(x = Name, y = `% of American Indian Teachers`, fill = Name)) + geom_col() +
        labs(title = "American Indian Teacher Breakdown", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))   + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip = c("x", "y")))
    }
    
    else if (var_teacherRaces() == "Two or More Races") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <-
        c(
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
      teacherByRace <- teacherByRace  %>%
        ggplot(aes(x = Name, y = `% of Two Or More Races Teachers`, fill = Name)) + geom_col() +
        labs(title = "Two or More Races Teacher Breakdown", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40)) + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip = c("x", "y")))
      
    }
    
    else if (var_teacherRaces() == "Hawaiian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <-
        c(
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
      teacherByRace <- teacherByRace  %>%
        ggplot(aes(x = Name, y = `% of Hawaiian Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))   + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip = c("x", "y")))
    }
    
  })
  
  
  
  # suspension line graph-------------------------------------------------------
  
  output$suspension_line_graph <- renderPlotly({
    year <- "2018-2019"
    suspension_data <-
      read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <-
      suspension_data %>% filter(Location == "Virginia") %>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <-
      suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <-
      suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <-
      suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race19 <-
      rbind(va_blck[, 6], va_hisp[, 6], va_white[, 6])
    va_suspension_race19$Data <-
      as.numeric(va_suspension_race19$Data)
    va_suspension_race19 <-
      mutate(va_suspension_race19, Data = Data * 100)
    va_suspension_race19 <-
      mutate(va_suspension_race19, race = c("Black", "Hispanic", "White"))
    va_suspension_race19 <-
      mutate(va_suspension_race19, year = "2019")
    ##
    year <- "AY 2017-2018"
    suspension_data <-
      read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <-
      suspension_data %>% filter(Location == "Virginia") %>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <-
      suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <-
      suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <-
      suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race18 <-
      rbind(va_blck[, 6], va_hisp[, 6], va_white[, 6])
    va_suspension_race18$Data <-
      as.numeric(va_suspension_race18$Data)
    va_suspension_race18 <-
      mutate(va_suspension_race18, Data = Data * 100)
    va_suspension_race18 <-
      mutate(va_suspension_race18, race = c("Black", "Hispanic", "White"))
    va_suspension_race18 <-
      mutate(va_suspension_race18, year = "2018")
    ##
    year <- "AY 2016-2017"
    suspension_data <-
      read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <-
      suspension_data %>% filter(Location == "Virginia") %>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <-
      suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <-
      suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <-
      suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race17 <-
      rbind(va_blck[, 6], va_hisp[, 6], va_white[, 6])
    va_suspension_race17$Data <-
      as.numeric(va_suspension_race17$Data)
    va_suspension_race17 <-
      mutate(va_suspension_race17, Data = Data * 100)
    va_suspension_race17 <-
      mutate(va_suspension_race17, race = c("Black", "Hispanic", "White"))
    va_suspension_race17 <-
      mutate(va_suspension_race17, year = "2017")
    ##
    year <- "AY 2015-2016"
    suspension_data <-
      read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <-
      suspension_data %>% filter(Location == "Virginia") %>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <-
      suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <-
      suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <-
      suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(black, hispanic, white)
    va_suspension_race16 <-
      rbind(va_blck[, 6], va_hisp[, 6], va_white[, 6])
    va_suspension_race16$Data <-
      as.numeric(va_suspension_race16$Data)
    va_suspension_race16 <-
      mutate(va_suspension_race16, Data = Data * 100)
    va_suspension_race16 <-
      mutate(va_suspension_race16, race = c("Black", "Hispanic", "White"))
    va_suspension_race16 <-
      mutate(va_suspension_race16, year = "2016")
    ##
    year <- "AY 2014-2015"
    suspension_data <-
      read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <-
      suspension_data %>% filter(Location == "Virginia") %>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <-
      suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <-
      suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <-
      suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race15 <-
      rbind(va_blck[, 6], va_hisp[, 6], va_white[, 6])
    va_suspension_race15$Data <-
      as.numeric(va_suspension_race15$Data)
    va_suspension_race15 <-
      mutate(va_suspension_race15, Data = Data * 100)
    va_suspension_race15 <-
      mutate(va_suspension_race15, race = c("Black", "Hispanic", "White"))
    va_suspension_race15 <-
      mutate(va_suspension_race15, year = "2015")
    #combining data
    suspension_line <-
      rbind(
        va_suspension_race19,
        va_suspension_race18,
        va_suspension_race17,
        va_suspension_race16,
        va_suspension_race15
      )
    colnames(suspension_line) <-
      c("Percent Suspended", "Race", "Year")
    suspension_line_graph <-
      ggplot(suspension_line,
             aes(
               x = Year,
               y = `Percent Suspended`,
               group = Race,
               color = Race
             )) +
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
      scale_color_viridis_d() +
      scale_y_continuous(limits = c(2, 14), breaks = seq(0, 14, by = 2))
    #plot
    ggplotly(suspension_line_graph, tooltip = c("x", "y", "group")) %>%
      layout(legend = list(y = 0.5))
    
  })
  
  #suspension gap line graph ------------------------------------------------
  output$suspensionGap <- renderPlotly({
    gap_data <- read.csv("data/suspension/suspensionGap.csv")
    gap_data$year[gap_data$year == "2018-2019"] <- "2019"
    gap_data$year[gap_data$year == "2017-2018"] <- "2018"
    gap_data$year[gap_data$year == "2016-2017"] <- "2017"
    gap_data$year[gap_data$year == "2015-2016"] <- "2016"
    gap_data$year[gap_data$year == "2014-2015"] <- "2015"
    colnames(gap_data) <-
      c("x", "Location", "Percent Difference", "Year")
    susGapPlot <-
      ggplot(gap_data,
             aes(
               x = Year,
               y = `Percent Difference`,
               group = Location,
               color = Location
             )) +
      geom_line(position = "identity", size = 1.5) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.text = element_text(siz = 13)
      ) +
      labs(y = "Percent Difference (%)") +
      scale_color_viridis_d() +
      ylim(0, 16)
    
    suspensionGap <-
      ggplotly(susGapPlot, tooltip = c("x", "y", "group")) %>%
      layout(legend = list(y = 0.5))
    
  })
  
  
  # Suspension for black and white (counties) ---------------------------------
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
        scale_fill_manual(values = c("#D55E00", "#0072B2")) +
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
        scale_fill_manual(values = c("#D55E00", "#0072B2")) +
        labs(x = "Location")
      #plot
      BW_map <- suspension_counties_plot
      ggplotly(BW_map)
    }
  })
  
  
  # On Time Graduation Rates -----------------------------------------------------------
  #
  rates <- read_csv("data/on_time_graduation.csv")
  rates <- rates %>%
    na.omit(rates)
  
  basemap <- leaflet(width = "100%", height = "400px") %>%
    addProviderTiles("CartoDB.Positron")
  
  colors <- c("#D55E00", "#0072B2")
  
  output$dropout_map <- renderLeaflet({
    grad_map <- basemap %>%
      addMinicharts(
        rates$lon,
        rates$lat,
        chartdata = rates[, c("Black Students", "All Students")],
        time = rates$`Cohort Year`,
        colorPalette = colors,
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
  var_medianIncome <- reactive({
    input$MedianIncomeYearDrop
  })
  
  output$income_plot <- renderPlot({
    if (var_medianIncome() %in% c("2019", "2018", "2017")) {
      if (var_medianIncome() == "2019") {
        va_yr <- read.csv("data/income/va_income2019.csv")
        hamp_yr <- read.csv("data/income/hampton_income2019.csv")
      }
      else if (var_medianIncome() == "2018") {
        va_yr <- read.csv("data/income/va_income2018.csv")
        hamp_yr <- read.csv("data/income/hampton_income2018.csv")
      }
      else if (var_medianIncome() == "2017") {
        va_yr <- read.csv("data/income/va_income2017.csv")
        hamp_yr <- read.csv("data/income/hampton_income2017.csv")
      }
      va_yr <- va_yr[2:6]
      race_names <- c("Total", "Black")
      #VA income
      va_race_income_median <- data.frame(va_yr[c(81, 83), 4])
      va_race_income <-
        data.frame(cbind(race_names, va_race_income_median))
      colnames(va_race_income) <- c("Race", "Median Income")
      #Hampton Income
      hamp_yr <- hamp_yr[2:6]
      #getting the name, variable and estimate
      hamp_income2 <- hamp_yr[, 2:4]
      hamp_income3 <- hamp_income2 %>%
        group_by(NAME) %>%
        slice(c(81, 83))
      variable <-
        sample(c("S1903_C03_001", "S1903_C03_003"), 32, replace = TRUE)
      hamp_race_income_median <-
        hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
      #Putting them together
      median_income <-
        cbind(va_race_income, hamp_race_income_median)
      median_income <- median_income[, c(2, 4)]
      #having all the estimates in the same column
      median_income2 <-
        data.frame(median = c(median_income[, 1], median_income[, 2]))
      #labeling
      median_income2 <-
        mutate(median_income2, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
      median_income2 <-
        mutate(median_income2, demo = rep(c(
          "Total Population", "Black Population"
        ), 2))
      colnames(median_income2) <-
        c("Median Income (US Dollars)", "Location", "Demographic")
      #making them all numeric
      median_income2 <-
        transform(median_income2,
                  `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
      colnames(median_income2) <-
        c("Median Income (US Dollars)", "Location", "Demographic")
      median_income2$Location <-
        factor(median_income2$Location,
               levels = c("Hampton Roads", "Virginia"))
      #Graph
      income_plot <-
        ggplot(
          median_income2,
          aes(x = Location, y = `Median Income (US Dollars)`, fill = Demographic)
        ) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(
          aes(label = paste0(round(
            `Median Income (US Dollars)`
          ))),
          vjust = 1.5,
          color = "white",
          position = position_dodge(0.9),
          size = 5
        ) +
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
          axis.title.y = element_text(margin = margin(
            t = 0,
            r = 20,
            b = 0,
            l = 0
          ))
        ) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      #plot
      income_plot
    }
    else if (var_medianIncome() %in% c("2016", "2015", "2014", "2013", "2012", "2011", "2010")) {
      if (var_medianIncome() == "2016") {
        va_yr <- read.csv("data/income/va_income2016.csv")
        hamp_yr <- read.csv("data/income/hampton_income2016.csv")
      }
      else if (var_medianIncome() == "2015") {
        va_yr <- read.csv("data/income/va_income2015.csv")
        hamp_yr <- read.csv("data/income/hampton_income2015.csv")
      }
      else if (var_medianIncome() == "2014") {
        va_yr <- read.csv("data/income/va_income2014.csv")
        hamp_yr <- read.csv("data/income/hampton_income2014.csv")
      }
      else if (var_medianIncome() == "2013") {
        va_yr <- read.csv("data/income/va_income2013.csv")
        hamp_yr <- read.csv("data/income/hampton_income2013.csv")
      }
      else if (var_medianIncome() == "2012") {
        va_yr <- read.csv("data/income/va_income2012.csv")
        hamp_yr <- read.csv("data/income/hampton_income2012.csv")
      }
      else if (var_medianIncome() == "2011") {
        va_yr <- read.csv("data/income/va_income2011.csv")
        hamp_yr <- read.csv("data/income/hampton_income2011.csv")
      }
      else if (var_medianIncome() == "2010") {
        va_yr <- read.csv("data/income/va_income2010.csv")
        hamp_yr <- read.csv("data/income/hampton_income2010.csv")
      }
      
      va_yr <- read.csv("data/income/va_income2016.csv")
      va_yr <- va_yr[, 2:6]
      #income by Race
      race_names <- c("Total", "Black")
      #median income
      va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
      va_race_income <-
        data.frame(cbind(race_names, va_race_income_median))
      colnames(va_race_income) <- c("Race", "Median Income")
      #Hampton Income
      #getting the name, variable and estimate
      hamp_income2 <- hamp_yr[, 2:6]
      hamp_income3 <- hamp_income2 %>%
        group_by(NAME) %>%
        slice(c(31, 33))
      variable <-
        sample(c("S1903_C02_001", "S1903_C02_003"), 32, replace = TRUE)
      hamp_race_income_median <-
        hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
      #Putting them in the same dataset
      median_income <-
        cbind(va_race_income, hamp_race_income_median)
      median_income <- median_income[, c(2, 4)]
      #having all the estimates in the same column
      median_income2 <-
        data.frame(median = c(median_income[, 1], median_income[, 2]))
      median_income2 <-
        mutate(median_income2, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
      median_income2 <-
        mutate(median_income2, demo = rep(c(
          "Total Population", "Black Population"
        ), 2))
      colnames(median_income2) <-
        c("Median Income (US Dollars)", "Location", "Demographic")
      #making them all numeric
      median_income2 <-
        transform(median_income2,
                  `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
      colnames(median_income2) <-
        c("Median Income (US Dollars)", "Location", "Demographic")
      median_income2$Location <-
        factor(median_income2$Location,
               levels = c("Hampton Roads", "Virginia"))
      #Hampton and VA graph
      income_plot <-
        ggplot(
          median_income2,
          aes(x = Location, y = `Median Income (US Dollars)`, fill = Demographic)
        ) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(
          aes(label = paste0(round(
            `Median Income (US Dollars)`
          ))),
          vjust = 1.5,
          color = "white",
          position = position_dodge(0.9),
          size = 5
        ) +
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
          axis.title.y = element_text(margin = margin(
            t = 0,
            r = 20,
            b = 0,
            l = 0
          ))
        ) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      #plot
      income_plot
    }
    
    
  })
  
  # Median Income line plots -------------------------------------------------
  
  output$medianTimeGraph <- renderPlot ({
    va_yr <- read.csv("data/income/va_income2019.csv")
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    #median income
    va_race_income_median <- data.frame(va_yr[c(81, 83), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2019.csv")
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(81, 83))
    #This give us overall hampton overall and black median income
    variable <-
      sample(c("S1903_C03_001", "S1903_C03_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income19 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income19 <-
      mutate(median_income19, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income19 <-
      mutate(median_income19, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income19) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income19 <-
      transform(median_income19,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income19) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income19 <- mutate(median_income19, Year = "2019")
    ############################################################################2018
    va_yr <- read.csv("data/income/va_income2018.csv")
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    #median income
    va_race_income_median <- data.frame(va_yr[c(81, 83), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2018.csv")
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(81, 83))
    #This give us overall hampton overall and black median income
    variable <-
      sample(c("S1903_C03_001", "S1903_C03_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income18 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income18 <-
      mutate(median_income18, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income18 <-
      mutate(median_income18, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income18) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income18 <-
      transform(median_income18,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income18) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income18 <- mutate(median_income18, Year = "2018")
    ############################################################################2017
    va_yr <- read.csv("data/income/va_income2017.csv")
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    #median income
    va_race_income_median <- data.frame(va_yr[c(81, 83), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2017.csv")
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(81, 83))
    #This give us overall hampton overall and black median income
    variable <-
      sample(c("S1903_C03_001", "S1903_C03_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income17 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income17 <-
      mutate(median_income17, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income17 <-
      mutate(median_income17, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income17) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income17 <-
      transform(median_income17,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income17) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income17 <- mutate(median_income17, Year = "2017")
    ###########################################################################2016
    va_yr <- read.csv("data/income/va_income2016.csv")
    va_yr <- va_yr[, 2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2016.csv")
    hamp_yr <- hamp_yr[, 2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31, 33))
    variable <-
      sample(c("S1903_C02_001", "S1903_C02_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income16 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income16 <-
      mutate(median_income16, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income16 <-
      mutate(median_income16, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income16) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income16 <-
      transform(median_income16,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income16) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income16 <- mutate(median_income16, Year = "2016")
    ###########################################################################2016
    va_yr <- read.csv("data/income/va_income2015.csv")
    va_yr <- va_yr[, 2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2015.csv")
    hamp_yr <- hamp_yr[, 2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31, 33))
    variable <-
      sample(c("S1903_C02_001", "S1903_C02_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income15 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income15 <-
      mutate(median_income15, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income15 <-
      mutate(median_income15, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income15) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income15 <-
      transform(median_income15,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income15) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income15 <- mutate(median_income15, Year = "2015")
    ###########################################################################2014
    va_yr <- read.csv("data/income/va_income2014.csv")
    va_yr <- va_yr[, 2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2014.csv")
    hamp_yr <- hamp_yr[, 2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31, 33))
    variable <-
      sample(c("S1903_C02_001", "S1903_C02_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income14 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income14 <-
      mutate(median_income14, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income14 <-
      mutate(median_income14, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income14) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income14 <-
      transform(median_income14,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income14) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income14 <- mutate(median_income14, Year = "2014")
    ###########################################################################2013
    va_yr <- read.csv("data/income/va_income2013.csv")
    va_yr <- va_yr[, 2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2013.csv")
    hamp_yr <- hamp_yr[, 2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31, 33))
    variable <-
      sample(c("S1903_C02_001", "S1903_C02_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income13 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income13 <-
      mutate(median_income13, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income13 <-
      mutate(median_income13, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income13) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income13 <-
      transform(median_income13,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income13) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income13 <- mutate(median_income13, Year = "2013")
    ############################################################################2012
    va_yr <- read.csv("data/income/va_income2012.csv")
    va_yr <- va_yr[, 2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2012.csv")
    hamp_yr <- hamp_yr[, 2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31, 33))
    variable <-
      sample(c("S1903_C02_001", "S1903_C02_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income12 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income12 <-
      mutate(median_income12, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income12 <-
      mutate(median_income12, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income12) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income12 <-
      transform(median_income12,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income12) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income12 <- mutate(median_income12, Year = "2012")
    ###########################################################################2011
    va_yr <- read.csv("data/income/va_income2011.csv")
    va_yr <- va_yr[, 2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2011.csv")
    hamp_yr <- hamp_yr[, 2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31, 33))
    variable <-
      sample(c("S1903_C02_001", "S1903_C02_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income11 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income11 <-
      mutate(median_income11, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income11 <-
      mutate(median_income11, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income11) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income11 <-
      transform(median_income11,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income11) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income11 <- mutate(median_income11, Year = "2011")
    ###########################################################################2010
    va_yr <- read.csv("data/income/va_income2010.csv")
    va_yr <- va_yr[, 2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31, 33), 4])
    va_race_income <-
      data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2010.csv")
    hamp_yr <- hamp_yr[, 2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[, 2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31, 33))
    variable <-
      sample(c("S1903_C02_001", "S1903_C02_003"), 32, replace = TRUE)
    hamp_race_income_median <-
      hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2, 4)]
    #having all the estimates in the same column
    median_income10 <-
      data.frame(median = c(median_income[, 1], median_income[, 2]))
    #labeling
    median_income10 <-
      mutate(median_income10, location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
    median_income10 <-
      mutate(median_income10, demo = rep(c(
        "Total Population", "Black Population"
      ), 2))
    colnames(median_income10) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income10 <-
      transform(median_income10,
                `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income10) <-
      c("Median Income (US Dollars)", "Location", "Demographic")
    median_income10 <- mutate(median_income10, Year = "2010")
    ###############################################################################Combined
    income_years <-
      rbind(
        median_income19,
        median_income18,
        median_income17,
        median_income16,
        median_income15,
        median_income14,
        median_income13,
        median_income12,
        median_income11,
        median_income10
      )
    #VA line graph
    va_years <- income_years %>% filter(Location == "Virginia")
    #graph
    va_line <-
      ggplot(
        va_years,
        aes(
          x = Year,
          y = `Median Income (US Dollars)`,
          group = Demographic,
          color = Demographic
        )
      ) +
      geom_line(position = "identity", size = 1.3) +
      theme_minimal() +
      ggtitle("Virginia") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank()) +
      theme(
        legend.title = element_blank(),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12)
      ) +
      labs(y = "Median Income (US Dollars)") +
      scale_color_manual(values = c("#D55E00", "#0072B2")) +
      ylim(35000, 75000)
    
    #hamp line graph
    hamp_years <-
      income_years %>% filter(Location == "Hampton Roads")
    #graph
    hamp_line <-
      ggplot(
        hamp_years,
        aes(
          x = Year,
          y = `Median Income (US Dollars)`,
          group = Demographic,
          color = Demographic
        )
      ) +
      geom_line(position = "identity", size = 1.3) +
      scale_color_manual(values = c("#D55E00", "#0072B2")) +
      theme_minimal() +
      ggtitle("Hampton Roads") +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12)
      ) +
      labs(y = "Median Income (US Dollars)") +
      ylim(35000, 75000)
    
    medianTimeGraph  <- grid.arrange(hamp_line, va_line, ncol = 2)
    medianTimeGraph
  })
  # Employment By Sector ----------------------------------------------------
  var_sectorEmployment <- reactive({
    input$SectorEmploymentYearDrop
  })
  
  output$sector_plot <- renderPlotly({
    if (var_sectorEmployment() == "2019") {
      sectors2019 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2019.csv")
      colnames(sectors2019) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2019 <- sectors2019  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40)) +  scale_fill_viridis_d()
      sectors2019  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2019 , tooltip = c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2018") {
      sectors2018 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2018.csv")
      colnames(sectors2018) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2018 <- sectors2018  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2018  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2018 , tooltip = c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2017") {
      sectors2017 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2017.csv")
      colnames(sectors2017) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2017 <- sectors2017  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40)) + scale_fill_viridis_d()
      sectors2017  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2017 , tooltip = c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2016") {
      sectors2016 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2016.csv")
      colnames(sectors2016) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2016 <- sectors2016  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2016  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2016, tooltip = c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2015") {
      sectors2015 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2015.csv")
      colnames(sectors2015) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2015 <- sectors2015  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2015  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2015, tooltip = c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2014") {
      sectors2014 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2014.csv")
      colnames(sectors2014) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2014 <- sectors2014  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2014  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2014, tooltip = c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2013") {
      sectors2013 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2013.csv")
      colnames(sectors2013) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2013 <- sectors2013  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40)) +  scale_fill_viridis_d()
      sectors2013  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2013, tooltip = c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2012") {
      sectors2012 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2012.csv")
      colnames(sectors2012) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2012 <- sectors2012  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40)) +  scale_fill_viridis_d()
      sectors2012  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2012, tooltip = c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2011") {
      sectors2011 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2011.csv")
      colnames(sectors2011) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2011 <- sectors2011  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2011  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2011, tooltip = c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2010") {
      sectors2010 <-
        read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2010.csv")
      colnames(sectors2010) <-
        c("Name",
          "Variable",
          "Number of People Employed in Sector",
          "Sector")
      sectors2010 <- sectors2010  %>%
        mutate(Name = str_remove(Name, "County, Virginia")) %>%
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  +
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2010 #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2010, tooltip = c("x", "y", "Sector")))
    }
  })
  
  # Unemployment Rate -------------------------------------------------------
  
  
  var_unemploymentRate <- reactive({
    input$UnemploymentRateSlider
  })
  
  output$unemployment_plot <- renderPlotly({
    if (var_unemploymentRate() == "2019") {
      unemp_19 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2019.csv")
      colnames(unemp_19)[2] <- "Locality"
      colnames(unemp_19)[3] <- "Demographic"
      colnames(unemp_19)[4] <- "Unemployment Rate"
      va_unemp_19 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2019.csv")
      unemployment_2019 <- unemp_19 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_19$estimate,
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
          caption = ""
        ) +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2019)
      
      
    }
    
    else if (var_unemploymentRate() == "2018") {
      unemp_18 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2018.csv")
      colnames(unemp_18)[2] <- "Locality"
      colnames(unemp_18)[3] <- "Demographic"
      colnames(unemp_18)[4] <- "Unemployment Rate"
      va_unemp_18 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2018.csv")
      unemployment_2018 <- unemp_18 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_18$estimate,
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
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2018)
      
      
    }
    
    else if (var_unemploymentRate() == "2017") {
      unemp_17 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2017.csv")
      colnames(unemp_17)[2] <- "Locality"
      colnames(unemp_17)[3] <- "Demographic"
      colnames(unemp_17)[4] <- "Unemployment Rate"
      va_unemp_17 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2017.csv")
      unemployment_2017 <- unemp_17 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_17$estimate,
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
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2017)
      
      
    }
    
    else if (var_unemploymentRate() == "2016") {
      unemp_16 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2016.csv")
      colnames(unemp_16)[2] <- "Locality"
      colnames(unemp_16)[3] <- "Demographic"
      colnames(unemp_16)[4] <- "Unemployment Rate"
      va_unemp_16 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2016.csv")
      unemployment_2016 <- unemp_16 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_16$estimate,
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
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2016)
      
      
    }
    
    else if (var_unemploymentRate() == "2015") {
      unemp_15 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2015.csv")
      colnames(unemp_15)[2] <- "Locality"
      colnames(unemp_15)[3] <- "Demographic"
      colnames(unemp_15)[4] <- "Unemployment Rate"
      va_unemp_15 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2015.csv")
      unemployment_2015 <- unemp_15 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_15$estimate,
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
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2015)
      
      
    }
    
    else if (var_unemploymentRate() == "2014") {
      unemp_14 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2014.csv")
      colnames(unemp_14)[2] <- "Locality"
      colnames(unemp_14)[3] <- "Demographic"
      colnames(unemp_14)[4] <- "Unemployment Rate"
      va_unemp_14 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2014.csv")
      unemployment_2014 <- unemp_14 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_14$estimate,
          linetype = "dashed",
          color = "red",
          show.legend = TRUE
        ) +
        theme(legend.title = element_blank()) +
        labs(
          title = "",
          y = "Unemployment Rate (%)",
          x = "",
          caption = "Source: ACS 5 Year Estimate Table S2301"
        ) +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2014)
      
      
    }
    
    else if (var_unemploymentRate() == "2013") {
      unemp_13 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2013.csv")
      colnames(unemp_13)[2] <- "Locality"
      colnames(unemp_13)[3] <- "Demographic"
      colnames(unemp_13)[4] <- "Unemployment Rate"
      va_unemp_13 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2013.csv")
      unemployment_2013 <- unemp_13 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_13$estimate,
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
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2013)
      
      
    }
    
    else if (var_unemploymentRate() == "2012") {
      unemp_12 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2012.csv")
      colnames(unemp_12)[2] <- "Locality"
      colnames(unemp_12)[3] <- "Demographic"
      colnames(unemp_12)[4] <- "Unemployment Rate"
      va_unemp_12 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2012.csv")
      unemployment_2012 <- unemp_12 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_12$estimate,
          linetype = "dashed",
          color = "red",
          show.legend = TRUE
        ) +
        theme(legend.title = element_blank()) +
        labs(
          title = "",
          y = "Unemployment Rate (%)",
          x = "",
          caption = "Source: ACS 5 Year Estimate Table S2301"
        ) +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2012)
      
      
    }
    
    else if (var_unemploymentRate() == "2011") {
      unemp_11 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2011.csv")
      colnames(unemp_11)[2] <- "Locality"
      colnames(unemp_11)[3] <- "Demographic"
      colnames(unemp_11)[4] <- "Unemployment Rate"
      va_unemp_11 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2011.csv")
      unemployment_2011 <- unemp_11 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_11$estimate,
          linetype = "dashed",
          color = "red",
          show.legend = TRUE
        ) +
        theme(legend.title = element_blank()) +
        labs(
          title = "",
          y = "Unemployment Rate (%)",
          x = "",
          caption = "Source: ACS 5 Year Estimate Table S2301"
        ) +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2011)
      
      
    }
    
    else if (var_unemploymentRate() == "2010") {
      unemp_10 <-
        read.csv("data/TableS2301FiveYearEstimates/unemployment2010.csv")
      colnames(unemp_10)[2] <- "Locality"
      colnames(unemp_10)[3] <- "Demographic"
      colnames(unemp_10)[4] <- "Unemployment Rate"
      va_unemp_10 <-
        read.csv("data/TableS2301FiveYearEstimates/vaunemployment2010.csv")
      unemployment_2010 <- unemp_10 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>%
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(
          yintercept = va_unemp_10$estimate,
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
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2010)
      
      
    }
  })
  
  # Poverty Rates in VA and Hampton Roads-------------------------------------
  var_poverty <- reactive ({
    input$PovertyYearDrop
  })
  
  output$pov_plot <- renderPlot({
    if (var_poverty() %in% c("2019", "2018", "2017", "2016", "2015")) {
      if (var_poverty() == "2019") {
        va_pov <- read.csv("data/poverty/va_poverty2019.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2019.csv")
      }
      else if (var_poverty() == "2018") {
        va_pov <- read.csv("data/poverty/va_poverty2018.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2018.csv")
      }
      else if (var_poverty() == "2017") {
        va_pov <- read.csv("data/poverty/va_poverty2017.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2017.csv")
      }
      else if (var_poverty() == "2016") {
        va_pov <- read.csv("data/poverty/va_poverty2016.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2016.csv")
      }
      else if (var_poverty() == "2015") {
        va_pov <- read.csv("data/poverty/va_poverty2015.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2015.csv")
      }
      va_pov <- va_pov[, 2:6]
      va_pct_pov <- va_pov[123, 4]
      va_pct_pov_blck <- va_pov[136, 4]
      va_pov_vector <- rbind(va_pct_pov, va_pct_pov_blck)
      hamp_pov <- hamp_pov[, 2:6]
      #General
      hamp_total <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(1))
      hamp_total2 <- colSums(hamp_total[, 4])
      hamp_pov2 <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(62))
      hamp_pov3 <- colSums(hamp_pov2[, 4])
      hamp_overall_pov <- hamp_pov3 / hamp_total2 * 100
      #Black
      hamp_total_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(14)
      hamp_total_blck2 <- colSums(hamp_total_blck[, 4])
      hamp_pov_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(75)
      hamp_pov_blck2 <- colSums(hamp_pov_blck[, 4])
      hamp_blck_overall_pov <-
        hamp_pov_blck2 / hamp_total_blck2 * 100
      hamp_pov_vector <-
        rbind(hamp_overall_pov, hamp_blck_overall_pov)
      #combing hampton and VA
      pov_pct <- data.frame(va_pov_vector, hamp_pov_vector)
      pov_pct2 <-
        data.frame(Ratio = unlist(pov_pct, use.names = FALSE))
      pov_pct2 <-
        mutate(pov_pct2,
               Location = c("Virginia", "Virginia", "Hampton Roads", "Hampton Roads"))
      pov_pct2 <-
        mutate(
          pov_pct2,
          Demographic = c(
            "Total Population",
            "Black Population",
            "Total Population",
            "Black Population"
          )
        )
      colnames(pov_pct2) <-
        c("Percentage (%)", "Location", "Demographic")
      pov_pct2$Location <-
        factor(pov_pct2$Location, levels = c("Hampton Roads", "Virginia"))
      #Graph for just hampton roads and VA
      pov_plot <-
        ggplot(pov_pct2,
               aes(x = Location, y = `Percentage (%)`, fill = Demographic)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(
          aes(label = paste0(
            round(`Percentage (%)`, digits = 2), "%"
          )),
          vjust = 1.5,
          color = "white",
          position = position_dodge(0.9),
          size = 5
        ) +
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
          axis.title.y = element_text(margin = margin(
            t = 0,
            r = 20,
            b = 0,
            l = 0
          ))
        ) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      #plot
      pov_plot
    }
    #when table changes
    else if (var_poverty() %in% c("2014", "2013", "2012")) {
      if (var_poverty() == "2014") {
        va_pov <- read.csv("data/poverty/va_poverty2014.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2014.csv")
      }
      else if (var_poverty() == "2013") {
        va_pov <- read.csv("data/poverty/va_poverty2013.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2013.csv")
      }
      else if (var_poverty() == "2012") {
        va_pov <- read.csv("data/poverty/va_poverty2012.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2012.csv")
      }
      va_pov <- va_pov[, 2:6]
      #General
      va_pct_pov <- va_pov[93, 4]
      #Black
      va_pct_pov_blck <- va_pov[102, 4]
      va_pov_vector <- rbind(va_pct_pov, va_pct_pov_blck)
      #Hampton Roads
      hamp_pov <- hamp_pov[, 2:6]
      #General
      hamp_total <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(1))
      hamp_total2 <- colSums(hamp_total[, 4])
      hamp_pov2 <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(47))
      hamp_pov3 <- colSums(hamp_pov2[, 4])
      hamp_overall_pov <- hamp_pov3 / hamp_total2 * 100
      #Black
      hamp_total_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(10)
      hamp_total_blck2 <- colSums(hamp_total_blck[, 4])
      hamp_pov_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(56)
      hamp_pov_blck2 <- colSums(hamp_pov_blck[, 4])
      hamp_blck_overall_pov <-
        hamp_pov_blck2 / hamp_total_blck2 * 100
      hamp_pov_vector <-
        rbind(hamp_overall_pov, hamp_blck_overall_pov)
      #combing hampton and VA
      pov_pct <- data.frame(va_pov_vector, hamp_pov_vector)
      pov_pct2 <-
        data.frame(Ratio = unlist(pov_pct, use.names = FALSE))
      pov_pct2 <-
        mutate(pov_pct2, Location = c(rep("Virginia", 2), rep("Hampton Roads", 2)))
      pov_pct2 <-
        mutate(pov_pct2, Demographic = rep(c(
          "Total Population", "Black Population"
        ), 2))
      colnames(pov_pct2) <-
        c("Percentage (%)", "Location", "Demographic")
      pov_pct2$Location <-
        factor(pov_pct2$Location, levels = c("Hampton Roads", "Virginia"))
      #Graph for just hampton roads and VA
      pov_plot <-
        ggplot(pov_pct2,
               aes(x = Location, y = `Percentage (%)`, fill = Demographic)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(
          aes(label = paste0(
            round(`Percentage (%)`, digits = 2), "%"
          )),
          vjust = 1.5,
          color = "white",
          position = position_dodge(0.9),
          size = 5
        ) +
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
          axis.title.y = element_text(margin = margin(
            t = 0,
            r = 20,
            b = 0,
            l = 0
          ))
        ) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      #plot
      pov_plot
    }
    
    
  })
  
  # Hampton counties poverty -------------------------------------------------------------
  var_povertyCount <- reactive ({
    input$PovertyCountYearDrop
  })
  
  output$counties_pov <- renderPlotly({
    if (var_povertyCount() %in% c("2019", "2018", "2017", "2016", "2015")) {
      if (var_povertyCount() == "2019") {
        hamp_pov <- read.csv("data/poverty/hamp_poverty2019.csv")
      }
      else if (var_povertyCount() == "2018") {
        hamp_pov <- read.csv("data/poverty/hamp_poverty2018.csv")
      }
      else if (var_povertyCount() == "2017") {
        hamp_pov <- read.csv("data/poverty/hamp_poverty2017.csv")
      }
      else if (var_povertyCount() == "2016") {
        hamp_pov <- read.csv("data/poverty/hamp_poverty2016.csv")
      }
      else if (var_povertyCount() == "2015") {
        hamp_pov <- read.csv("data/poverty/hamp_poverty2015.csv")
      }
      hamp_pov <- hamp_pov[, 2:6]
      hamp_pov_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(123)
      hamp_pov_blck_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(136)
      hamp_pov_tbl %>% ungroup()
      hamp_pov_blck_tbl %>% ungroup()
      hamp_pctG <- hamp_pov_tbl[, 4]
      hamp_pctB <- hamp_pov_blck_tbl[, 4]
      hamp_comb <- rbind(hamp_pctG, hamp_pctB)
      colnames(hamp_comb) <- "Ratio"
      hamp_comb <-
        mutate(hamp_comb, Location =  rep(
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
          ),
          2
        ))
      hamp_comb <-
        mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                          rep("Black Population", 16)))
      colnames(hamp_comb) <-
        c("Percentage (%)", "Location", "Demographic")
      hamp_comb <-
        hamp_comb %>% filter(hamp_comb[, 2] != "Poquoson")
      #Graph
      counties_pov <-
        ggplot(hamp_comb,
               aes(Location, y = `Percentage (%)`, fill = Demographic)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        #geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
        #position = position_dodge(0.9), size=3)+
        theme_minimal() +
        scale_fill_manual(values = c("#D55E00", "#0072B2")) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 25),
          legend.key.size = unit(1, 'cm'),
          #legend.key.height = unit(0.5, 'cm'),
          #legend.key.width = unit(0.5, 'cm'),
          legend.title = element_blank(),
          #legend.text = element_text(size=14),
          #axis.text=element_text(size=15),
          #axis.text.x = element_text(size=8, face="bold"),
          #axis.title=element_text(size=17),
          axis.title.x = element_blank()
        ) +
        theme(axis.text.x = element_text(
          angle = 40,
          vjust = 0.95,
          hjust = 1
        )) +
        labs(caption = "Source: ACS 5 Year Estimate Table S1701")
      #plot
      counties_pov <- ggplotly(counties_pov)
    }
    #when table changes
    else if (var_povertyCount() %in% c("2014", "2013", "2012")) {
      if (var_povertyCount() == "2014") {
        hamp_pov <- read.csv("data/poverty/hamp_poverty2014.csv")
      }
      else if (var_povertyCount() == "2013") {
        hamp_pov <- read.csv("data/poverty/hamp_poverty2013.csv")
      }
      else if (var_povertyCount() == "2012") {
        hamp_pov <- read.csv("data/poverty/hamp_poverty2012.csv")
      }
      hamp_pov <- hamp_pov[, 2:6]
      hamp_pov_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(93)
      hamp_pov_blck_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(102)
      hamp_pov_tbl %>% ungroup()
      hamp_pov_blck_tbl %>% ungroup()
      hamp_pctG <- hamp_pov_tbl[, 4]
      hamp_pctB <- hamp_pov_blck_tbl[, 4]
      hamp_comb <- rbind(hamp_pctG, hamp_pctB)
      colnames(hamp_comb) <- "Ratio"
      hamp_comb <-
        mutate(hamp_comb, Location = c(rep(
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
          ),
          2
        )))
      hamp_comb <-
        mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                          rep("Black Population", 16)))
      colnames(hamp_comb) <-
        c("Percentage (%)", "Location", "Demographic")
      hamp_comb <-
        hamp_comb %>% filter(hamp_comb[, 2] != "Poquoson")
      #Graph
      
      counties_pov <-
        ggplot(hamp_comb,
               aes(Location, y = `Percentage (%)`, fill = Demographic)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        #geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
        #position = position_dodge(0.9), size=3)+
        theme_minimal() +
        scale_fill_manual(values = c("#D55E00", "#0072B2")) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 25),
          legend.key.size = unit(1, 'cm'),
          legend.title = element_blank(),
          #legend.text = element_text(size=14),
          # axis.text=element_text(size=15),
          #axis.text.x = element_text(size=10, face="bold"),
          #axis.title=element_text(size=17),
          axis.title.x = element_blank()
        ) +
        theme(axis.text.x = element_text(
          angle = 40,
          vjust = 0.95,
          hjust = 1
        )) +
        labs(caption = "Source: ACS 5 Year Estimate Table S1701")
      
      #plot
      counties_pov <- ggplotly(counties_pov)
    }
    
  })
  
  
  
  # Uninsured Population ----------------------------------------------------
  
  var_uninsuredpct <- reactive({
    input$UninsuredPctSlider
  })
  
  output$uninsured_plot <- renderPlotly({
    if (var_uninsuredpct() == "2019") {
      unins_19 <-
        read.csv("data/TableS2701FiveYearEstimates/uninsured2019.csv")
      colnames(unins_19)[1] <- "Locality"
      colnames(unins_19)[2] <- "Demographic"
      colnames(unins_19)[3] <- "Percent Uninsured"
      unins_19 <- unins_19 %>%
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2019 <- unins_19 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
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
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2019)
      
    }
    
    else if (var_uninsuredpct() == "2018") {
      unins_18 <-
        read.csv("data/TableS2701FiveYearEstimates/uninsured2018.csv")
      colnames(unins_18)[1] <- "Locality"
      colnames(unins_18)[2] <- "Demographic"
      colnames(unins_18)[3] <- "Percent Uninsured"
      unins_18 <- unins_18 %>%
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2018 <- unins_18 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
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
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2018)
    }
    
    else if (var_uninsuredpct() == "2017") {
      unins_17 <-
        read.csv("data/TableS2701FiveYearEstimates/uninsured2017.csv")
      colnames(unins_17)[1] <- "Locality"
      colnames(unins_17)[2] <- "Demographic"
      colnames(unins_17)[3] <- "Percent Uninsured"
      unins_17 <- unins_17 %>%
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2017 <- unins_17 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
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
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2017)
    }
    
    else if (var_uninsuredpct() == "2016") {
      unins_16 <-
        read.csv("data/TableS2701FiveYearEstimates/uninsured2016.csv")
      colnames(unins_16)[1] <- "Locality"
      colnames(unins_16)[2] <- "Demographic"
      colnames(unins_16)[3] <- "Percent Uninsured"
      unins_16 <- unins_16 %>%
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2016 <- unins_16 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
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
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2016)
    }
    
    else if (var_uninsuredpct() == "2015") {
      unins_15 <-
        read.csv("data/TableS2701FiveYearEstimates/uninsured2015.csv")
      colnames(unins_15)[1] <- "Locality"
      colnames(unins_15)[2] <- "Demographic"
      colnames(unins_15)[3] <- "Percent Uninsured"
      unins_15 <- unins_15 %>%
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2015 <- unins_15 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
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
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2015)
    }
    
    else if (var_uninsuredpct() == "2014") {
      unins_14 <-
        read.csv("data/TableS2701FiveYearEstimates/uninsured2014.csv")
      colnames(unins_14)[1] <- "Locality"
      colnames(unins_14)[2] <- "Demographic"
      colnames(unins_14)[3] <- "Percent Uninsured"
      unins_14 <- unins_14 %>%
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2014 <- unins_14 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
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
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2014)
    }
    
    else if (var_uninsuredpct() == "2013") {
      unins_13 <-
        read.csv("data/TableS2701FiveYearEstimates/uninsured2013.csv")
      colnames(unins_13)[1] <- "Locality"
      colnames(unins_13)[2] <- "Demographic"
      colnames(unins_13)[3] <- "Percent Uninsured"
      unins_13 <- unins_13 %>%
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2013 <- unins_13 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
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
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2013)
    }
    
    else if (var_uninsuredpct() == "2012") {
      unins_12 <-
        read.csv("data/TableS2701FiveYearEstimates/uninsured2012.csv")
      
      colnames(unins_12)[1] <- "Locality"
      colnames(unins_12)[2] <- "Demographic"
      colnames(unins_12)[3] <- "Percent Uninsured"
      unins_12 <- unins_12 %>%
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2012 <- unins_12 %>%
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>%
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
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
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2012)
    }
    
  })
  
  
  # Veteran Status ----------------------------------------------------------
  var_veteran <- reactive({
    input$VeteranSlider
  })
  
  output$veteran_map <- renderLeaflet({
    if (var_veteran() == "2019") {
      vet_19 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2019.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_19$Percent,
          reverse = TRUE
        )
      veteran_19 <- vet_19 %>%
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
    }
    
    else if (var_veteran() == "2018") {
      vet_18 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2018.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_18$Percent,
          reverse = TRUE
        )
      veteran_18 <- vet_18 %>%
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
    }
    
    else if (var_veteran() == "2017") {
      vet_17 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2017.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_17$Percent,
          reverse = TRUE
        )
      veteran_17 <- vet_17 %>%
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
    }
    
    else if (var_veteran() == "2016") {
      vet_16 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2016.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_16$Percent,
          reverse = TRUE
        )
      veteran_16 <- vet_16 %>%
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
    }
    
    else if (var_veteran() == "2015") {
      vet_15 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2015.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_15$Percent,
          reverse = TRUE
        )
      veteran_15 <- vet_15 %>%
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
    }
    
    else if (var_veteran() == "2014") {
      vet_14 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2014.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_14$Percent,
          reverse = TRUE
        )
      veteran_14 <- vet_14 %>%
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
    }
    
    else if (var_veteran() == "2013") {
      vet_13 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2013.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_13$Percent,
          reverse = TRUE
        )
      veteran_13 <- vet_13 %>%
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
    }
    
    else if (var_veteran() == "2012") {
      vet_12 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2012.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_12$Percent,
          reverse = TRUE
        )
      veteran_12 <- vet_12 %>%
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
    }
    
    else if (var_veteran() == "2011") {
      vet_11 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2011.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_11$Percent,
          reverse = TRUE
        )
      veteran_11 <- vet_11 %>%
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
    }
    
    else if (var_veteran() == "2010") {
      vet_10 <-
        read_rds("data/TableS2101FiveYearEstimates/bveteran2010.rds")
      military_bases <-
        read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <-
        colorNumeric(
          palette = "viridis",
          domain = vet_10$Percent,
          reverse = TRUE
        )
      veteran_10 <- vet_10 %>%
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
    }
    
  })
  
  
  # Home Ownership Map -------------------------------------------------------
  var_hmown <- reactive({
    input$HomeOwnSlider
  })
  
  
  output$homeownership_map <- renderLeaflet({
    b_hm_19 <-
      read_rds("data/TableS2502FiveYearEstimates/bhmown2019.rds")
    tot_hm_19 <-
      read_rds("data/TableS2502FiveYearEstimates/tothmown2019.rds")
    all_hm_data <-
      read_rds("data/TableS2502FiveYearEstimates/allhomedata.rds")
    colnames(all_hm_data)[2] <- "Demographic"
    
    pick_n <- function(Locality) {
      dataFiltered <- filter(all_hm_data, NAME == Locality)
      
      hm_line <-
        ggplot(dataFiltered,
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
        scale_fill_manual(values = c("#D55E00", "#0072B2")) +
        theme(legend.position = "bottom") +
        labs(title = Locality)
      
      #ggplotly(hm_line)
    }
    
    r <- lapply(1:length(unique(b_hm_19$NAME)), function(i) {
      pick_n(b_hm_19$NAME[i])
    })
    
    pal <-
      colorNumeric(palette = "viridis",
                   domain = b_hm_19$Percent,
                   reverse = TRUE)
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
  
  # People & Values ---------------------------------------------------------
  var_fam <- reactive({
    input$select_family
  })
  
  
  output$family_maps <- renderLeaflet({
    if(var_fam() == "Percent of Black Children under 18 in Female Head of Household") {
      
      fml <- read_rds("data/fml.rds")
      fml <- fml %>%
        na.omit(fml)
      colnames(fml)[4] <- "Percent"
      fempal <-
        colorNumeric(
          palette = "viridis",
          domain = fml$Percent,
          reverse = TRUE
        )
      
      fml_map <- fml %>%
        leaflet(options = leafletOptions(
          minZoom = 5,
          maxZoom = 15,
          drag = FALSE
        )) %>%
        addProviderTiles("CartoDB.PositronNoLabels") %>%
        addPolygons(
          color = ~ fempal(Percent),
          weight = 0.5,
          fillOpacity = 0.7,
          smoothFactor = 0,
          highlightOptions = highlightOptions(
            bringToFront = TRUE,
            opacity = 1.5,
            weight = 3
          ),
          label = ~ paste0(NAME, " - ", variable, ": ", Percent, "%")
        ) %>%
        addLegend(
          "topleft",
          pal = fempal,
          values = ~ Percent,
          title = "Female HOH",
          labFormat = labelFormat(suffix = "%"),
          opacity = 1
        )
    }
    
    
    else if(var_fam() == "Percent of Black Grandparents who are Guardians"){
      
      grand <- read_rds("data/grand.rds")
      colnames(grand)[4] <- "Percent"
      colnames(grand)[3] <- "Grandparent Guardian"
      grandpal <- colorNumeric(palette = "viridis", domain = grand$Percent, reverse = TRUE)
      
      grand_map <- grand %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ grandpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", "Grandparent Guardian: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = grandpal,
                  values = ~ Percent,
                  title = "Grandparent Guardian",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_fam() == "Percent of Married Black Population 15 years and over"){
      married <- read_rds("data/married.rds")
      colnames(married)[4] <- "Percent"
      marriedpal <- colorNumeric(palette = "viridis", domain = married$Percent, reverse = TRUE)
      
      married_map <- married %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ marriedpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", "Married: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = marriedpal,
                  values = ~ Percent,
                  title = "Married",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
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
  
  # Religion
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
    pal2 <- colorNumeric(viridis_pal()(11), plot_data$value)
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
  
  # Food Banks
  output$foodBanksLeaflet <- renderLeaflet({
    foodBankLoc <- read.csv("./data/foodBank/FoodBanks.csv")
    countyOutlines <- read_sf(dsn = "./data/countyOutlines/countyOutlines.shp")
    labs <- paste0(foodBankLoc$name, "<br></br><a href='", foodBankLoc$url,"'>View</a>")
    pal <- colorFactor(palette = 'viridis', foodBankLoc$county)
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
    gg.table <- gg.table %>% arrange(Locality)
    
    foodBank.ggplot <- ggplot(gg.table, aes(x = Locality, y = `Food Banks`)) + ggtitle("# of Food Banks in Locality", ) + 
      geom_col() + theme(legend.position="none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 15)) + 
      geom_text(
        aes(label = `Food Banks`), 
        position = position_dodge(width = 1),
        vjust = -1.5, color = 'black') +
      scale_fill_viridis_d() + 
      ylim(0, 12)
      
    
    ggplotly(foodBank.ggplot)
  })
  
  
  # Low Access to Food -----------------------------------------------------
  var_LowAcc <- reactive({
    input$LowAccIn
  })

  output$povertyRateMap <- renderLeaflet({
    lowAccDF <- read_sf("./data/foodAtlas/masterData.shp") %>% filter(year == 2019)
    countyOutlines <- read_sf(dsn = "./data/countyOutlines/countyOutlines.shp")
    
    pvrtyRate <- round(lowAccDF$PvrtyRt, 2)
    county <- word(lowAccDF$County, 1, -2)
    pal1 <- colorNumeric(palette = "magma", 
                         domain = as.double(pvrtyRate), reverse = TRUE)

    foodAccessMap <- leaflet(lowAccDF, options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = ~pal1(as.double(pvrtyRate)), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                highlightOptions = highlightOptions(bringToFront = TRUE, sendToBack = TRUE, opacity = 1.5, weight = 3),
                label = paste0(county, ": ", pvrtyRate, "%")) %>%
    addPolylines(data = countyOutlines, color = "black", weight = 1.2, smoothFactor = .5,
                 fillOpacity = 0, fillColor = "transparent") %>%
    addLegend(position = "topright", pal = pal1, values = as.double(pvrtyRate), opacity = .9, title = "Poverty Rate (%)")
    
    foodAccessMap
  })
  
  output$lowAccessAF1 <- renderLeaflet({
    lowAccDF <- read_sf("./data/foodAtlas/masterData.shp") %>% filter(year == 2019)
    countyOutlines <- read_sf(dsn = "./data/countyOutlines/countyOutlines.shp")
    
    lowAccessAF1Mile <- round(as.numeric(lowAccDF$lblck1s), 2)
    county <- word(lowAccDF$County, 1, -2)
    pal1 <- colorNumeric(palette = "magma", 
                         domain = as.double(lowAccessAF1Mile), reverse = TRUE)
    
    foodAccessMap <- leaflet(lowAccDF, options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal1(as.double(lowAccessAF1Mile)), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                  highlightOptions = highlightOptions(bringToFront = TRUE, sendToBack = TRUE, opacity = 1.5, weight = 3),
                  label = paste0(county, ": ", lowAccessAF1Mile, "%")) %>%
      addPolylines(data = countyOutlines, color = "black", weight = 1.2, smoothFactor = .5,
                   fillOpacity = 0, fillColor = "transparent") %>%
      addLegend(position = "topright", pal = pal1, values = as.double(lowAccessAF1Mile), opacity = .9, title = "Low Market Access (%)")
    
    foodAccessMap
  })
  
  output$lowAccessAF <- renderLeaflet({
    lowAccDF <- read_sf("./data/foodAtlas/masterData.shp") %>% filter(year == 2019)
    countyOutlines <- read_sf(dsn = "./data/countyOutlines/countyOutlines.shp")
    
    lowAccessAFHalfMile <- round(as.numeric(lowAccDF$lblckhlfs), 2)
    county <- word(lowAccDF$County, 1, -2)
    pal1 <- colorNumeric(palette = "magma", 
                         domain = as.double(lowAccessAFHalfMile), reverse = TRUE)
    
    foodAccessMap <- leaflet(lowAccDF, options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal1(as.double(lowAccessAFHalfMile)), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                  highlightOptions = highlightOptions(bringToFront = TRUE, sendToBack = TRUE, opacity = 1.5, weight = 3),
                  label = paste0(county, ": ", lowAccessAFHalfMile, "%")) %>%
      addPolylines(data = countyOutlines, color = "black", weight = 1.2, smoothFactor = .5,
                   fillOpacity = 0, fillColor = "transparent") %>%
      addLegend(position = "topright", pal = pal1, values = as.double(lowAccessAFHalfMile), opacity = .9, title = "Low Market Access (%)")
    
    foodAccessMap
  })
  
  # Financial Literacy
  
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
      geom_boxplot(varwidth = TRUE, alpha=0.2) +
      theme(legend.position="none") +
      scale_x_discrete(labels=my_xlab, limits = positions)+
      #scale_x_discrete(limits = positions) + 
      labs(x = "Race") +
      labs(y = "Financial Literacy Score") +
      ylim(0, 100) +
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
      labs(fill = "Race")
    dont_know <- ggplotly(dont_know)
  })
  
  # Media and Entertainment graphs ------------------------------------------
  # Internet Coverage 2015 VS 2020
  var_coverage <- reactive({
    input$select_coverage
  })
  output$internet_coverage_maps <- renderLeaflet({
    if (var_coverage() == "2015") {
      # load zipcode geojson file
      zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
      # read broadbandnow data
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
      
      # join the data to the geojson
      joined_zips <- geo_join(zips, dat, by_sp = "name", by_df = "Zip", how = "inner")
      
      # load redlining data
      norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
      cols <- c("green", "blue", "yellow", "red")
      pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
    
    # create map details (2015)
    pal <- colorNumeric(viridis_pal()(6), joined_zips$AllProviderCount_2015)
    labs <- sprintf(
      "<strong>%s</strong><br/>Zip: %i<br/>Num. Providers: %i",
      joined_zips$County,
      joined_zips$Zip,
      joined_zips$AllProviderCount_2015
    ) %>%
      lapply(htmltools::HTML)
    
    ## 2015 map
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
    else if (var_coverage() == "2020") {
      # load zipcode geojson file
      zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
      # read broadbandnow data
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
      
      # join population characteristics data
      
      jim_dat <- read.csv("./data/jim_data.csv")
      new_dat <- jim_dat %>% select(zipcode, black.people, percent_of_population)
      joined <- inner_join(dat, new_dat, by = c("Zip" = "zipcode"))
      
      # join the data to the geojson
      joined_zips <- geo_join(zips, joined, by_sp = "name", by_df = "Zip", how = "inner")
      
      # load redlining data
      norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
      cols <- c("green", "blue", "yellow", "red")
      pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
      
      # map details
      pal <- colorNumeric(viridis_pal()(11), joined_zips$AllProviderCount_2020)
      labs <- sprintf(
        "<strong>%s</strong><br/>Zip: %i<br/>Num. Providers: %i<br/>Black Population: %i<br/>Percent Black: %.2f%%",
        joined_zips$County,
        joined_zips$Zip,
        joined_zips$AllProviderCount_2020,
        joined_zips$black.people,
        joined_zips$percent_of_population
      ) %>%
        lapply(htmltools::HTML)
      
      # create map
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
  
  #internet quality 2015 VS 2020
  var_quality <- reactive({
    input$select_quality
  })
  
  output$internet_quality_maps <- renderLeaflet({
    if (var_quality() == "2015"){
      # load zipcode geojson file
      zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
      # read broadbandnow data
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
      
      # join the data to the geojson
      joined_zips <- geo_join(zips, dat, by_sp = "name", by_df = "Zip", how = "inner")
      
      # load redlining data
      norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
      cols <- c("green", "blue", "yellow", "red")
      pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
      
      # create map details 2015
      pal <- colorNumeric(viridis_pal()(11), joined_zips$All100_3.1)
      labs <- sprintf(
        "<strong>%s</strong><br/>Zip: %i<br/>Num. Providers: %i",
        joined_zips$County,
        joined_zips$Zip,
        joined_zips$All100_3.1
      ) %>%
        lapply(htmltools::HTML)
      
      ## 2015 map
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
          label = ~ labs) %>%
        addPolygons(
          data = norfolk,
          color = ~ pal2(norfolk$holc_grade),
          opacity = 0.8,
          stroke = F
        ) %>%
        addLegend(
          pal = pal,
          values = ~ joined_zips$All100_3.1,
          position = "topleft",
          title = "Num. Providers",
          opacity = 0.75
        ) 
    }
    else if (var_quality() == "2020") {
      # load zipcode geojson file
      zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
      # read broadbandnow data
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
      
      # join to population characteristic data
      jim_dat <- read.csv("./data/jim_data.csv")
      new_dat <- jim_dat %>% select(zipcode, black.people, percent_of_population)
      joined <- inner_join(dat, new_dat, by = c("Zip" = "zipcode"))
      
      # join the data to the geojson
      joined_zips <- geo_join(zips, joined, by_sp = "name", by_df = "Zip", how = "inner")
      
      # load redlining data
      norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
      cols <- c("green", "blue", "yellow", "red")
      pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
      
      # create map
      pal <- colorNumeric(viridis_pal()(11), joined_zips$All100_3)
      labs <- sprintf(
        "<strong>%s</strong><br/>Zip: %i<br/>Num. Providers: %i<br/>Black Population: %i<br/>Percent Black: %.2f%%",
        joined_zips$County,
        joined_zips$Zip,
        joined_zips$All100_3,
        joined_zips$black.people,
        joined_zips$percent_of_population
      ) %>%
        lapply(htmltools::HTML)
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
          label = ~ labs) %>%
        addPolygons(
          data = norfolk,
          color = ~ pal2(norfolk$holc_grade),
          opacity = 0.8,
          stroke = F
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
  
  output$anch_plots <- renderPlot({
    anch <- read.csv("./data/news_anchors.csv", stringsAsFactors = TRUE)
    ethn <- table(anch$Ethnicity)
    # if (var_news() == "Ethnicity"){}
    #pie chart displaying the counts of Ethnicity breakdown
    plt1 <- anch %>%
      group_by(Ethnicity) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = "", y = count, fill = Ethnicity)) +
      geom_bar(stat = "identity", width = 1) + 
      coord_polar("y", start = 0) +
      theme_void() +
      geom_text(aes(
        y = count,
        label = paste(round(count / sum(count) * 100, 1), "%"),
        x = 1.3
      ), position = position_stack(vjust = 0.5)) + theme(legend.text=element_text(size=15))
    
    
    #bar chart of the gender and ethnicity distributions
    plt2 <- anch %>%
      group_by(Ethnicity, Gender) %>%
      summarise(count = n()) %>%
      ggplot(aes(Ethnicity, count, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") + theme_fivethirtyeight() +
      theme(axis.title = element_text()) + ylab('Count') +
      theme(axis.title.y = element_text(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text( size = 15),
            legend.text=element_text(size=20)) +
      xlab('Ethnicity')
    #bar chart of the Roles vs. Ethnicity
    plt3 <- anch %>%
      group_by(Ethnicity, Role) %>%
      summarise(count = n()) %>%
      ggplot(aes(Ethnicity, count, fill = Role)) +
      geom_bar(stat = "identity", position = "dodge") + theme_fivethirtyeight() +
      theme(axis.title = element_text()) + ylab('Count') +
      theme(axis.title.y = element_text(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text( size = 15),
            legend.text=element_text(size=20)) +
      xlab('Ethnicity')
    
    plt4 <- anch %>%
      group_by(Ethnicity, Channel) %>%
      summarise(count = n()) %>%
      ggplot(aes(Ethnicity, count, fill = Channel)) +
      geom_bar(stat = "identity", position = "dodge") + theme_fivethirtyeight() +
      theme(axis.title = element_text()) + ylab('Count') +
      theme(axis.title.y = element_text(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text( size = 15),
            legend.text=element_text(size=20)) +
      xlab('Ethnicity')
    anch_plots <- ggarrange(plt1, plt2, plt3, plt4, ncol = 1, nrow = 4)
    # ggarrange(plt2, plt3, plt4,  common.legend = TRUE)
    anch_plots
  }, height=1000)
  
  # Radio stations
  output$radio <- renderLeaflet({
    geo_data <- readRDS('./data/geo_data.rds')
    geo_data <- st_transform(geo_data)
    
    geo_data$loc_name <- str_to_lower(geo_data$loc_name)
    geo_data$loc_name <- word(geo_data$loc_name, 1) 
    
    
    radio_df <- read.csv('./data/radio_stations.csv')  %>%
      filter(State == 'VA') %>%
      mutate(loc_name = str_to_lower(Community.of.License),
             loc_name = word(loc_name, 1),
             city_name = Community.of.License)%>%
      group_by(loc_name) %>%
      summarise(formats = paste0(Format, collapse = ', '), count = n(),
                city_name = city_name) %>%
      distinct(loc_name, .keep_all = TRUE) %>%
      mutate(formats1 = ifelse(loc_name == 'norfolk', 
                               'religious, urban/variety contemporary, Rhythmic/urban Adult Contemporary' , formats))%>%
      mutate(formats2 =ifelse(loc_name == 'norfolk', 
                              'new/public affairs/npr, classical, country, active rock, christian chr', '')) %>%
      mutate(formats1 = ifelse(loc_name == 'virginia', 
                               'modern adult contemporary, big band/ nostalgia/old time radio,' , formats1))%>%
      mutate(formats2 =ifelse(loc_name == 'virginia', 
                              'christian contemporaty/preaching, album adult alternative, Christian contemporary hit radio', ''))
    
    
    merged_data2 <- merge(radio_df, geo_data, by = 'loc_name')
    # merged_data2$geometry <- st_transform(merged_data2$geometry)
    
    labs <- lapply(seq(nrow(merged_data2)), function(i) {
      paste0( '<p>', merged_data2[i, "city_name"], '<p></p>', 'Count of Stations: ', 
              merged_data2[i, "count"], '</p>', 'Type/Formats: ', '</p>',
              merged_data2[i, "formats1"],'</p>', 
              merged_data2[i, "formats2"], '</p>' ) 
    })
    
    # pal2 <- colorNumeric(palette = "viridis", domain = 1:2, reverse = TRUE)
    # show_col(viridis_pal()(11))
    pal2 <- viridis_pal()(11)
    pal2 <- colorNumeric(viridis_pal()(11), merged_data2$count)
    
    radio <- merged_data2 %>%
      leaflet( options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(data = merged_data2$geometry, color= viridis_pal()(11)[merged_data2$count],
                  weight = 0.5,
                  fillOpacity = 0.7, smoothFactor = 0,
                  highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                  label = lapply(labs, htmltools::HTML)) %>%
      addLegend(pal = pal2, values = ~merged_data2$count, title = 'Number of Stations', opacity = .75)
    radio
  })
  
  
  ## Policy and Justice -------------------------------------------------------
  # Traffic stops
  # race count
  output$trafficRace <- renderPlot({
    data <- read.csv("./data/hampton_trafficstop.csv")
    
    # Analyze
    print(is.data.frame(data))
    print(ncol(data))
    print(nrow(data))
    
    # Race Count ------------
    trafficRace <- ggplot(data,
                          aes(x = RACE)) +  theme_fivethirtyeight() +
      geom_bar(fill = "cornflowerblue") +
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.5,
                colour = "black") + 
      labs(x = "Race",
           y = "Count",
           title = "Demographics of Traffic Stops") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
            axis.text.y = element_text(hjust = 1, size = 15)) + 
      theme(plot.title = element_text(color = "black", size = 24)) + ylim(0, 10500) +
      theme( # remove the vertical grid lines
        panel.grid.major.x = element_blank()
      )
    
    trafficRace
  })
  
  # Race and Jurisdiction
  
  output$jurisdiction <- renderPlot({
    data <- read.csv("./data/hampton_trafficstop.csv")
    
    # Analyze
    jurisdiction <- ggplot(data, aes(x = JURISDICTION, fill = RACE)) +
      geom_bar(position = "dodge") + theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
            axis.text.y = element_text(hjust = 1, size = 15)) +
      theme(plot.title = element_text(color = "black", size = 24, face = "bold"), 
            legend.text=element_text(size=15)) + ylim(0, 2000)
    
    jurisdiction
  })
  var_stop <- reactive(
    input$select_stop
  )
  output$jurisdiction2 <- renderPlot({
    data <- read.csv("./data/hampton_trafficstop.csv")
    
    jurisdiction2 <- data %>% filter(JURISDICTION == var_stop()) %>% 
      ggplot(aes(y=RACE,x = AGE, color = RACE)) + theme_fivethirtyeight() +
      geom_boxplot(size = 1, 
                   outlier.shape =1,
                   outlier.color = "black",
                   outlier.size = 3) +
      geom_jitter(alpha = 0.25, width = .2) +
      labs(title = "Traffic Stops Data") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(hjust = 1, size = 15)) +
      theme(plot.title = element_text(color = "black", size = 24, face = "bold")) +
      theme(legend.position = "none") + 
      coord_flip()
    jurisdiction2
  })
  
  # City council demographics Race
  output$cityd <- renderPlot({
    police_df <- read.csv('./data/hampton_roads_police_chief.csv')
    politicans_df <- read.csv('./data/hampton_roads_politicans.csv')
    police_sum <- data.frame('black' = sum(police_df$Black, na.rm = TRUE),
                             'white' = sum(police_df$White, na.rm = TRUE))
    cityd <- politicans_df %>%
      pivot_longer(4:7, names_to = 'demographic') %>%
      mutate(demographic = str_sub(demographic,  14)) %>%
      select(-c(Mayor, Vice.Mayor)) %>%
      filter(demographic == 'White' | demographic == 'Black') %>%
      ggplot(aes(x=City, y = value, fill = demographic)) +
      geom_bar(stat = 'identity', position = 'dodge') + 
      theme_fivethirtyeight() +
      theme(axis.title.y = element_text(),
            axis.title = element_text(),
            axis.text.x = element_text(angle = 45, size = 13, hjust =1), 
            axis.text.y = element_text(hjust = 1, size = 15),
            legend.text=element_text(size=20), legend.title=element_blank()) +
      ggtitle('City Council Demographics by Race 2021') +ylab('count')
    cityd
  })
  
  # City council demographics Gender
  output$cityd2 <- renderPlot({
    police_df <- read.csv('./data/hampton_roads_police_chief.csv')
    politicans_df <- read.csv('./data/hampton_roads_politicans.csv')
    cityd2 <- politicans_df %>%
      pivot_longer(4:7, names_to = 'demographic') %>%
      mutate(demographic = str_sub(demographic,  14)) %>%
      select(-c(Mayor, Vice.Mayor)) %>%
      filter(demographic == 'Female' | demographic == 'Male') %>%
      ggplot(aes(x=City, y = value, fill = demographic)) +
      geom_bar(stat = 'identity', position = 'dodge') + 
      theme_fivethirtyeight() +
      theme(axis.title.y = element_text(),
            axis.title = element_text(),
            axis.text.x = element_text(angle = 45, size = 13, hjust = 1),
            axis.text.y = element_text( size = 15),
            legend.text=element_text(size=20), legend.title=element_blank()) +
      ggtitle('City Council Demographics by Gender 2021') +ylab('count')
    cityd2
  })
  
  
  # Jail plots
  var_jailChoice <- reactive({
    input$select_jailChoice
  })
  
  output$jail <- renderPlot({
    if (var_jailChoice()=="Virginia"){
      va_incarceration_trends <- read.csv('./data/va_incarceration_trends.csv')
      jail <- va_incarceration_trends %>%
        group_by(year) %>%
        select(black_jail_pop, black_pop_15to64, latinx_jail_pop, latinx_pop_15to64,
               native_jail_pop, native_pop_15to64, white_jail_pop, white_pop_15to64,
               aapi_jail_pop, aapi_pop_15to64) %>%
        summarise(black =
                    sum(black_jail_pop, na.rm = TRUE)/sum(black_pop_15to64, na.rm =TRUE)*100000,
                  white =
                    sum(white_jail_pop, na.rm = TRUE)/sum(white_pop_15to64, na.rm =TRUE)*100000,
                  asian.pi =
                    sum(aapi_jail_pop, na.rm = TRUE)/sum(aapi_pop_15to64, na.rm =TRUE)*100000,
                  latinx =
                    sum(latinx_jail_pop, na.rm = TRUE)/sum(latinx_pop_15to64, na.rm =TRUE)*100000,
                  native.amer =
                    sum(native_jail_pop, na.rm = TRUE)/sum(native_pop_15to64, na.rm =TRUE)*100000) %>%
        pivot_longer(cols = 2:6, names_to = 'race.ethnicity', values_to = 'jail.rate.per.100k') %>%
        ungroup()%>%
        arrange(desc(year),desc(jail.rate.per.100k))%>%
        mutate(label = ifelse(year==2018, race.ethnicity, ''))%>%
        ggplot() + geom_line(aes(year, jail.rate.per.100k, col = race.ethnicity), size = 2.5) + 
        # geom_label_repel(aes(year, jail.rate.per.100k, label = label),
        #               nudge_x = 1, nudge_y = 5,
        #               na.rm = TRUE) +
        theme_fivethirtyeight() + scale_colour_viridis_d() + 
        ggtitle('Jail Rate per 100,000 ages 15-64 for VA state') +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
        theme(legend.text=element_text(size=20), legend.title=element_blank(), 
              axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))  +  
        xlim(1990, 2020) + ylim(0, 2000)   
    }
    else if (var_jailChoice() == "Hampton Roads") {
      va_hampton_roads_incarceration_trends <- read.csv('./data/va_hampton_roads_incarceration_trends.csv')
      jail <- va_hampton_roads_incarceration_trends %>%
        group_by(year) %>%
        select(black_jail_pop, black_pop_15to64, latinx_jail_pop, latinx_pop_15to64,
               native_jail_pop, native_pop_15to64, white_jail_pop, white_pop_15to64,
               aapi_jail_pop, aapi_pop_15to64) %>%
        summarise(black = 
                    sum(black_jail_pop, na.rm = TRUE)/sum(black_pop_15to64, na.rm =TRUE)*100000,
                  white = 
                    sum(white_jail_pop, na.rm = TRUE)/sum(white_pop_15to64, na.rm =TRUE)*100000,
                  asian.pi = 
                    sum(aapi_jail_pop, na.rm = TRUE)/sum(aapi_pop_15to64, na.rm =TRUE)*100000,
                  latinx = 
                    sum(latinx_jail_pop, na.rm = TRUE)/sum(latinx_pop_15to64, na.rm =TRUE)*100000,
                  native.amer = 
                    sum(native_jail_pop, na.rm = TRUE)/sum(native_pop_15to64, na.rm =TRUE)*100000) %>%
        pivot_longer(cols = 2:6, names_to = 'race.ethnicity', values_to = 'jail.rate.per.100k') %>%
        ungroup()%>%
        arrange(desc(year),desc(jail.rate.per.100k))%>%
        mutate(label = ifelse(year==2018, race.ethnicity, ''))%>%
        ggplot() + geom_line(aes(year, jail.rate.per.100k, col = race.ethnicity), size = 2.5) + 
        # geom_label_repel(aes(year, jail.rate.per.100k, label = label),
        #               nudge_x = 1, nudge_y = 5,
        #               na.rm = TRUE) +
        theme_fivethirtyeight() + scale_colour_viridis_d() + 
        ggtitle('Jail Rate per 100,000 ages 15-64 in Hampton Roads') +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
        theme(legend.text=element_text(size=20), legend.title=element_blank(), 
              axis.text.x = element_text(size=15), axis.text.y = element_text(size=15)) +    
        xlim(1990, 2020) + ylim(0, 2000)   
      
    }
    jail
  })
  
  # Pie plots
  var_pieYear <- reactive(
    input$select_pieYear
  )
  
  output$pie_plots1 <- renderHighchart({
    # va_incarceration_trends <- read.csv('./data/va_incarceration_trends.csv')
    va_hampton_roads_incarceration_trends <- read.csv('./data/va_hampton_roads_incarceration_trends.csv')
    # percentage of jail pop. and state pop.
    col_plot <- va_hampton_roads_incarceration_trends %>%
      # group_by(year) %>%
      filter(year == var_pieYear()) %>% # filter by year here
      select(year, black_jail_pop, black_pop_15to64, latinx_jail_pop, latinx_pop_15to64,
             native_jail_pop, native_pop_15to64, white_jail_pop, white_pop_15to64,
             aapi_jail_pop, aapi_pop_15to64, other_race_jail_pop, total_jail_pop, total_pop_15to64) %>%
      summarise(black_jail = round(sum(black_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                black_pop = round(sum(black_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                native_jail = round(sum(native_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                native_pop = round(sum(native_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                aapi_jail = round(sum(aapi_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                aapi_pop = round(sum(aapi_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                white_jail = round(sum(white_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                white_pop = round(sum(white_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                latinx_jail = round(sum(latinx_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                latinx_pop = round(sum(latinx_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                other_jail = round(sum(other_race_jail_pop, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0)) %>%
      pivot_longer(cols = 1:10,values_to = 'prop', names_to = 'race.ethnicity') %>%
      mutate(class = case_when(race.ethnicity == 'black_jail' | race.ethnicity == 'black_pop' ~ 'black',
                               race.ethnicity == 'white_jail' | race.ethnicity == 'white_pop' ~ 'white',
                               race.ethnicity == 'aapi_jail' | race.ethnicity == 'aapi_pop' ~ 'asian.pi',
                               race.ethnicity == 'latinx_jail' | race.ethnicity == 'latinx_pop' ~ 'latinx',
                               race.ethnicity == 'native_jail' | race.ethnicity == 'native_pop' ~ 'native',
                               race.ethnicity == 'other_jail' ~ 'other'
      )) %>%
      mutate(type = case_when(str_sub(race.ethnicity,-4)== 'jail'~ 'Jail Population',
                              str_sub(race.ethnicity,-3) == 'pop'~ 'Total Population' )) %>%
      arrange(desc(type)) %>%
      ungroup()%>%
      group_by(race.ethnicity)%>%
      mutate(label.pos = prop/2)
    
    pie_plots1 <- col_plot %>%
      filter(type == 'Jail Population')%>%
      mutate(race.ethnicity = str_sub(race.ethnicity, 1, -6)) %>%
      hchart(
        "pie", hcaes(x = race.ethnicity, y = prop),
        name = paste0("Percentage of Jail Population ", var_pieYear())
      )
    
    pie_plots1
  })
  
  output$pie_plots2 <- renderHighchart({
    # va_incarceration_trends <- read.csv('./data/va_incarceration_trends.csv')
    va_hampton_roads_incarceration_trends <- read.csv('./data/va_hampton_roads_incarceration_trends.csv')
    # percentage of jail pop. and state pop.
    col_plot <- va_hampton_roads_incarceration_trends %>%
      # group_by(year) %>%
      filter(year == var_pieYear()) %>% # filter by year here
      select(year, black_jail_pop, black_pop_15to64, latinx_jail_pop, latinx_pop_15to64,
             native_jail_pop, native_pop_15to64, white_jail_pop, white_pop_15to64,
             aapi_jail_pop, aapi_pop_15to64, other_race_jail_pop, total_jail_pop, total_pop_15to64) %>%
      summarise(black_jail = round(sum(black_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                black_pop = round(sum(black_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                native_jail = round(sum(native_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                native_pop = round(sum(native_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                aapi_jail = round(sum(aapi_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                aapi_pop = round(sum(aapi_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                white_jail = round(sum(white_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                white_pop = round(sum(white_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                latinx_jail = round(sum(latinx_jail_pop, na.rm = TRUE)/sum( total_jail_pop, na.rm =TRUE)*100,0),
                latinx_pop = round(sum(latinx_pop_15to64, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0),
                other_jail = round(sum(other_race_jail_pop, na.rm =TRUE)/sum(total_pop_15to64, na.rm=TRUE)*100, 0)) %>%
      pivot_longer(cols = 1:10,values_to = 'prop', names_to = 'race.ethnicity') %>%
      mutate(class = case_when(race.ethnicity == 'black_jail' | race.ethnicity == 'black_pop' ~ 'black',
                               race.ethnicity == 'white_jail' | race.ethnicity == 'white_pop' ~ 'white',
                               race.ethnicity == 'aapi_jail' | race.ethnicity == 'aapi_pop' ~ 'asian.pi',
                               race.ethnicity == 'latinx_jail' | race.ethnicity == 'latinx_pop' ~ 'latinx',
                               race.ethnicity == 'native_jail' | race.ethnicity == 'native_pop' ~ 'native',
                               race.ethnicity == 'other_jail' ~ 'other'
      )) %>%
      mutate(type = case_when(str_sub(race.ethnicity,-4)== 'jail'~ 'Jail Population',
                              str_sub(race.ethnicity,-3) == 'pop'~ 'Total Population' )) %>%
      arrange(desc(type)) %>%
      ungroup()%>%
      group_by(race.ethnicity)%>%
      mutate(label.pos = prop/2)
    
    pie_plots2 <- col_plot %>%
      filter(type == 'Total Population')%>%
      mutate(race.ethnicity = str_sub(race.ethnicity, 1, -5)) %>%
      hchart(
        "pie", hcaes(x = race.ethnicity, y = prop),
        name = paste0("Percentage of Total Population ", var_pieYear())
      )
    pie_plots2
  })
  
  # Prison
  var_prisonYear <- reactive({
    input$select_prisonYear
  })
  
  output$prison <- renderLeaflet({
    # incarceration_trends <- read.csv('./data/incarceration_trends.csv')
    # incarceration_trends_jurisdicition <- read.csv('./data/incarceration_trends_jail_jurisdiction.csv')
    # va_incarceration_trends <- read.csv('./data/va_incarceration_trends.csv')
    va_hampton_roads_incarceration_trends <- read.csv('./data/va_hampton_roads_incarceration_trends.csv')
    geo_data <- readRDS('./data/geo_data.rds')
    geo_data <- st_transform(geo_data)
    
    geo_data$loc_name <- str_to_lower(geo_data$loc_name)
    geo_data$loc_name <- word(geo_data$loc_name, 1) 
    
    
    va_prison_plot_df <- va_hampton_roads_incarceration_trends %>%
      filter(year == var_prisonYear()) %>%
      select(city_name, total_prison_adm_rate) %>%
      drop_na(total_prison_adm_rate) %>%
      arrange(desc(total_prison_adm_rate)) %>%
      mutate(loc_name = str_to_lower(city_name),
             loc_name = word(loc_name, 1)
      ) %>%
      mutate(total_prison_adm_rate = ifelse(total_prison_adm_rate == 0, NA, total_prison_adm_rate))
    
    
    merged_data2 <- merge(va_prison_plot_df, geo_data, by = 'loc_name')
    # merged_data2$geometry <- st_transform(merged_data2$geometry)
    
    labs <- lapply(seq(nrow(merged_data2)), function(i) {
      paste0( '<p>', merged_data2[i, "city_name"], '<p></p>', paste(var_prisonYear(), ' Prison Admission Rate Per 100k: '), 
              merged_data2[i, "total_prison_adm_rate"], '</p>') 
    })
    
    # pal2 <- colorNumeric(palette = "viridis", domain = 1:2, reverse = TRUE)
    # show_col(viridis_pal()(11))
    # pal2 <- viridis_pal()(11)
    pal2 <- colorNumeric(viridis_pal()(10), merged_data2$total_prison_adm_rate)
    
    prison <- merged_data2 %>%
      leaflet( options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(data = merged_data2$geometry, color= pal2(merged_data2$total_prison_adm_rate),
                  weight = 0.5,
                  fillOpacity = 0.7, smoothFactor = 0,
                  highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                  label = lapply(labs, htmltools::HTML)) %>%
      addLegend(pal = pal2, values = ~merged_data2$total_prison_adm_rate, title = 'Admission Rate Per 100k', opacity = .75)  
    prison
  })
  
  # Gentrification (map home values)
  output$map_homevalues <- renderLeaflet({
    # load zipcode geojson file
    zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
    # load home value data
    data_jim_final <- read.csv("./data/data_jim_final.csv")
    data_jim_final <- na.omit(data_jim_final)
    data_jim_final <- data_jim_final %>%  
      mutate_at("homevalue", funs(as.character(.))) %>%
      mutate_at("homevalue", funs(gsub(",", "", .))) %>%
      mutate_at("homevalue", funs(as.numeric(.)))
    data_jim_final <-  data_jim_final%>%mutate(popup_info=paste("Zipcode",zipcode,"<br/>","Homevalue",homevalue,"<br/>","% White Population",Percentofpopulation,"<br/>","% Black Population",percent_of_population))
    
    joined_jim <- geo_join(zips, data_jim_final, by_sp = "name", by_df = "zipcode", how = "inner")
    pal <- colorNumeric("viridis", joined_jim$homevalue)
    
    # load redlining data
    norfolk <- geojson_read("./data/VANorfolk1940.geojson", what = "sp")
    cols <- c("green", "blue", "yellow", "red")
    
    pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
    
    map_homevalues <- joined_jim %>% 
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(
        data = norfolk,
        color = ~ pal2(norfolk$holc_grade),
        opacity = 0.9,
        stroke = F
      ) %>%
      addPolygons(fillColor = ~ pal(joined_jim$homevalue),
                  color = ~ pal(joined_jim$homevalue),
                  fillOpacity = 0.5,
                  weight = 0.9,
                  smoothFactor = 0.2,
                  stroke = TRUE,
                  popup = ~popup_info
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
    geo_data <- readRDS('./data/geo_data.rds')
    geo_data <- st_transform(geo_data)
    
    geo_data$loc_name <- str_to_lower(geo_data$loc_name)
    geo_data$loc_name <- word(geo_data$loc_name, 1) 
    
    
    radio_df <- read.csv('./data/radio_stations.csv')  %>%
      filter(State == 'VA') %>%
      mutate(loc_name = str_to_lower(Community.of.License),
             loc_name = word(loc_name, 1),
             city_name = Community.of.License)%>%
      group_by(loc_name) %>%
      summarise(formats = paste0(Format, collapse = ', '), count = n(),
                city_name = city_name) %>%
      distinct(loc_name, .keep_all = TRUE) %>%
      mutate(formats1 = ifelse(loc_name == 'norfolk', 
                               'religious, urban/variety contemporary, Rhythmic/urban Adult Contemporary' , formats))%>%
      mutate(formats2 =ifelse(loc_name == 'norfolk', 
                              'new/public affairs/npr, classical, country, active rock, christian chr', '')) %>%
      mutate(formats1 = ifelse(loc_name == 'virginia', 
                               'modern adult contemporary, big band/ nostalgia/old time radio,' , formats1))%>%
      mutate(formats2 =ifelse(loc_name == 'virginia', 
                              'christian contemporaty/preaching, album adult alternative, Christian contemporary hit radio', ''))
    
    
    merged_data2 <- merge(radio_df, geo_data, by = 'loc_name')
    # merged_data2$geometry <- st_transform(merged_data2$geometry)
    
    labs <- lapply(seq(nrow(merged_data2)), function(i) {
      paste0( '<p>', merged_data2[i, "city_name"], '<p></p>', 'Count of Stations: ', 
              merged_data2[i, "count"], '</p>', 'Type/Formats: ', '</p>',
              merged_data2[i, "formats1"],'</p>', 
              merged_data2[i, "formats2"], '</p>' ) 
    })
    
    # pal2 <- colorNumeric(palette = "viridis", domain = 1:2, reverse = TRUE)
    # show_col(viridis_pal()(11))
    pal2 <- viridis_pal()(11)
    pal2 <- colorNumeric(viridis_pal()(11), merged_data2$count)
    
    radio <- merged_data2 %>%
      leaflet( options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(data = merged_data2$geometry, color= viridis_pal()(11)[merged_data2$count],
                  weight = 0.5,
                  fillOpacity = 0.7, smoothFactor = 0,
                  highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                  label = lapply(labs, htmltools::HTML)) %>%
      addLegend(pal = pal2, values = ~merged_data2$count, title = 'Number of Stations', opacity = .75)
    radio
  })
  
  # Household Wellbeing -----------------------------------------------------
  var_well <- reactive({
    input$select_wellbeing
  })
  
  output$wellbeing_maps <- renderLeaflet({
    if(var_well() == "Percent of Black Households Receiving Foodstamps/SNAP Benefits"){
      
      foodstmp <- read_rds("data/foodstmp.rds")
      colnames(foodstmp)[4] <- "Percent"
      foodpal <- colorNumeric(palette = "viridis", domain = foodstmp$Percent, reverse = TRUE)
      
      foodstmp_map <- foodstmp %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ foodpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", variable, ": ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = foodpal,
                  values = ~ Percent,
                  title = "Food Stamps",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_well() == "Percent of Black County Migration"){
      
      mobile <- read_rds("data/mobile.rds")
      colnames(mobile)[4] <- "Percent"
      colnames(mobile)[3] <- "Intra-County Migration"
      mobpal <- colorNumeric(palette = "viridis", domain = mobile$Percent, reverse = TRUE)
      
      mobile_map <- mobile %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ mobpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, "", "Intra-County Migration: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = mobpal,
                  values = ~ Percent,
                  title = "County Migration",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
      
    }
    
    else if(var_well() == "Percent of Black Population that uses car/truck/van to get to work"){
      
      priv_trans <- read_rds("data/priv_trans.rds")
      priv_trans <- priv_trans %>%
        na.omit(priv_trans)
      colnames(priv_trans)[4] <- "Percent"
      colnames(priv_trans)[3] <- "Private Transport"
      priv_transpal <-
        colorNumeric(
          palette = "viridis",
          domain = priv_trans$Percent,
          reverse = TRUE
        )
      
      priv_trans_map <- priv_trans %>%
        leaflet(options = leafletOptions(
          minZoom = 5,
          maxZoom = 15,
          drag = FALSE
        )) %>%
        addProviderTiles("CartoDB.PositronNoLabels") %>%
        addPolygons(
          color = ~ priv_transpal(Percent),
          weight = 0.5,
          fillOpacity = 0.7,
          smoothFactor = 0,
          highlightOptions = highlightOptions(
            bringToFront = TRUE,
            opacity = 1.5,
            weight = 3
          ),
          label = ~ paste0(NAME, " - ", "Private Transport: ", Percent, "%")
        ) %>%
        addLegend(
          "topleft",
          pal = priv_transpal,
          values = ~ Percent,
          title = "Private Transportation",
          labFormat = labelFormat(suffix = "%"),
          opacity = 1
        )
    }
    
    else if (var_well() == "Percent of Black Population that uses public transportation to get to work") {
      pub_trans <- read_rds("data/pub_trans.rds")
      pub_trans <- pub_trans %>%
        na.omit(pub_trans)
      colnames(pub_trans)[4] <- "Percent"
      colnames(pub_trans)[3] <- "Public Transport"
      pub_transpal <-
        colorNumeric(
          palette = "viridis",
          domain = pub_trans$Percent,
          reverse = TRUE
        )
      
      pub_trans_map <- pub_trans %>%
        leaflet(options = leafletOptions(
          minZoom = 5,
          maxZoom = 15,
          drag = FALSE
        )) %>%
        addProviderTiles("CartoDB.PositronNoLabels") %>%
        addPolygons(
          color = ~ pub_transpal(Percent),
          weight = 0.5,
          fillOpacity = 0.7,
          smoothFactor = 0,
          highlightOptions = highlightOptions(
            bringToFront = TRUE,
            opacity = 1.5,
            weight = 3
          ),
          label = ~ paste0(NAME, " - ", "Public Transport: ", Percent, "%")
        ) %>%
        addLegend(
          "topleft",
          pal = pub_transpal,
          values = ~ Percent,
          title = "Public Transportation",
          labFormat = labelFormat(suffix = "%"),
          opacity = 1
        )
      
    }
    
    else if (var_well() == "Percent of Black Households with a computer with broadband internet") {
      compin <- read_rds("data/compin.rds")
      colnames(compin)[4] <- "Percent"
      colnames(compin)[3] <- "Computer and Internet"
      compinpal <-
        colorNumeric(
          palette = "viridis",
          domain = compin$Percent,
          reverse = TRUE
        )
      
      compin_map <- compin %>%
        leaflet(options = leafletOptions(
          minZoom = 5,
          maxZoom = 15,
          drag = FALSE
        )) %>%
        addProviderTiles("CartoDB.PositronNoLabels") %>%
        addPolygons(
          color = ~ compinpal(Percent),
          weight = 0.5,
          fillOpacity = 0.7,
          smoothFactor = 0,
          highlightOptions = highlightOptions(
            bringToFront = TRUE,
            opacity = 1.5,
            weight = 3
          ),
          label = ~ paste0(NAME, " - ", "Computer and Internet: ", Percent, "%")
        ) %>%
        addLegend(
          "topleft",
          pal = compinpal,
          values = ~ Percent,
          title = "Computer with Internet Access",
          labFormat = labelFormat(suffix = "%"),
          opacity = 1
        )
    }
    
    else if (var_well() == "Percent of Black Households without a computer") {
      nocomp <- read_rds("data/nocomp.rds")
      colnames(nocomp)[4] <- "Percent"
      colnames(nocomp)[3] <- "No Computer"
      nocomppal <-
        colorNumeric(
          palette = "viridis",
          domain = nocomp$Percent,
          reverse = TRUE
        )
      
      nocomp_map <- nocomp %>%
        leaflet(options = leafletOptions(
          minZoom = 5,
          maxZoom = 15,
          drag = FALSE
        )) %>%
        addProviderTiles("CartoDB.PositronNoLabels") %>%
        addPolygons(
          color = ~ nocomppal(Percent),
          weight = 0.5,
          fillOpacity = 0.7,
          smoothFactor = 0,
          highlightOptions = highlightOptions(
            bringToFront = TRUE,
            opacity = 1.5,
            weight = 3
          ),
          label = ~ paste0(NAME, " - ", "No Computer: ", Percent, "%")
        ) %>%
        addLegend(
          "topleft",
          pal = nocomppal,
          values = ~ Percent,
          title = "No Computer Access",
          labFormat = labelFormat(suffix = "%"),
          opacity = 1
        )
    }
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
  
  #Media article analysis for Media/Entertainment pillar --------
  article_dat <- read.csv("data/articledata.csv")
  #one specific value had year written in different format
  article_dat$year[167] <- 2022
  #removing empty data
  article_dat <- article_dat[-(104:110),]
  
  article_dat$hqlocation <- as.factor(article_dat$hqlocation)
  article_dat$source <- as.factor(article_dat$source)
  
  #Summing the words by year
  pos_sum <- aggregate(pos_len ~ year, data = article_dat, FUN = sum)
  neg_sum <- aggregate(neg_len ~ year, data = article_dat, FUN = sum)
  div_sum <- aggregate(div_len ~ year, data = article_dat, FUN = sum)
  tot_sum <- aggregate(text_len ~ year, data = article_dat, FUN = sum)
  
  #Merging above data into one data frame
  sub_dat <- merge(pos_sum, neg_sum, all=TRUE, no.dups = TRUE)
  sub_dat <- merge(sub_dat, div_sum, all=TRUE, no.dups = TRUE)
  sub_dat <- merge(sub_dat, tot_sum, all=TRUE, no.dups = TRUE)
  
  #Columns for the ratio of each sentiment compared to the total number of words collected
  sub_dat$pos_ratio <- sub_dat$pos_len/sub_dat$text_len
  sub_dat$neg_ratio <- sub_dat$neg_len/sub_dat$text_len
  sub_dat$div_ratio <- sub_dat$div_len/sub_dat$text_len
  sub_dat$year <- as.character(sub_dat$year)
  
  
  #Adding ratios to individual article data
  article_dat$pos_ratio <- article_dat$pos_len/article_dat$text_len
  article_dat$neg_ratio <- article_dat$neg_len/article_dat$text_len
  article_dat$div_ratio <- article_dat$div_len/article_dat$text_len
  

  sentiment_year <- reactive({
    input$select_sent_year
  })
  
  output$sentiment_by_year <- renderPlotly({
     sentiment_sub_dat <- select(filter(sub_dat, year == sentiment_year()), pos_ratio, neg_ratio, div_ratio)
     dat_for_bar_graph <- data.frame(Ratios = c(sentiment_sub_dat[1,1], sentiment_sub_dat[1,2], sentiment_sub_dat[1,3]),
                                     Sentiments = c("Positive", "Negative", "Diversity"))
     ggplot(data = dat_for_bar_graph, aes(x = Sentiments, y = Ratios)) + geom_bar(stat = "identity", fill = "firebrick4") +
       ylab("Proportion to Total Words")
  })

  output$headquarters_graph <- renderPlot(
    ggplot(article_dat, mapping = aes(hqlocation)) +
      geom_bar(fill = "firebrick4") + xlab("City") +
      ylab("Number of Articles") +
      ggtitle("Headquarters Locations of Media Companies") +
      theme(axis.text.x = element_text(color = "black", size = 12, angle = 30),
            axis.text.y = element_text(color = "black", size = 12), axis.title = element_text(size = 17),
            plot.title = element_text(size = 19))
  )

  library(reshape2); 
  ratios <- melt(article_dat[, c("pos_ratio", "neg_ratio", "div_ratio")], id.vars = "div_ratio")
  
  output$div_by_pos_and_neg <- renderPlot(
    ggplot(ratios,  aes(x = div_ratio, y = value, color = variable)) +  
      geom_point() + 
      scale_colour_manual(values = c("black", "red")) +
      ylab("Positive and Negative Ratios") + xlab("Diversity Word Ratio") + 
      ggtitle("Positive and Negative Sentiment Ratios versus Diversity Ratio") +
      theme(axis.text.x = element_text(color = "black", size = 10),
            axis.text.y = element_text(color = "black", size = 10), axis.title = element_text(size = 15),
            plot.title = element_text(size = 18)))
  
  url <- a("Link to Word Bags", href="https://tinyurl.com/4ym9njb7")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
}

return(server)
shinyApp(ui = ui, server = server)