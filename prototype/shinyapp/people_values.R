library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(leaflet.minicharts)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(gridExtra)
library(stringr)
library(shinyjs)
library(plotly)
library(ggrepel)
library(shinydashboard)
library(mapdata)
library(plotrix)
library(scatterpie)
library(leafpop)
library(ggpubr)
library(viridis)
library(highcharter)
#library(rCharts)
library(terra)
library(geojsonio)
library(tigris)
library(dplyr)
library(lattice)
library(ggplot2)
library(forcats)
library(plotly)
library(formattable)
library(hrbrthemes)
library(bslib)
library(reshape2) 

#Family Dynamic ---------------------------------------------------------
generateMap <- function(data, title, labelSuffix = "%") {
  pal <- colorNumeric(palette = "viridis", domain = data$Percent, reverse = TRUE)
  data %>% 
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                label = ~ paste0(NAME, " - ", title, ": ", Percent, labelSuffix)) %>%
    addLegend("topleft",pal = pal,values = ~ Percent,title = title,
              labFormat = labelFormat(suffix = labelSuffix),opacity = 1)
}

family_dynamic <- function(input,output,session){
  # use reactive for selected family
  var_fam <- reactive({
    input$select_family
  })

  # render the map based on the selected family
  output$family_maps <- renderLeaflet({
    req(var_fam()) # ensure that var_fam is not NULL
    # define a list to map selection values to file names
    fileMap <- list(
      "Percent of Black Children under 18 in Female Head of Household" = "people_values/fml.rds",
      "Percent of Black Grandparents who are Guardians" = "people_values/grand.rds",
      "Percent of Married Black Population 15 years and over" = "people_values/married.rds"
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
}
# End Family Dynamic ---------------------------------------------------------


# Religion ---------------------------------------------------------
religion <- function(input,output,session){
  var_religion <- reactive({
    input$select_rel
  })
  
  output$religion <- renderLeaflet({
    geo_data <- readRDS('./data/geo_data.rds')
    geo_data <- st_transform(geo_data)
    
    geo_data$loc_name <- str_to_lower(geo_data$loc_name)
    geo_data$loc_name <- word(geo_data$loc_name, 1) 
    
    religion_data <- read.csv('./data/people_values/capstone_religious_adh.csv') 
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
}
# End of Religion section ---------------------------------------------------------

# Financial Literacy -----------------------------------------------------
financial_lit <- function(input,output,session){
  output$financial_literacy <- renderPlot({
    # load financial literacy data 
    df <- read.csv("./data/people_values/public2020.csv")
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
  
  #Credit score
  output$credit_scores <- renderPlotly({
    # load financial literacy data 
    df <- read.csv("./data/people_values/public2020.csv")
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
    df <- read.csv("./data/people_values/public2020.csv")
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
}
# end of Financial Literacy section -----------------------------------------------------


foodbanks <- function(input,output,session){
  # Food Banks ---------------------------------------------------------
  output$foodBanksLeaflet <- renderLeaflet({
    foodBankLoc <- read.csv("./data/people_values/foodBank/FoodBanks.csv")
    countyOutlines <- read_sf(dsn = "./data/people_values/countyOutlines/countyOutlines.shp")
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
    foodBankLoc <- read.csv("./data/people_values/foodBank/FoodBanks.csv")
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
      geom_col() + 
      theme(legend.position="none", 
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 15)) + 
      geom_text(aes(label = `Food Banks`), 
                vjust = 0.5, # Adjust this value as needed to prevent overlap
                color = 'black') + ylim(0, 12)
    ggplotly(foodBank.ggplot)
  })
  # End of Food Banks section ---------------------------------------------------------
}

food_insecurity <- function(input,output,session){
  # Food Insecurity -----------------------------------------------------
  # function to generate a leaflet map
  generateFoodAccessMap <- function(data, valueColumn, titleSuffix) {
    value <- round(as.numeric(data[[valueColumn]]), 2)
    county <- word(data$County, 1, -2)
    pal <- colorNumeric(palette = "magma", domain = as.double(value), reverse = TRUE)
    
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
  lowAccDF <- read_sf("./data/people_values/foodAtlas/masterData.shp") %>% filter(year == 2019)
  countyOutlines <- read_sf(dsn = "./data/people_values/countyOutlines/countyOutlines.shp")
  
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
}

