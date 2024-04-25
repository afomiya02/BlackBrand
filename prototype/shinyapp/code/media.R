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

# make sure to set working directory in either ui.R or server.R
# if not this will not load!
source("code/sodem.r") # load color palettes

InternetCoverage <- function(input,output,session){
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
            dat <- read.csv("./data/media/broadbandnow_data.csv")
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
            norfolk <- geojson_read("./data/media/VANorfolk1940.geojson", what = "sp")
            cols <- c("green", "blue", "yellow", "red")
            pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
            
            # Create map details for 2015
            pal <- colorBin(continuous_pal, joined_zips$AllProviderCount_2015)
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
                    color = "black",
                    weight = 1,
                    fillOpacity = 0.75,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    highlightOptions = highlightOptions(
                        bringToFront = TRUE, 
                        color = "white",
                        weight = 2),
                    label = ~ labs) %>%
                # i'm not sure how important this is and it kind of gets in the way so.
                # talk to sponsors?
                # addPolygons(
                #     data = norfolk,
                #     color = ~ pal2(norfolk$holc_grade),
                #     opacity = 0.9,
                #     stroke = F
                # ) %>%
                addLegend(
                    pal = pal,
                    values = ~ joined_zips$AllProviderCount_2015,
                    position = "bottomright",
                    title = "Num. Providers",
                    labFormat = labelFormat(digits = 0)
                ) 
        }
        # Check if the selected coverage year is 2020
        else if (var_coverage() == "2020") {
            # Load zipcode geojson file  # load zipcode geojson file
            zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
            
            # Read broadbandnow data
            dat <- read.csv("./data/media/broadbandnow_data.csv")
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
            
            jim_dat <- read.csv("./data/media/jim_data.csv")
            new_dat <- jim_dat %>% select(zipcode, black.people, percent_of_population)
            joined <- inner_join(dat, new_dat, by = c("Zip" = "zipcode"))
            
            # Join the data to the geojson# join the data to the geojson
            joined_zips <- geo_join(zips, joined, by_sp = "name", by_df = "Zip", how = "inner")
            
            # Load redlining data
            norfolk <- geojson_read("./data/media/VANorfolk1940.geojson", what = "sp")
            cols <- c("green", "blue", "yellow", "red")
            pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
            
            # Create map details for 2020
            pal <- colorBin(continuous_pal, joined_zips$AllProviderCount_2020)
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
                    fillColor = ~ pal(joined_zips$AllProviderCount_2015),
                    color = "black",
                    weight = 1,
                    fillOpacity = 0.75,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    highlightOptions = highlightOptions(
                        bringToFront = TRUE, 
                        color = "white",
                        weight = 2),
                    label = ~ labs) %>%
                # i'm not sure how important this is and it kind of gets in the way so.
                # talk to sponsors?
                # addPolygons(
                #     data = norfolk,
                #     color = ~ pal2(norfolk$holc_grade),
                #     opacity = 0.8,
                #     stroke = F
                # ) %>%
                addLegend(
                    pal = pal,
                    values = ~ joined_zips$AllProviderCount_2020,
                    position = "topleft",
                    title = "Num. Providers",
                    opacity = 0.75
                ) 
        }
    })
}
InternetQualityMap <- function(input,output,session) {
    #Internet quality
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
            dat <- read.csv("./data/media/broadbandnow_data.csv")
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
            norfolk <- geojson_read("./data/media/VANorfolk1940.geojson", what = "sp")
            cols <- c("green", "blue", "yellow", "red")
            pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
            
            # Create map details for 2015
            pal <- colorBin(continuous_pal, joined_zips$All100_3.1)
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
                    color = "black",
                    weight = 1,
                    fillOpacity = 0.75,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    highlightOptions = highlightOptions(
                        bringToFront = TRUE, 
                        color = "white",
                        weight = 2),
                    label = ~ labs
                ) %>%
                # addPolygons(
                #     data = norfolk,
                #     color = ~ pal2(norfolk$holc_grade),
                #     opacity = 0.8,
                #     stroke = FALSE
                # ) %>%
                addLegend(
                    pal = pal,
                    values = ~ joined_zips$All100_3.1,
                    position = "bottomright",
                    title = "Num. Providers",
                    opacity = 0.75
                ) 
        } 
        else if (quality_year == "2020") {
            # Load zipcode geojson file
            zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
            
            # Read broadbandnow data
            dat <- read.csv("./data/media/broadbandnow_data.csv")
            dat <- filter(dat, State == "Virginia")
            dat <- dat[dat$County %in% c(
                "Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City", 
                "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", 
                "Suffolk", "Virginia Beach", "Williamsburg", "York"
            ),]
            dat <- na.omit(dat)
            dat <- select(dat, -State)
            
            # Join to population characteristic data
            jim_dat <- read.csv("./data/media/jim_data.csv")
            new_dat <- jim_dat %>% select(zipcode, black.people, percent_of_population)
            joined <- inner_join(dat, new_dat, by = c("Zip" = "zipcode"))
            
            # Join the data to the geojson
            joined_zips <- geo_join(zips, joined, by_sp = "name", by_df = "Zip", how = "inner")
            
            # Load redlining data
            norfolk <- geojson_read("./data/media/VANorfolk1940.geojson", what = "sp")
            cols <- c("green", "blue", "yellow", "red")
            pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
            
            # Create map details for 2020
            pal <- colorBin(continuous_pal, joined_zips$All100_3)
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
                    fillColor = ~ pal(joined_zips$All100_3.1),
                    color = "black",
                    weight = 1,
                    fillOpacity = 0.75,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    highlightOptions = highlightOptions(
                        bringToFront = TRUE, 
                        color = "white",
                        weight = 2),
                    label = ~ labs
                ) %>%
                # addPolygons(
                #     data = norfolk,
                #     color = ~ pal2(norfolk$holc_grade),
                #     opacity = 0.8,
                #     stroke = FALSE
                # ) %>%
                addLegend(
                    pal = pal,
                    values = ~ joined_zips$All100_3,
                    position = "bottomright",
                    title = "Num. Providers"
                ) 
        }
    }) 
}

NewsAnchors <- function(input,output,session){
    # Define the renderPlot function
    output$anch_plots <- renderPlot({
        anch <- read.csv("./data/media/news_anchors.csv", stringsAsFactors = TRUE)
        
        # Plot 1: Pi Chart
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
            ), position = position_stack(vjust = 0.5)) + 
            theme(legend.text=element_text(size=15))
        
        # Plot 2: Bar Chart of Gender and Ethnicity Distributions
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
        
        # Plot 3: Bar Chart of Roles vs. Ethnicity
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
        
        # Plot 4: Bar Chart of Channel vs. Ethnicity
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
        
        # Arrange the plots in a 2x2 grid
        anch_plots <- ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2)
        
        # Return the arranged plots
        anch_plots
    }, height = 1200) 
}

RadioStations <- function(input,output, session){
    # Radio stations
    output$radio <- renderLeaflet({
        # Read geo data
        geo_data <- readRDS('./data/media/geo_data.rds')
        geo_data <- st_transform(geo_data)
        
        # Preprocess location names
        geo_data$loc_name <- str_to_lower(geo_data$loc_name)
        geo_data$loc_name <- word(geo_data$loc_name, 1) 
        
        # Read radio station data and preprocess
        radio_df <- read.csv('./data/media/radio_stations.csv')  %>%
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
        merged_data <- merge(geo_data, radio_df, by = 'loc_name')
        
        # Create labels for markers
        # TODO this is broken as FUCK
        labs <- lapply(seq(nrow(merged_data)), function(i) {
            paste0('<p>', merged_data[i, "city_name"], '</p>',
                   '<p>Count of Stations: ', merged_data[i, "count"], '</p>',
                   '<p>Type/Formats: </p>',
                   '<p>', merged_data[i, "formats1"], '</p>',
                   '<p>', merged_data[i, "formats2"], '</p>')
        })
        
        # Define color palette for markers
        pal <- colorBin(continuous_pal, merged_data$count)
        
        # Create Leaflet map
        View(merged_data)
        radio_map <- leaflet(merged_data) %>% 
            addTiles() %>%
            addPolygons(
                fillColor = ~pal(count),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                smoothFactor = 0.5,
                opacity = 1.0,
                highlightOptions = highlightOptions(
                    bringToFront = TRUE, 
                    color = "white",
                    weight = 2),
                # what the fuck is this
                # label = lapply(labs, htmltools::HTML)
                label = city_name # placeholder cause labs is UGLY
            ) %>%
            addLegend(
                pal = pal, 
                values = ~count, 
                title = 'Number of Stations', 
                position = "bottomright"
            )
        
        radio_map
    })
}

#Graphs for Headquarter Locations, Ratio of Sentiment and Diversity Vs Pos and Neg
# Function to load data from CSV file
load_data <- function() {
    article_dat <- read.csv("data/media/articledata.csv")
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

headquarter_sentiment_deversity <- function(input,output,session){
    # Output for headquarters plot
    output$headquarters_graph <- renderPlot({
        headquarters_data <- load_data()
        create_headquarters_plot(headquarters_data)
    })
    
    # Reactive expression for selecting sentiment year
    sentiment_year <- reactive({
        input$select_sent_year
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
}
