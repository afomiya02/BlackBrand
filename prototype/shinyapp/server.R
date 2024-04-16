library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(tidyverse)
library(ggrepel)
library(ggExtra)
library(thematic)

source("sodem.r")
source("education.r")
source("economics.r")

server <- function(input, output, session) {
    ### --- SOCIODEMOGRAPHICS ---
    
    # black population value box
    output$sodem_vb1 <- renderUI({
        black_pop <- round(sum(sodem_data$black_or_african_american) / 
                               sum(sodem_data$total_population), 2) * 100
        value_box(
            title = p("Black Population:", style = "font-size: 20px"),
            value = shiny::p(paste0(black_pop, "%"), style = "font-size: 36px"),
            showcase = bs_icon("pie-chart"),
            theme = "info"
        )
    })
    
    # median age value box
    output$sodem_vb2 <- renderUI({
        median_age <- round(mean(sodem_data$median_age_years), 1)
        value_box(
            title = shiny::p("Median Age:", style = "font-size: 20px"),
            value = shiny::p(median_age, "years", style = "font-size: 36px"),
            showcase = bs_icon("cake"),
            theme = "primary"
        )
    })
    
    # total population value box
    output$sodem_vb3 <- renderUI({
        value_box(
            title = shiny::p("Total Population:", style = "font-size: 20px"),
            value = shiny::p(sum(sodem_data$total_population), style = "font-size: 36px"),
            showcase = bs_icon("person"),
            theme = "primary"
        )
    })
    
    ## LEAFLET OUTPUT
    output$pop_choropleth <- renderLeaflet({
        # Yellow-Orange-Red color palette used for all choropleth maps
        pal <- colorBin("YlOrRd", heatmap_data$pct_black)
        sodem_choropleth <- leaflet() %>%
            addPolygons(
                data = heatmap_data,
                fillColor = pal(heatmap_data$pct_black),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                popup = paste(
                    "<h1>", heatmap_data$loc,"</h1>",
                    "<b>Median Age (years):</b>", heatmap_data$median_age_years,
                    "<br><b>Black Population (%):</b>", heatmap_data$pct_black,
                    "<br><b>Total Population:</b>", heatmap_data$total_population
                )
            ) %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = heatmap_data$pct_black,
                title = "Black Population (%)"
            ) %>%
            addTiles()
        
        sodem_choropleth
    })
    
    output$age_choropleth <- renderLeaflet({
        pal <- colorBin("YlOrRd", heatmap_data$median_age_years)
        sodem_choropleth <- leaflet() %>%
            addPolygons(
                data = heatmap_data,
                fillColor = pal(heatmap_data$median_age_years),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                popup = paste(
                    "<h1>", heatmap_data$loc,"</h1>",
                    "<b>Median Age (years):</b>", heatmap_data$median_age_years,
                    "<br><b>Black Population (%):</b>", heatmap_data$pct_black,
                    "<br><b>Total Population:</b>", heatmap_data$total_population
                )
            ) %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = heatmap_data$median_age_years,
                title = "Median Age (years)"
            ) %>%
            addTiles()
        
        sodem_choropleth
    })
    
    ### --- EDUCATION ---
    
    # black student % in loc
    output$edu_vb1 <- renderUI({
        prop <- local_student_data() %>% filter(races == "Black") %>% select(total_count) /
            sum(local_student_data()$total_count)
        
        value_box(
            title = p("Percentage of Black students in", input$edu_loc),
            value = p(paste0(round(prop * 100, 2), "%"), style = "font-size: 36px"),
            theme = "info",
            showcase = bs_icon("pie-chart"),
            showcase_layout = "top right"
        )
    })
    
    # average test scores for black population
    output$edu_vb2 <- renderUI({
        pct <- st_data %>% 
            filter(subgroup == "Black" & division_name == input$edu_loc) %>%
            pull(`2022-2023_pass_rate`) %>%
            mean()
        
        value_box(
            title = shiny::p("Average Black test scores in", input$edu_loc),
            value = shiny::p(paste0(pct, "%"), style = "font-size: 36px"),
            theme = "danger",
            showcase = bs_icon("card-checklist"),
            showcase_layout = "top right"
        )
    })
    
    output$edu_vb3 <- renderUI({
        value_box(
            title = p("Total student population in", input$edu_loc),
            value = p(sum(local_student_data()$total_count), style = "font-size: 36px"),
            theme = "primary",
            showcase = bs_icon("person"),
            showcase_layout = "top right"
        )
    })
    
    local_student_data <- reactive({
        df <- student_count_data %>%
            filter(division_name %in% input$edu_loc) %>%
            pivot_longer(cols = -division_name, names_to = "races", values_to = "total_count") %>%
            arrange(desc(total_count)) %>%
            filter(!grepl("total_count", races)) %>%
            mutate(total_count = as.numeric(total_count)) %>%
            mutate_if(is.character, str_replace_all, "_", " ") %>%
            mutate_if(is.character, str_to_title)
        df
    })
    
    local_educator_data <- reactive({
        df <- educator_count_data %>% 
            filter(division_name %in% input$edu_loc) %>%
            pivot_longer(cols = -division_name, names_to = "races", values_to = "total_count") %>%
            arrange(desc(total_count)) %>%
            filter(!grepl("total_count", races)) %>%
            mutate(total_count = as.numeric(total_count)) %>%
            mutate_if(is.character, str_replace_all, "_", " ") %>%
            mutate_if(is.character, str_to_title)
        df
    })
    
    output$student_race_plot <- renderPlot({
        hsize <- 3
        p <- ggplot(local_student_data(), aes(x = hsize, y = total_count, fill = races)) +
            geom_col(color = "black") +
            geom_text(aes(x = 2.1, label = total_count), position = position_stack(vjust = 0.5)) +
            coord_polar(theta = "y") +
            xlim(c(0.2, hsize + 0.5)) +
            theme(panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank()) +
            ggtitle(paste("Racial Distribution of Students in", input$edu_loc))
        p
    })
    
    output$educator_race_plot <- renderPlot({
        hsize <- 3
        p <- ggplot(local_educator_data(), aes(x = hsize, y = total_count, fill = races)) +
            geom_col(color = "black") +
            geom_text(aes(x = 2.1, label = total_count), position = position_stack(vjust = 0.5)) +
            coord_polar(theta = "y") +
            xlim(c(0.2, hsize + 0.5)) +
            theme(panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank()) +
            ggtitle(paste("Racial Distribution of Educators in", input$edu_loc))
        p
    })
    
    # reactive that gets all necessary info for radar plot
    st_radar <- reactive({
        df <- st_data %>%
            dplyr::filter(division_name %in% input$edu_loc) %>%
            dplyr::filter(subgroup %in% input$edu_races) %>%
            select(c(subject, subgroup, one_of(input$st_year))) %>%
            # pivot dataset such that subjects are columns and
            # subjects are row names
            pivot_wider(names_from = subject, values_from = one_of(input$st_year)) %>%
            column_to_rownames(., "subgroup")
        df
    })
    
    # create radio plot with subetted data
    output$radar_plot <- renderPlotly({
        req(input$edu_races)
        
        fig <- plot_ly(
            data = st_radar(),
            type = "scatterpolar",
            mode = "lines+markers"
        )
        
        # TODO: CREATE PROPER FIXED COLOR PALETTE THAT MATCHES
        # CHECKBOX INPUTS WITH ITS RESPECTIVE COLOR
        
        # iterate thru input$edu_races to get line for each race in plot
        # have to wrap r and theta such that the values iterate like a circle
        # (e.g [Black, White, Asian, Black])
        for (i in 1:length(input$edu_races)) {
            fig <- fig %>%
                add_trace(r = as.numeric(unlist(c(st_radar()[input$edu_races[i], ], 
                                                  st_radar()[input$edu_races[i], 1]))), 
                          theta = unlist(c(colnames(st_radar()), colnames(st_radar())[1])),
                          name = paste(input$edu_races[i], "Students"))
        }
        
        fig %>% layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))))
    })
    
    # reactive that gets all necessary info for lollipop plot
    st_lollipop <- reactive({
        req(input$edu_races)
        df <- st_data %>%
            dplyr::filter(division_name %in% input$edu_loc) %>%
            dplyr::filter(subgroup %in% input$edu_races) %>%
            group_by(subgroup) %>% dplyr::summarise(
                across(ends_with("pass_rate"), mean)
            ) %>%
            pivot_longer(!subgroup, names_to = "year", values_to = "pass_rate") %>%
            dplyr::mutate(year = str_remove(year, "_pass_rate")) %>%
            pivot_wider(names_from = subgroup, values_from = pass_rate)
        df
    })
    
    # create lollipop plot
    output$lollipop_plot <- renderPlot({
        req(input$edu_races)
        
        # TODO create separate legend w/ colors
        # current theme (journal) colors
        pal <- c("Black" = "#AAAAAA",
                 "White" = "#EB6864",
                 "Asian" = "#336699",
                 "Hispanic" = "#F57A00")
        
        # apply(df, 1, min or max) gets both mins and maxes from each row
        p <- ggplot(st_lollipop(), aes(x = year)) + 
            geom_segment(aes(x = year, xend = year, y = apply(st_lollipop() %>% select(-year), 1, min), 
                             yend = apply(st_lollipop() %>% select(-year), 1, max)), color = "grey", linewidth = 1.5) +
            theme_minimal() + labs(x = "School Year", y = "Testing Pass Rate (%)")
        
        for (i in 1:length(input$edu_races)) {
            p <- p +
                # sym() turns a string into a variable, i.e "Asian" becomes Asian and therefore
                # becomes readable and fetches Asian from data frame
                # !! unpacks these variables from input$edu_races[i]
                geom_point(aes(x = year, y = !!sym(input$edu_races[i])), colour = pal[input$edu_races[i]], size = 5)
        }
        
        p + ylim(0, 100)
    })
    
    # create choropleth data & map
    cohort_grad_data <- reactive({
        ch <- cohort_pass_rates %>%
            filter(cohort_year %in% input$cohort_year)
        # TODO CREATE NEW GEO_DATA FOR EDUCATIONAL DATA COMBINING WILLIAMSBURG AND JC COUNTY
        gd <- geo_data %>%
            # remove williamsburg
            filter(loc_name != "williamsburg") %>%
            # rename james city county to williamsburg-james city county
            mutate(loc_name = case_when(str_detect(loc_name, "^j") ~ "williamsburg-james_city_county",
                                        TRUE ~ str_replace(loc_name, "_city", ""))) %>%
            mutate(across(loc_name, str_replace_all, "_", " ")) %>%
            mutate(across(loc_name, str_to_title))
        
        df <- merge(gd, ch, by.x = "loc_name", by.y = "division_name") %>%
            filter(race == input$grad_race)
        df
    })
    
    output$cohort_choropleth_map <- renderLeaflet({
        data <- cohort_grad_data()
        # brainrot solution, please implement this in a more efficient way
        # next semester's capstone team :3
        
        # minimum graduation rate across ENTIRE dataset is 72%
        pal <- colorBin("YlOrRd", bins = 7, 70:100)
        map <- leaflet(data) %>%
            addPolygons(
                color = "black",
                fillColor = ~pal(data$graduation_rate),
                fillOpacity = 0.75,
                weight = 1,
                popup = paste(
                    "<h1>", data$loc_name,"</h1>",
                    "<b>", input$grad_race, "Student Population:</b>", 2 * 10
                )
            ) %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = data$graduation_rate,
                title = "Graduation Rate (%)"
            ) %>%
            addTiles()
        map
    })
    
    ### --- ECONOMICS ---
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
    
    # Home Ownership Map 
    # Reactive expression to get the selected value from the HomeOwnSlider input
    var_hmown <- reactive({
        input$HomeOwnSlider
    })
    
    # Render Leaflet map
    output$homeownership_map <- renderLeaflet({
        # Read homeownership data
        data <- read_homeownership_data()
        b_hm_19 <- data$b_hm_19
        tot_hm_19 <- data$tot_hm_19
        all_hm_data <- data$all_hm_data
        
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
        pal <- colorNumeric(palette = "viridis",
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
    
    #Labor market
    # Employment By Sector
    # Reactive expression to get the selected year from the input dropdown
    var_sectorEmployment <- reactive({
        input$SectorEmploymentYearDrop
    })
    
    # Render the plotly plot
    output$sector_plot <- renderPlotly({
        # Call the read_and_plot_sectors function with the selected year from the reactive expression
        read_and_plot_sectors(var_sectorEmployment())
    })
    
    #Poverty
    #poverty rates
    # Define a reactive expression to capture the selected year from the dropdown
    var_poverty <- reactive({
        input$PovertyYearDrop
    })
    
    
    # Render plot based on selected year
    output$pov_plot <- renderPlot({
        # Check the selected year and load corresponding data
        if (var_poverty() %in% c("2019", "2018", "2017", "2016", "2015")) {
            year <- var_poverty()
            va_file <- paste0("data/economics/poverty/va_poverty", year, ".csv")
            hamp_file <- paste0("data/economics/poverty/hamp_poverty", year, ".csv")
            pov_data <- read_process_csv(va_file, hamp_file, c(123, 136))
        } else if (var_poverty() %in% c("2014", "2013", "2012")) {
            year <- var_poverty()
            va_file <- paste0("data/economics/poverty/va_poverty", year, ".csv")
            hamp_file <- paste0("data/economics/poverty/hamp_poverty", year, ".csv")
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
    
    # poverty rates across localities
    # Define a reactive expression to capture the selected year from the dropdown
    var_povertyCount <- reactive({
        input$PovertyCountYearDrop
    })
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
    
    #Health
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
    
}

return(server)
