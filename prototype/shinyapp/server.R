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
library(htmlwidgets)
library(highcharter)
library(sf)
library(geojsonio)
library(tigris)
library(ggpubr)
library(reshape2)

source("code/sodem.R")
source("code/education.R")
source("code/economics.R")
source("code/media.R")
source("code/politics_justice.R")
source("code/people_values.R")
source("code/feedback.R")

server <- function(input, output, session) {
    ### --- SOCIODEMOGRAPHICS -------------------------------------------------------------
    
    # total population value box
    output$sodem_vb1 <- renderUI({
        total_pop <- sum(sodem_data$total_population)
        value_box(
            title = shiny::p("Total population:"),
            value = shiny::p(prettyNum(total_pop, big.mark = ","), style = "font-size: 36px"),
            showcase = bs_icon("person"),
            theme = "primary"
        )
    })
    
    # median age value box
    output$sodem_vb2 <- renderUI({
        median_age <- round(mean(sodem_data$median_age_years), 1)
        value_box(
            title = shiny::p("Median age:"),
            value = shiny::p(median_age, "years", style = "font-size: 36px"),
            showcase = bs_icon("cake"),
            theme = "primary"
        )
    })
    
    # black population value box
    output$sodem_vb3 <- renderUI({
        black_pop <- round(sum(sodem_data$black_or_african_american) / 
                               sum(sodem_data$total_population), 2) * 100
        value_box(
            title = p("Percent of Black individuals:"),
            value = shiny::p(paste0(black_pop, "%"), style = "font-size: 36px"),
            showcase = bs_icon("pie-chart"),
            theme = "info"
        )
    })
    
    ## LEAFLET OUTPUT
    output$pop_choropleth <- renderLeaflet({
        # Yellow-Orange-Red color palette used for all choropleth maps
        pal <- colorBin(continuous_pal, 
                        min(heatmap_data$pct_black):max(heatmap_data$pct_black))
        sodem_choropleth <- leaflet() %>%
            addPolygons(
                data = heatmap_data,
                fillColor = pal(heatmap_data$pct_black),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                smoothFactor = 0.5,
                opacity = 1.0,
                highlightOptions = highlightOptions(
                    bringToFront = TRUE, 
                    color = "white",
                    weight = 2),
                label = heatmap_data$loc_name,
                popup = paste(
                    "<h3>", heatmap_data$loc,"</h3>",
                    "<b>Total Population:</b>", heatmap_data$total_population,
                    "<br><b>Median Age (years):</b>", heatmap_data$median_age_years,
                    "<br><b>Black Population (%):</b>", heatmap_data$pct_black
                )
            ) %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = heatmap_data$pct_black,
                title = "Black Population",
                labFormat = labelFormat(suffix = "%")
            ) %>%
            addTiles()
        
        sodem_choropleth
    })
    
    output$age_choropleth <- renderLeaflet({
        pal <- colorBin(continuous_pal, 
                        min(heatmap_data$median_age_years):max(heatmap_data$median_age_years))
        
        cmap <- leaflet(heatmap_data) %>%
            addPolygons(
                fillColor = pal(heatmap_data$median_age_years),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                smoothFactor = 0.5,
                opacity = 1.0,
                highlightOptions = highlightOptions(
                    bringToFront = TRUE, 
                    color = "white",
                    weight = 2),
                label = heatmap_data$loc_name,
                popup = paste(
                    "<h3>", heatmap_data$loc_name,"</h3>",
                    "<b>Total Population:</b>", heatmap_data$total_population,
                    "<br><b>Median Age (years):</b>", heatmap_data$median_age_years,
                    "<br><b>Black Population (%):</b>", heatmap_data$pct_black
                )
            ) %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = heatmap_data$median_age_years,
                title = "Median Age (years)"
            ) %>%
            addTiles()
        
        cmap
    })
    
    ### --- EDUCATION ----------------------------------------------------------
    ## subset standardized testing radar data
    
    # black student % in loc
    output$edu_vb1 <- renderUI({
        black <- local_education_data() %>% 
            filter(races == "Black") %>% 
            select(total_student_count)
        total <- local_education_data() %>%
            filter(races == "Total Counts") %>%
            select(total_student_count)
        
        prop <- black / total
        
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
    
    # total student population in loc
    output$edu_vb3 <- renderUI({
        sum <- sum(local_education_data()$total_student_count)
        value_box(
            title = p("Total student population in", input$edu_loc),
            value = p(prettyNum(sum, big.mark = ","), style = "font-size: 36px"),
            theme = "primary",
            showcase = bs_icon("person"),
            showcase_layout = "top right"
        )
    })
    
    # gonna try to merge student and educator data together
    local_education_data <- reactive({
        std <- student_count_data %>%
            filter(division_name %in% input$edu_ratio_loc) %>%
            pivot_longer(cols = -division_name, names_to = "races", values_to = "total_student_count") %>%
            arrange(desc(total_student_count)) %>%
            filter(!grepl("total_student_count", races)) %>%
            mutate(total_student_count = as.numeric(total_student_count)) %>%
            # max(total_student_count) will always be the total count
            mutate(student_pct = round(total_student_count / max(total_student_count), 2) * 100) %>%
            mutate_if(is.character, str_replace_all, "_", " ") %>%
            mutate_if(is.character, str_to_title)
        
        edu <- educator_count_data %>% 
            filter(division_name %in% input$edu_ratio_loc) %>%
            pivot_longer(cols = -division_name, names_to = "races", values_to = "total_educator_count") %>%
            arrange(desc(total_educator_count)) %>%
            filter(!grepl("total_educator_count", races)) %>%
            mutate(total_educator_count = as.numeric(total_educator_count)) %>%
            mutate(educator_pct = round(total_educator_count / max(total_educator_count), 2) * 100) %>%
            mutate_if(is.character, str_replace_all, "_", " ") %>%
            mutate_if(is.character, str_to_title)
        
        df <- inner_join(std, edu, by = c("division_name", "races")) ; df
    })
    
    # TODO create percentiles instead of numbers for donut plots
    # creates donut graph for student demographics in loc
    output$student_race_plot <- renderPlot({
        # size of blank space for donut
        hsize <- 3 
        data <- local_education_data() %>% filter(races != "Total Counts")
        
        p <- ggplot(data, aes(x = hsize, y = total_student_count, fill = races)) +
            geom_col(color = "black") +
            geom_text(aes(x = 2.1, label = paste0(student_pct, "%")), 
                      position = position_stack(vjust = 0.5)) +
            coord_polar(theta = "y") +
            xlim(c(0.2, hsize + 0.5)) +
            theme(panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank()) +
            scale_fill_manual(name = "Demographic", values = discrete_pal) +
            ggtitle(paste("Racial Distribution of Students in", input$edu_ratio_loc))
        p
    })
    
    # creates donut graph for educator demographics in loc
    output$educator_race_plot <- renderPlot({
        hsize <- 3
        data <- local_education_data() %>% filter(races != "Total Counts")
        p <- ggplot(data, aes(x = hsize, y = total_educator_count, fill = races)) +
            geom_col(color = "black") +
            geom_text(aes(x = 2.1, label = paste0(educator_pct, "%")), 
                      position = position_stack(vjust = 0.5)) +
            coord_polar(theta = "y") +
            xlim(c(0.2, hsize + 0.5)) +
            theme(panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank()) +
            scale_fill_manual(name = "Demographic", values = discrete_pal) +
            ggtitle(paste("Racial Distribution of Educators in", input$edu_ratio_loc))
        p
    })
    
    # black teacher-student ratio
    output$edu_ratio1 <- renderUI({
        data <- local_education_data() %>% filter(races == "Black")
        pct <- data$total_student_count / data$total_educator_count
        value_box(
            title = shiny::p("Black teacher-student ratio in", input$edu_ratio_loc),
            value = shiny::p(paste0(ceiling(pct), ":1"), style = "font-size: 36px"),
            theme = "primary",
            showcase = bs_icon("hand-index"),
            showcase_layout = "top right"
        )
    })
    
    # white teacher-student ratio
    output$edu_ratio2 <- renderUI({
        data <- local_education_data() %>% filter(races == "White")
        pct <- data$total_student_count / data$total_educator_count
        value_box(
            title = shiny::p("White teacher-student ratio in", input$edu_ratio_loc),
            value = shiny::p(paste0(ceiling(pct), ":1"), style = "font-size: 36px"),
            theme = "info",
            showcase = bs_icon("backpack"),
            showcase_layout = "top right"
        )
    })
    
    # total teacher-student ratio
    output$edu_ratio3 <- renderUI({
        data <- local_education_data() %>% filter(races == "Total Counts")
        pct <- data$total_student_count / data$total_educator_count
        value_box(
            title = shiny::p("Total teacher-student ratio in", input$edu_ratio_loc),
            value = shiny::p(paste0(ceiling(pct), ":1"), style = "font-size: 36px"),
            theme = "secondary",
            showcase = bs_icon("aspect-ratio"),
            showcase_layout = "top right"
        )
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
            mode = "lines+markers",
            colors = discrete_pal
        )
        
        for (i in 1:length(input$edu_races)) {
            fig <- fig %>%
                add_trace(r = as.numeric(unlist(c(st_radar()[input$edu_races[i], ], 
                                                  st_radar()[input$edu_races[i], 1]))), 
                          theta = unlist(c(colnames(st_radar()), colnames(st_radar())[1])),
                          line = list(color = discrete_pal[input$edu_races[i]]),
                          marker = list(color = discrete_pal[input$edu_races[i]]),
                          name = paste(input$edu_races[i], "Students"))
        }
        
        fig %>% layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))))
    })
    
    # create lollipop plot
    output$lollipop_plot <- renderPlot({
        req(input$edu_races)
        
        pal <- discrete_pal
        data <- st_lollipop()
        
        # apply(df, 1, min or max) gets both mins and maxes from each row
        # add data + line segments for the plot
        p <- ggplot(data, aes(x = year, color = pal)) + 
            geom_segment(aes(x = year, xend = year, 
                             y = apply(data %>% select(-year), 1, min), 
                             yend = apply(data %>% select(-year), 1, max)), 
                         color = "grey", linewidth = 1.5) +
            theme_minimal() + labs(x = "School Year", y = "Testing Pass Rate (%)")
        
        # add points for each demographic user wants to show
        for (i in 1:length(input$edu_races)) {
            p <- p +
                # sym() turns a string into a variable, i.e "Asian" becomes Asian and therefore
                # becomes readable and fetches Asian from data frame
                # !! unpacks these variables from input$edu_races[i]
                geom_point(aes(x = year, y = !!sym(input$edu_races[i]), 
                               color = !!input$edu_races[i]), size = 5)
        }
        
        # finish plot with limits and legend
        p + ylim(0, 100) + scale_color_manual(name = "Demographic", values = pal)
    })
    
    # create choropleth data & map
    cohort_grad_data <- reactive({
        # get cohort pass rates data from education.r
        ch <- cohort_pass_rates %>%
            filter(cohort_year %in% input$cohort_year)
        
        # TODO CREATE NEW GEO_DATA FOR EDUCATIONAL DATA COMBINING WILLIAMSBURG AND JC COUNTY
        # NEXT SEMESTER THIS IS ALL YOU!
        
        # get geo data from sodem.r
        # TODO create separate file containing metadata such as geo data, color palettes, etc. ??
        gd <- geo_data %>%
            # remove williamsburg
            filter(loc_name != "williamsburg") %>%
            # rename james city county to williamsburg-james city county
            # ^j == first occurrence of "j" in regex
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
        # TODO find a way to get graduation rate across ALL years
        # by using variables instead of numbers
        # i haven't been able to get this to work
        pal <- colorBin(continuous_pal, 70:100) # change this if necessary
        map <- leaflet(data) %>%
            addPolygons(
                fillColor = ~pal(data$graduation_rate),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                smoothFactor = 0.5,
                opacity = 1.0,
                highlightOptions = highlightOptions(
                    bringToFront = TRUE, 
                    color = "white",
                    weight = 2),
                label = data$loc_name,
                popup = paste(
                    "<h3>", data$loc_name,"</h3>",
                    "<b>", input$grad_race, "Student Population:</b>", "TODO please get population",
                    "<br><b>", input$grad_race, "Graduation Rate (%):</b>", data$graduation_rate
                )
            ) %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = data$graduation_rate,
                title = "Graduation Rate",
                labFormat = labelFormat(suffix = "%")
            ) %>%
            addTiles()
        map
    })
    
    ### --- ECONOMICS --- ----------------------------------------------------------
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
        
        incomeGraph <- ggplot(income_years, 
                              aes(x = Year, y = `Median Income (US Dollars)`, 
                                  color = Demographic, group = Location, linetype = Location)) + 
            geom_line(data = va_total, size = 1.3, aes(linetype = Location)) +
            geom_line(data = va_black, size = 1.3, aes(linetype = Location)) +
            geom_line(data = hr_total, size = 1.3, aes(linetype = Location)) +
            geom_line(data = hr_black, size = 1.3, aes(linetype = Location)) +
            scale_color_manual(name = "Population", values = c("#A9A9A9", "#8B0000")) + # placeholder
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
        
        # TODO create new tab containing these plots
        # having this encased in a popup is going to go unnoticed by majority of users
        # we'd much rather have this in its own tab
        
        # # Function to create line plots for each locality
        # pick_n <- function(Locality) {
        #     # Filter data for the selected locality
        #     dataFiltered <- filter(all_hm_data, NAME == Locality)
        #     
        #     # Create ggplot line plot
        #     hm_line <- ggplot(dataFiltered,
        #                       aes(
        #                           x = Year,
        #                           y = Percent,
        #                           color = Demographic,
        #                           group = Demographic
        #                       )) +
        #         geom_line(position = "identity") +
        #         theme(axis.text.x = element_text(angle = 40)) +
        #         scale_fill_discrete(name = "",
        #                             labels = c("Black Home Owners", "White Home Owners")) +
        #         scale_fill_manual(values = c("#A9A9A9", "#8B0000")) +
        #         theme(legend.position = "bottom") +
        #         labs(title = Locality)
        #     
        #     # Return ggplot object
        #     return(hm_line)
        # }
        # 
        # # Apply pick_n function to each locality in the data
        # r <- lapply(1:length(unique(b_hm_19$NAME)), function(i) {
        #     pick_n(b_hm_19$NAME[i])
        # })
        
        # Create color palette for choropleth map
        pal <- colorBin(palette = continuous_pal,
                        domain = floor(min(b_hm_19$Percent)):ceiling(max(b_hm_19$Percent)))
        
        # Create Leaflet map object
        b_hmown_leaf_19 <- b_hm_19 %>%
            leaflet(options = leafletOptions(
                minZoom = 5,
                maxZoom = 15,
                drag = FALSE
            )) %>%
            addTiles() %>%
            addPolygons(
                data = b_hm_19,
                fillColor = ~ pal(Percent),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                smoothFactor = 0.5,
                opacity = 1.0,
                highlightOptions = highlightOptions(
                    bringToFront = TRUE, 
                    color = "white",
                    weight = 2),
                label = ~NAME,
                popup = ~paste("<h3>", NAME, "</h3>",
                               "<b>Black Homeowners (%):</b>", Percent,
                               "<br><b>Total Homeowners (%):</b>", tot_hm_19$Percent),
                group = "Black Homeowners"
            ) %>%
            addPolygons(
                data = tot_hm_19,
                fillColor = ~ pal(Percent),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                smoothFactor = 0.5,
                opacity = 1.0,
                highlightOptions = highlightOptions(
                    bringToFront = TRUE, 
                    color = "white",
                    weight = 2),
                label = ~NAME,
                popup = ~paste("<h3>", NAME, "</h3>",
                               "<b>Black Homeowners (%):</b>", b_hm_19$Percent,
                               "<br><b>Total Homeowners (%):</b>", Percent),
                group = "Total Homeowners"
                # TODO create separate tab showcasing black vs. total homeownership
            ) %>%
            addLayersControl(
                baseGroups = c("Black Homeowners", "Total Homeowners"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            hideGroup("Total Homeowners") %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = ~Percent,
                title = "Homeowners",
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
    # Uneployment Rate
    # Reactive expression for selected unemployment rate year
    var_unemploymentRate <- reactive({
        input$UnemploymentRateSlider  # Retrieve selected year from slider input
    })
    
    # Render the unemployment plot using plotly
    output$unemployment_plot <- renderPlotly({
        year <- var_unemploymentRate()  # Get the selected year
        generate_unemployment_plot(year)  # Generate the plot for the selected year
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
        # TODO create fixed y-values for the plot
        # also please don't align the plot title to center :(
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
        as.character(input$PovertyCountYearDrop)
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
    
    #Veterans
    # Define a reactive expression to retrieve the selected year from the VeteranSlider input
    var_veteran <- reactive({
        input$VeteranSlider
    })
    
    # Render Leaflet map based on the selected year
    output$veteran_map <- renderLeaflet({
        # Check the selected year and load corresponding data
        selected_year <- var_veteran()
        data <- read_veteran_data(selected_year)
        vet_data <- data$vet_data
        military_bases <- data$military_bases
        
        # Define color palette
        pal <- colorBin(
            palette = continuous_pal,
            domain = floor(min(vet_data$Percent)):ceiling(max(vet_data$Percent))
        )
        
        # Create Leaflet map
        veteran_map <- vet_data %>%
            leaflet() %>%
            addTiles() %>%
            addPolygons(
                fillColor = ~pal(Percent),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                smoothFactor = 0.5,
                opacity = 1.0,
                highlightOptions = highlightOptions(
                    bringToFront = TRUE, 
                    color = "white",
                    weight = 2),
                label = ~NAME,
                popup = ~paste("<h3>", NAME, "</h3>", 
                               "<b>Black Veterans (%): </b>", Percent),
                group = "Veteran Status"
            ) %>%
            addMarkers(
                data = military_bases,
                label = ~base_name,
                popup = ~paste("<h3>", base_name, "</h3>",
                               "<b>Branch: </b>", branch),
                group = "Military Bases"
            ) %>%
            addLayersControl(
                baseGroups = c("Veteran Status"),
                overlayGroups = c("Military Bases"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            hideGroup("Military Bases") %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = ~Percent,
                title = "Black Veterans",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1
            )
    })
    
    #Household Well-being
    var_well <- reactive({
        input$select_wellbeing
    })
    
    output$wellbeing_maps <- renderLeaflet({
        # Function to create leaflet map for each variable
        create_leaflet_map <- function(data_file, col_names, legend_title) {
            data <- read_rds(data_file)
            colnames(data)[4] <- "Percent"
            colnames(data)[3] <- col_names
            data_pal <- colorBin(palette = continuous_pal, domain = data$Percent)
            
            map <- data %>%
                leaflet() %>% 
                addTiles() %>% 
                addPolygons(fillColor = ~ data_pal(Percent), 
                            color = "black",
                            weight = 1,
                            fillOpacity = 0.75,
                            smoothFactor = 0.5,
                            opacity = 1.0,
                            highlightOptions = highlightOptions(
                                bringToFront = TRUE, 
                                color = "white",
                                weight = 2),
                            label = ~NAME,
                            popup = ~paste("<h3>", NAME, "</h3>",
                                           "<b>", col_names, " (%): </b>", Percent)) %>% 
                addLegend("bottomright",
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
            map <- create_leaflet_map("data/economics/household_wellbeing/foodstmp.rds", "Food Stamps", "Food Stamps")
        } 
        else if (selected_variable == "Percent of Black County Migration") {
            map <- create_leaflet_map("data/economics/household_wellbeing//mobile.rds", "Intra-County Migration", "County Migration")
        } 
        else if (selected_variable == "Percent of Black Population that uses car/truck/van to get to work") {
            map <- create_leaflet_map("data/economics/household_wellbeing/priv_trans.rds", "Private Transport", "Private Transportation")
        } 
        else if (selected_variable == "Percent of Black Population that uses public transportation to get to work") {
            map <- create_leaflet_map("data/economics/household_wellbeing/pub_trans.rds", "Public Transport", "Public Transportation")
        } 
        else if (selected_variable == "Percent of Black Households with a computer with broadband internet") {
            map <- create_leaflet_map("data/economics/household_wellbeing/compin.rds", "Computer and Internet", "Computer with Internet Access")
        } 
        else if (selected_variable == "Percent of Black Households without a computer") {
            map <- create_leaflet_map("data/economics/household_wellbeing/nocomp.rds", "No Computer", "No Computer Access")
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
    
    #Business
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
        } 
        else {
            selected_metro <- input$metroArea
            metro_summary_metrics %>% filter(NAME == selected_metro)
        }
    })
    
    output$plot <- renderPlot({
        req(input$metrics)  # Ensure that a metric is selected
        # TODO there's an error here??? i don't get it
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
            scale_color_manual(values = c("Positive" = "green", "Negative" = "red", "Flat" = "blue")) +
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
    
    # Media/Entertainment Tab -----------------------------------------------
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
            # TODO fix palette range
            pal <- colorBin(continuous_pal, 3:6, bins = 4)
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
            pal <- colorBin(continuous_pal, 6:11, bins = 6)
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
                    position = "bottomright",
                    title = "Num. Providers",
                    labFormat = labelFormat(digits = 0)
                ) 
        }
    })
    
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
            pal <- colorBin(palette = continuous_pal, domain = 0:3, bins = 4)
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
            pal <- colorBin(continuous_pal, 0:5, bins = 6)
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
            theme_bw() +
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
            geom_bar(stat = "identity", position = "dodge") + 
            theme_bw() +
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
            geom_bar(stat = "identity", position = "dodge") + 
            theme_bw() +
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
            geom_bar(stat = "identity", position = "dodge") + 
            theme_bw() +
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
                # TODO fix popup (look at labs)
                label = ~city_name # placeholder cause labs is UGLY
            ) %>%
            addLegend(
                pal = pal, 
                values = ~count, 
                title = 'Number of Stations',
                position = "bottomright"
            )
        
        radio_map
    })
    
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
    # TODO investigate why this slider won't change output
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
    # End of Media/Entertainment Tab -----------------------------------------------
    
    ### ---Politics/Justice Tab ---------------------
    # Render the Traffic Race plot
    output$trafficRace <- renderPlot({
        # Call the function to read the data
        data <- read_traffic_data()
        
        # Analyze the data
        print(is.data.frame(data))  # Check if data is a data frame
        print(ncol(data))            # Print the number of columns
        print(nrow(data))            # Print the number of rows
        
        # Create the Race Count plot
        trafficRace <- ggplot(data, aes(x = RACE)) +  
            theme_bw() +                    # Set theme to black and white
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
    # Render the Jurisdiction plot
    output$jurisdiction <- renderPlot({
        # Call the function to read the data
        data <- read_traffic_data()
        
        # Create the Race vs Jurisdiction plot
        jurisdiction <- ggplot(data, aes(x = JURISDICTION, fill = RACE)) +
            geom_bar(position = "dodge") +                   # Add dodged bar plot
            theme_bw() +                    # Set theme to black and white
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),       # Set x-axis text properties
                  axis.text.y = element_text(hjust = 1, size = 15),                   # Set y-axis text properties
                  plot.title = element_text(color = "black", size = 24, face = "bold"),  # Set plot title properties
                  legend.text=element_text(size=15)) +                                # Set legend text properties
            ylim(0, 2000)                                  # Set y-axis limits
        
        jurisdiction
    })
    
    #Toggle Jurisdictions
    
    # Reactive variable for selected stop
    var_stop <- reactive(input$select_stop)
    
    # Render plot for Race vs Age for selected jurisdiction
    output$jurisdiction2 <- renderPlot({
        # Read the CSV data
        data <- read_traffic_data()
        
        # Filter data based on selected jurisdiction
        jurisdiction2 <- data %>% 
            filter(JURISDICTION == var_stop()) %>%         # Filter data based on selected jurisdiction
            ggplot(aes(y = RACE, x = AGE, color = RACE)) + # Create scatter plot
            theme_bw() +                    # Set theme to black and white
            geom_boxplot(size = 1, outlier.shape = 1,      # Add boxplot with specified properties
                         outlier.color = "black",
                         outlier.size = 3) +
            geom_jitter(alpha = 0.25, width = .2) +        # Add jittered points
            labs(title = "Traffic Stops Data") +           # Set plot title
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),       # Set x-axis text properties
                  axis.text.y = element_text(hjust = 1, size = 15),                   # Set y-axis text properties
                  plot.title = element_text(color = "black", size = 24, face = "bold"),  # Set plot title properties
                  legend.position = "none") +              # Remove legend
            coord_flip()                                   # Flip coordinates to horizontal
        
        jurisdiction2
    })
    
    
    # City council demographics Race
    output$cityd <- renderPlot({
        # Read data
        data <- read_race()
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
            theme_bw() +                    # Set theme to black and white
            theme(axis.title.y = element_text(),                    # Set y-axis title properties
                  axis.title = element_text(),                      # Set x-axis title properties
                  axis.text.x = element_text(angle = 45, size = 13, hjust = 1),  # Set x-axis text properties
                  axis.text.y = element_text(hjust = 1, size = 15),  # Set y-axis text properties
                  legend.text = element_text(size = 20),            # Set legend text properties
                  legend.title = element_blank()) +                  # Remove legend title
            ggtitle('City Council Demographics by Race 2021') +      # Set plot title
            ylab('count')                                           # Set y-axis label
        
        cityd  # Return the plot
    })
    
    # City council demographics Gender
    output$cityd2 <- renderPlot({
        # Read data
        data <- read_gender()
        politicans_df <- data$politicans_df
        
        # Plot City Council demographics by gender
        cityd2 <- politicans_df %>%
            pivot_longer(4:7, names_to = 'demographic') %>%       # Reshape data for plotting
            mutate(demographic = str_sub(demographic,  14)) %>%   # Extract demographic information
            select(-c(Mayor, Vice.Mayor)) %>%                    # Remove Mayor and Vice Mayor columns
            filter(demographic == 'Female' | demographic == 'Male') %>%  # Filter for Female and Male demographics
            ggplot(aes(x = City, y = value, fill = demographic)) +   # Set up plot aesthetics
            geom_bar(stat = 'identity', position = 'dodge') +       # Add dodged bar plot
            theme_bw() +                    # Set theme to black and white
            theme(axis.title.y = element_text(),                    # Set y-axis title properties
                  axis.title = element_text(),                      # Set x-axis title properties
                  axis.text.x = element_text(angle = 45, size = 13, hjust = 1),  # Set x-axis text properties
                  axis.text.y = element_text(size = 15),            # Set y-axis text properties
                  legend.text = element_text(size = 20),            # Set legend text properties
                  legend.title = element_blank()) +                  # Remove legend title
            ggtitle('City Council Demographics by Gender 2021') +     # Set plot title
            ylab('count')                                           # Set y-axis label
        
        cityd2  # Return the plot
    })
    
    # Jail plots
    var_jailChoice <- reactive({
        input$select_jailChoice
    })
    
    # Jail plots for Jail Population
    output$jail <- renderPlot({
        # Call the function to read the data
        data_info <- read_jail_data(var_jailChoice())
        data <- data_info$data
        title <- data_info$title
        
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
            theme_bw() +
            scale_colour_viridis_d() +
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
        va_hampton_roads_incarceration_trends <- read.csv('data/politics/incarceration/va_hampton_roads_incarceration_trends.csv')
        
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
        va_hampton_roads_incarceration_trends <- read.csv('data/politics/incarceration/va_hampton_roads_incarceration_trends.csv')
        
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
    
    #Prison rates
    # Reactive expression to get the selected year from the input
    var_prisonYear <- reactive({
        input$select_prisonYear
    })
    
    # Render Leaflet map for prison data
    output$prison <- renderLeaflet({
        year <- var_prisonYear()
        
        # Read and process prison data
        merged_data <- read_process_prison_data(year)
        
        # Create labels for map markers
        labs <- lapply(seq(nrow(merged_data)), function(i) {
            paste0(
                '<p>', merged_data[i, "city_name"], '<p></p>', 
                paste(year, ' Prison Admission Rate Per 100k: '), 
                merged_data[i, "total_prison_adm_rate"], '</p>'
            ) 
        })
        
        # Define color palette for map markers
        pal <- colorBin(continuous_pal, merged_data$total_prison_adm_rate)
        
        # Create Leaflet map
        # TODO fix this. look at merged_data and make sure leaflet takes in a data frame of class
        # "sf"
        prison <- merged_data %>%
            leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
            addTiles() %>%
            addPolygons(
                data = merged_data$geometry, 
                color = pal(merged_data$total_prison_adm_rate),
                weight = 0.5,
                fillOpacity = 0.7, 
                smoothFactor = 0,
                highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                label = lapply(labs, htmltools::HTML)
            ) %>%
            addLegend(pal = pal, values = ~merged_data$total_prison_adm_rate, title = 'Admission Rate Per 100k', opacity = .75)  
        
        prison
    })
    
    # Gentrification (map home values)
    output$map_homevalues <- renderLeaflet({
        # Load zipcode geojson file
        zips <- geojson_read("https://raw.githubusercontent.com/jalbertbowden/open-virginia-gis/master/zip-codes/json/zt51_d00.geojson", what = "sp")
        
        # Load home value data
        data_jim_final <- read.csv("data/politics/gentrification/data_jim_final.csv")
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
        pal <- colorBin(continuous_pal, joined_jim$homevalue)
        
        # Load redlining data
        norfolk <- geojson_read("data/politics/gentrification/VANorfolk1940.geojson", what = "sp")
        cols <- c("green", "blue", "yellow", "red")
        
        # Define color factor for redlining grades
        pal2 <- colorFactor(cols, domain = norfolk$holc_grade)
        
        # Create Leaflet map
        map_homevalues <- joined_jim %>% 
            leaflet() %>% 
            addTiles() %>% 
            # addPolygons(
            #   data = norfolk,
            #   color = ~ pal2(norfolk$holc_grade),
            #   opacity = 0.9,
            #   stroke = F
            # ) %>%
            addPolygons(
                fillColor = ~ pal(joined_jim$homevalue),
                color = "black",
                weight = 1,
                fillOpacity = 0.75,
                smoothFactor = 0.5,
                opacity = 1.0,
                highlightOptions = highlightOptions(
                    bringToFront = TRUE, 
                    color = "white",
                    weight = 2),
                # TODO turn this popup to a proper label and popup
                # look at sodem and education leaflet maps for inspiration
                popup = ~popup_info
            ) %>%
            addLegend(
                pal = pal,
                values = ~ joined_jim$homevalue,
                position = "bottomright",
                title = "Median Home Value",
                labFormat = labelFormat(prefix = "$")
            )
        
        map_homevalues
    }) 
    
    
    # People and Values Tab -----------------------------------------------
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
        if (var_famtext() == "Percent of Black Children under 18 in Female Head of Household") {
            "Percentage of Black Children under the age of 18 that live in a female-headed household. 
            We included this indicator as research has shown that female-headed households have
            a greater risk of poverty and are more likely to be food-insecure. In Hampton Roads, 
            regardless of location, majority of Black households with children under 18 have a 
            female as the head.  For 7 of the 11 areas, over 50% of Black households for which data 
            is available, is led by a female. This may suggest some family instability for half of 
            the black children in the Hampton Roads region."
        }
        
        else if (var_famtext() == "Percent of Black Grandparents who are Guardians") {
            "Percent distribution of Black grandparents who live with grandchildren who are 
            responsible for said grandchildren. Grandparents becoming principal guardians for their 
            grandchildren suggest economic distress for families, as such, we included this indicator 
            in our analysis. There are some differences in this distribution across the cities and 
            counties in Hampton Roads.  For example, of those Black grandparents who live with their 
            own grandchildren, 80% of them are responsible for their grandchildren in Franklin, 
            whereas in Gloucester, the rate is a low 4.8%."
            
        }
        
        else if (var_famtext() == "Percent of Married Black Population 15 years and over") {
            "The percentage of the Black population 15 years and over who are married. 
            The literature shows that married households tend to be less impoverished and are more
            economically stable and stable. Except for York, Gloucester, Chesapeake (about 50%), 
            there is a low marriage rate among the Black population. Marriage rates range from 
            as low as 20% (Norfolk) to 51% (Chesapeake), with the average rate being around 35%."
        }
    })
    
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
        pal2 <- colorBin(continuous_pal, plot_data$value)
        religion <- plot_data %>%
            leaflet() %>% 
            addTiles() %>%
            addPolygons(
                data = plot_data$geometry, 
                color = "black",
                fillColor = pal2(plot_data$value),
                weight = 1,
                fillOpacity = 0.75,
                smoothFactor = 0.5,
                opacity = 1.0,
                highlightOptions = highlightOptions(
                    bringToFront = TRUE, 
                    color = "white",
                    weight = 2),
                label = lapply(labs, htmltools::HTML)) %>%
            addLegend(
                "bottomright",
                pal = pal2, 
                values = ~plot_data$value,
                title = 'Percent Adherence'
            )
        religion
    })
    
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
            theme_bw() +
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
    output$credit_scores <- renderPlot({
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
            theme_bw() +
            geom_col(aes(x = ppfs1482, y = n, fill = ppracem)) +
            scale_x_discrete(limits = positions) + 
            ylim(0, 110) +
            labs(x = "Credit Score Ranking") +
            labs(y = "Count") +
            ggtitle("Credit Score Ranking by Race") +
            labs(fill = "Race")
        
        # TODO figure out why this cannot render as plotly object
        # credit_scores <- ggplotly(credit_scores) 
        credit_scores
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
            theme_bw() +
            geom_col(aes(x = DOV_FL, y = n, fill = ppracem)) +
            labs(x = "Answered 'Do not know' during survey") +
            labs(y = "Count") +
            labs(fill = "Race")
        dont_know <- ggplotly(dont_know)
    })
    
    # Food Banks ---------------------------------------------------------
    output$foodBanksLeaflet <- renderLeaflet({
        foodBankLoc <- read.csv("./data/people_values/foodBank/FoodBanks.csv")
        countyOutlines <- read_sf(dsn = "./data/people_values/countyOutlines/countyOutlines.shp")
        labs <- paste0(foodBankLoc$name, "<br></br><a href='", foodBankLoc$url,"'>View</a>")
        pal <- colorFactor(continuous_pal, foodBankLoc$county)
        foodBank.map <- foodBankLoc %>%
            leaflet() %>%
            addTiles() %>%
            addCircleMarkers(~lng, ~lat, popup=lapply(labs, htmltools::HTML), radius = 5, 
                             fillOpacity = 1.0, weight = 1,  color = ~pal(foodBankLoc$county)) %>%
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
            theme_bw() +
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
    
    # Food Insecurity -----------------------------------------------------
    # function to generate a leaflet map
    generateFoodAccessMap <- function(data, valueColumn, titleSuffix) {
        value <- round(as.numeric(data[[valueColumn]]), 2)
        county <- word(data$County, 1, -2)
        pal <- colorBin(continuous_pal, domain = as.double(value))
        
        foodAccessMap <- leaflet(data) %>%
            addTiles() %>%
            addPolygons(fillColor = ~pal(as.double(value)), 
                        color = "black",
                        weight = 0.5,
                        fillOpacity = 0.75,
                        smoothFactor = 0.5,
                        opacity = 1.0,
                        highlightOptions = highlightOptions(
                            bringToFront = TRUE, 
                            color = "white",
                            weight = 1),
                        label = paste0(county, ": ", value, "%")) %>%
            addLegend(position = "bottomright", pal = pal, values = as.double(value), 
                      title = paste(titleSuffix, "(%)"))
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
    
    # People and Values Tab -----------------------------------------------
    
    # Feedback button-----------------------------------------------
    feedback(input,output,session)
    # End feedback button-----------------------------------------------
}

return(server)
