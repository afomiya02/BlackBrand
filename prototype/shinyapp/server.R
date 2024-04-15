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

server <- function(input, output, session) {
    ### --- SOCIODEMOGRAPHICS ---
    
    ## VALUE BOXES
    # black population value box
    output$pop_value_box <- renderUI({
        black_pop <- round(sum(sodem_data$black_or_african_american) / 
                               sum(sodem_data$total_population), 2) * 100
        box <- value_box(
            title = p("Black Population (%):", style = "font-size: 20px"),
            value = shiny::p(black_pop, style = "font-size: 36px"),
            showcase = bs_icon("pie-chart-fill"),
            theme = "info"
        )
        box
    })
    
    output$age_value_box <- renderUI({
        median_age <- round(mean(sodem_data$median_age_years), 1)
        box <- value_box(
            title = shiny::p("Median Age:", style = "font-size: 20px"),
            value = shiny::p(median_age, "years", style = "font-size: 36px"),
            showcase = bs_icon("cake"),
            theme = "primary"
        )
        box
    })
    
    output$total_pop_value_box <- renderUI({
        box <- value_box(
            title = shiny::p("Total Population:", style = "font-size: 20px"),
            value = shiny::p(sum(sodem_data$total_population), style = "font-size: 36px"),
            showcase = bs_icon("check2-all"),
            theme = "primary"
        )
        box
        
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
    ## subset standardized testing radar data
    
    # # just a header
    # output$metadata <- renderUI({
    #     HTML(h2(input$loc))
    # })
    
    # create value boxes
    output$st_value_boxes <- renderUI({
        prop <- local_student_data() %>% filter(races == "Black") %>% select(total_count) /
            sum(local_student_data()$total_count)
        vbs <- list(
            value_box(
                title = "# of Black students:",
                value = local_student_data() %>% filter(races == "Black") %>% select(total_count),
                theme = "primary"
            ),
            value_box(
                title = "Total # of students:",
                value = sum(local_student_data()$total_count),
                theme = "primary"
            ),
            value_box(
                title = "Percentage of Black students:",
                value = paste0(round(prop * 100, 2), "%"),
                theme = "info"
            )
        )
        vbs
    })
    
    local_student_data <- reactive({
        df <- student_count_data %>%
            filter(division_name %in% input$loc) %>%
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
            filter(division_name %in% input$loc) %>%
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
            scale_fill_brewer(palette = "Dark2") +
            xlim(c(0.2, hsize + 0.5)) +
            theme(panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank()) +
            ggtitle(paste("Racial Distribution of Students in", input$loc))
        p
    })
    
    output$educator_race_plot <- renderPlot({
        hsize <- 3
        p <- ggplot(local_educator_data(), aes(x = hsize, y = total_count, fill = races)) +
            geom_col(color = "black") +
            geom_text(aes(x = 2.1, label = total_count), position = position_stack(vjust = 0.5)) +
            coord_polar(theta = "y") +
            scale_fill_brewer(palette = "Dark2") +
            xlim(c(0.2, hsize + 0.5)) +
            theme(panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank()) +
            ggtitle(paste("Racial Distribution of Educators in", input$loc))
        p
    })
    
    # reactive that gets all necessary info for radar plot
    st_radar <- reactive({
        df <- st_data %>%
            dplyr::filter(division_name %in% input$loc) %>%
            dplyr::filter(subgroup %in% input$races) %>%
            select(c(subject, subgroup, `2022-2023_pass_rate`)) %>%
            # pivot dataset such that subjects are columns and
            # subjects are row names
            pivot_wider(names_from = subject, values_from = `2022-2023_pass_rate`) %>%
            column_to_rownames(., "subgroup")
        df
    })
    
    # create radio plot with subetted data
    output$radar_plot <- renderPlotly({
        req(input$races)
        
        pal <- c("Black" = "black",
                 "White" = "orange",
                 "Asian" = "darkgreen",
                 "Hispanic" = "violet")
        
        fig <- plot_ly(
            data = st_radar(),
            type = "scatterpolar",
            mode = "lines+markers"
        )
        
        # TODO: CREATE PROPER FIXED COLOR PALETTE THAT MATCHES
        # CHECKBOX INPUTS WITH ITS RESPECTIVE COLOR
        
        # iterate thru input$races to get line for each race in plot
        # have to wrap r and theta such that the values iterate like a circle
        # (e.g [Black, White, Asian, Black])
        for (i in 1:length(input$races)) {
            fig <- fig %>%
                add_trace(r = as.numeric(unlist(c(st_radar()[input$races[i], ], 
                                                  st_radar()[input$races[i], 1]))), 
                          theta = unlist(c(colnames(st_radar()), colnames(st_radar())[1])),
                          name = paste(input$races[i], "Students"))
        }
        
        fig %>% layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))))
    })
    
    # reactive that gets all necessary info for lollipop plot
    st_lollipop <- reactive({
        df <- st_data %>%
            dplyr::filter(division_name %in% input$loc) %>%
            dplyr::filter(subgroup %in% input$races) %>%
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
        req(input$races)
        
        # TODO create separate legend w/ colors
        pal <- c("Black" = "black",
                 "White" = "orange",
                 "Asian" = "darkgreen",
                 "Hispanic" = "violet")
        
        # apply(df, 1, min or max) gets both mins and maxes from each row
        p <- ggplot(st_lollipop(), aes(x = year)) + 
            geom_segment(aes(x = year, xend = year, y = apply(st_lollipop() %>% select(-year), 1, min), 
                             yend = apply(st_lollipop() %>% select(-year), 1, max)), color = "grey", linewidth = 1.5) +
            theme_minimal() + labs(x = "School Year", y = "Testing Pass Rate (%)")
        
        for (i in 1:length(input$races)) {
            p <- p +
                # sym() turns a string into a variable, i.e "Asian" becomes Asian and therefore
                # becomes readable and fetches Asian from data frame
                # !! unpacks these variables from input$races[i]
                geom_point(aes(x = year, y = !!sym(input$races[i])), colour = pal[input$races[i]], size = 5)
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
            # filter(race %in% c("All Students", input$races))
            filter(race %in% "Black")
        df
    })
    
    output$cohort_choropleth_map <- renderLeaflet({
        data <- cohort_grad_data()
        pal <- colorBin("YlOrRd", data$graduation_rate)
        map <- leaflet(data) %>%
            addPolygons(
                color = "black",
                fillColor = ~pal(data$graduation_rate),
                fillOpacity = 0.75,
                weight = 1,
                popup = paste(
                    "<h1>", data$loc_name,"</h1>",
                    "<b>Black Student Population:</b>", "purr",
                    "<br><b>Total Student Population:</b>", "meow"
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
    
}

return(server)
shinyApp(ui = ui, server = server)