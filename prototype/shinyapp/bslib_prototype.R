library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(tidyverse)

source("sodem.r")
source("education.r")
ui <- page_navbar(
    title = "phaceholder",
    selected = "overview",
    theme = bs_theme(preset = "journal"),
    useShinyjs(),
    nav_menu(
        title = "Overview",
        nav_panel(
            title = "Project Background",
            value = "overview",
            # probably the only time you'll see fluidRow since it allows text alignment
            fluidRow(
                style = "margin: 2px",
                align = "center",
                includeMarkdown("markdown/overview/title.md"),
            ),
            br(""),
            fluidRow(
                style = "margin: 6px",
                column(
                    width = 4,
                    includeMarkdown("markdown/overview/hampton_roads.md"),
                ),
                column(
                    width = 4,
                    includeMarkdown("markdown/overview/project_background.md"),
                ),
                column(
                    width = 4,
                    includeMarkdown("markdown/overview/project_goals.md"),
                ),
            )
        ),
        nav_panel(title = "Data & Methodology",)
    ),
    nav_panel(
        title = "Sociodemographics",
        layout_sidebar(
            sidebar = sidebar(
                width = validateCssUnit("33%"),
                title = "Demographic Characteristics",
                includeMarkdown("markdown/sodem/sodem_characteristics.md")
            ),
            layout_column_wrap(
                width = 1,
                heights_equal = "row",
                layout_column_wrap(
                    width = 1/3,
                    uiOutput("age_value_box"),
                    uiOutput("pop_value_box"),
                    uiOutput("total_pop_value_box")
                ),
                navset_card_tab(
                    height = validateCssUnit("1000px"),
                    full_screen = TRUE,
                    wrapper = card_body(),
                    title = "Hampton Roads Demographics",
                    id = "demographic",
                    nav_panel(
                        title = "Median Age",
                        value = "age",
                        leafletOutput("age_choropleth")
                    ),
                    nav_panel(
                        title = "Black Population",
                        value = "pop",
                        leafletOutput("pop_choropleth")
                    ),
                    card_footer("Data: 2022 ACS 5-Year DP05 Table")
                )
            )
        )
    ),
    nav_panel(
        title = "Education",
        layout_sidebar(
            sidebar = sidebar(
                title = "Standardized Testing",
                width = validateCssUnit("20%"), # sidebar takes up x% of page
                selectInput(
                    inputId = "loc",
                    label = "Select locality:",
                    selected = "Chesapeake",
                    choices = unique(st_data$division_name)
                ),
                checkboxGroupInput(
                    inputId = "races",
                    label = "Select races:",
                    choices = c("Black" = "Black",
                                "White" = "White",
                                "Asian" = "Asian",
                                "Hispanic" = "Hispanic"),
                    selected = "Black"
                )
            ),
            accordion(
                multiple = FALSE,
                accordion_panel(
                    title = "Standardized Testing",
                    navset_card_tab(
                        nav_panel(
                            title = "2022-2023 Testing Results",
                            layout_column_wrap(
                                plotlyOutput("radio_plot"),
                                layout_column_wrap(
                                    uiOutput("st_value_boxes")
                                ))
                        ),
                        nav_panel(
                            title = "Race Comparison",
                            card_body(plotOutput("lollipop_plot")),
                        ),
                        card_footer("Source: VDOE Annual Pass Rates (Division Subject Area)")
                    ),
                    
                ),
                accordion_panel(
                    title = "Educators?",
                    "meow"
                )
            )
        )
    ),
    nav_spacer(),
    nav_menu(
        title = "Links",
        align = "right",
        nav_item(
            tags$a("GitHub", 
                   href = "https://github.com/afomiya02/testrepo", 
                   target = "_blank")
        ),
        nav_item(
            tags$a("Black BRAND", 
                   href = "https://blackbrand.biz/", 
                   target = "_blank")
        )
    )
)

server <- function(input, output, session) {
    ### --- SOCIODEMOGRAPHICS ---
    
    ## VALUE BOXES
    # black population value box
    output$pop_value_box <- renderUI({
        black_pop <- round(sum(sodem_data$black_or_african_american) / 
                               sum(sodem_data$total_population), 2) * 100
        box <- value_box(
            title = "Black Population (%):",
            value = black_pop,
            showcase = bs_icon("pie-chart-fill"),
            theme = "info"
        )
        box
    })
    
    output$age_value_box <- renderUI({
        median_age <- round(mean(sodem_data$median_age_years), 1)
        box <- value_box(
            title = "Median Age:",
            value = shiny::p(median_age, "years"),
            showcase = bs_icon("cake"),
            theme = "primary"
        )
        box
    })
    
    output$total_pop_value_box <- renderUI({
        box <- value_box(
            title = "Total Population",
            value = sum(sodem_data$total_population),
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

    # reactive that gets all necessary info for lollipop plot
    st_lollipop <- reactive({
        df <- st_data %>%
            dplyr::filter(division_name %in% input$loc) %>%
            # dplyr::filter(subgroup %in% input$races) %>%
            dplyr::filter(subgroup %in% c("Black", "White")) %>%
            group_by(subgroup) %>% dplyr::summarise(
                across(ends_with("pass_rate"), mean)
            ) %>%
            pivot_longer(!subgroup, names_to = "year", values_to = "pass_rate") %>%
            dplyr::mutate(year = str_remove(year, "_pass_rate")) %>%
            pivot_wider(names_from = subgroup, values_from = pass_rate)
        df
    })
    
    st_meta <- reactive({
        df <- student_count_data %>%
            filter(division_name %in% input$loc)
        df
    })
    
    # just a header
    output$metadata <- renderUI({
        HTML(h2(input$loc))
    })
    
    # create value boxes
    output$st_value_boxes <- renderUI({
        prop <- st_meta() %>% filter(race == "Black") %>% select(total_count) /
            sum(st_meta()$total_count)
        vbs <- list(
            value_box(
                title = "# of Black students:",
                value = st_meta() %>% filter(race == "Black") %>% select(total_count),
                theme = "primary"
            ),
            value_box(
                title = "Total # of students:",
                value = sum(st_meta()$total_count),
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
    
    # create radio plot with subetted data
    output$radio_plot <- renderPlotly({
        req(input$races)
        fig <- plot_ly(
            data = st_radar(),
            type = "scatterpolar",
            mode = "markers+lines"
        )
        
        for (i in 1:length(input$races)) {
            fig <- fig %>%
                add_trace(r = as.numeric(unlist(c(st_radar()[input$races[i], ], st_radar()[input$races[i], 1]))),
                          theta = unlist(c(colnames(st_radar()), colnames(st_radar())[1])),
                          name = paste(input$races[i], "Students"))
        }
        
        fig %>% layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))))
    })
    
    # create lollipop plot
    output$lollipop_plot <- renderPlot({
        req(input$races)
        p <- ggplot(st_lollipop()) + 
            geom_segment(aes(x = year, xend = year, y = Black, yend = White), color = "grey") +
            geom_point(aes(x = year, y = Black), color = "black", size = 3) +
            geom_point(aes(x = year, y = White), color = "orange", size = 3) +
            theme_minimal() + labs(x = "School Year", y = "Testing Pass Rate (%)") +
            ylim(0, 100)
        p
    })
}

shinyApp(ui, server)