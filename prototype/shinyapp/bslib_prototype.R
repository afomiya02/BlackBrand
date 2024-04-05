library(shiny)
library(bslib)
library(bsicons)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(tidyverse)

source("sodem.r")
source("education.r")
ui <- page_navbar(
    title = "phaceholder",
    selected = "overview",
    theme = bs_theme(preset = "journal"),
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
    nav_panel(title = "Sociodemographics",
              page_fillable(
                  layout_columns(
                      layout_columns(
                          col_widths = c(4, 8),
                          card(
                              card_header("Demographic Characteristics"),
                              card_body(includeMarkdown("markdown/sodem/sodem_characteristics.md")),
                              card_footer("Data: 2022 ACS 5-Year DP05 Table")
                          ),
                          layout_columns(
                              col_widths = 12,
                              row_heights = c(1, 6),
                              #value boxes
                              layout_columns(
                                  uiOutput("median_age_box"),
                                  uiOutput("race_box")
                              ),
                              # tabs between median age and race
                              navset_card_tab(
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
                                  )
                              )
                          )
                      )
                  )
              )
    ),
    nav_panel(
        title = "Education",
        layout_sidebar(
            sidebar = sidebar(
                title = "Standardized Testing Tracker",
                width = validateCssUnit("25%"), # sidebar takes up x% of page
                varSelectInput(
                    inputId = "loc",
                    label = "Select locality:",
                    selected = "hampton",
                    data = iris
                )
            ),
            layout_columns(
                col_widths = 12,
                row_heights = c(1, 3, 2),
                layout_column_wrap(
                    width = 1 / 3,
                    card("value box 1"),
                    card("value box 2"),
                    card("value box 3")
                ),
                card(
                    card_header("2022-2023 Standardized Testing Comparison Radar Plot"),
                    card_body(
                        layout_column_wrap(width = 1 / 3,
                                           "plot goes here",
                                           "metadata goes here")
                    ),
                    card_footer("Source: VDOE Annual Pass Rates (Division Subject Area)")
                ),
                card("lollipop plot"),
            )
        )
    )
)

server <- function(input, output, session) {
    ### --- SOCIODEMOGRAPHICS ---
    
    ## VALUE BOXES
    # black population value box
    output$race_box <- renderUI({
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
    
    # median age value box
    output$median_age_box <- renderUI({
        median_age <- round(mean(sodem_data$median_age_years), 1)
        box <- value_box(
            title = "Median Age (years):",
            value = median_age,
            showcase = bs_icon("cake"),
            theme = "primary"
        )
        box
    })
    
    output$choropleth <- renderLeaflet({
        leaflet(heatmap_data) %>% addTiles()
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
}

shinyApp(ui, server)