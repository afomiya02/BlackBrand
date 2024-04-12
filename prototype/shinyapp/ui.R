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
                                plotlyOutput("radar_plot"),
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
                    plotOutput("educator_race_plot")
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

return(ui)

shinyApp(ui, server)