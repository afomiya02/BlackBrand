library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(tidyverse)
library(thematic) # INSTALL THESE!
library(ragg)

source("sodem.r")
source("education.r")
source("economics.r")

options(shiny.useragg = TRUE)
thematic_shiny(font = "auto")

ui <- page_navbar(
    title = img(src="logo_WIDE.png"),
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
                title = "Education in Hampton Roads",
                width = validateCssUnit("20%"), # sidebar takes up x% of page
                selectInput(
                    inputId = "loc",
                    label = "Select location:",
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
                    title = "Educators vs. Students",
                    card(
                        card_header("Distribution of Educators per Location"),
                        card_body(
                            layout_column_wrap(
                                plotOutput("educator_race_plot"),
                                plotOutput("student_race_plot")
                            )
                        ),
                        card_footer("Source: VDOE Virginia Educator Ethnicity and Race Data")
                    )
                ),
                accordion_panel(
                    title = "Educational Attainment (Graduation Rates)",
                    card(
                        card_header("Graduation Rate Choropleth Map"),
                        card_body(class = "p-0", leafletOutput("cohort_choropleth_map")),
                        p(), # spacer
                        card_body(
                            sliderInput(
                                inputId = "cohort_year",
                                label = "Select Year:",
                                value = 2023,
                                min = min(cohort_pass_rates$cohort_year),
                                max = max(cohort_pass_rates$cohort_year),
                                round = TRUE,
                                step = 1,
                                sep = "",
                                width = "100%",
                                animate = animationOptions(interval = 2400)
                            ),
                        ),
                        card_footer("Source: VDOE Cohort Graduation Build-a-Table")
                    )
                )
            )
        )
    ),
    nav_panel(
        title = "Economics",
        navset_card_tab(
            nav_panel(
                title = "Income",
                layout_sidebar(
                    sidebar = sidebar(
                        width = "33%", # does the same as validateCssUnit("33%")
                        title = "Household's Economic Status in Hampton Roads",
                        includeMarkdown("markdown/economics/income.Rmd")
                    ),
                    layout_column_wrap(
                        card(
                            card_header("Hampton Roads vs. Virginia Line Graph"),
                            card_body(plotOutput("medianTimeGraph")),
                            card_footer("Data Source: ACS 5 Year Estimates Table S1903")
                        )
                    )
                )
            ),
            nav_panel(
                title = "Homeownership", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = "33%",
                        title = "Homeownership in Hampton Roads",
                        includeMarkdown("markdown/economics/homeownership.Rmd"),
                    ),
                    layout_column_wrap(
                        width = 1,
                        card(
                            card_header("Black vs. Total Homeowners in Hampton Roads"),
                            card_body(leafletOutput("homeownership_map")),
                            card_footer("Data Source: ACS 5 Year Estimates Table S2505")
                        )
                    )
                )     
            ),
            nav_panel(
                title = "Labor Market", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("33%"),
                        title = "Labor Market Characteristics in Hampton Roads",
                        includeMarkdown("markdown/economics/labor_market.Rmd"),
                    ),
                    navset_card_tab(
                        title = "Labor Market Analysis",
                        nav_panel(
                            title = "Industry Employment",
                            value = "plot1",
                            h4(strong("Top Two Industry Sectors"), align = "center"),
                            selectInput(
                                "SectorEmploymentYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                    "2019", "2018", "2017", "2016", "2015",
                                    "2014", "2013", "2012", "2011", "2010"
                                )
                            ),
                            
                            layout_column_wrap(
                                width = 1,
                                heights_equal = "row",
                                fluidRow(
                                    plotlyOutput("sector_plot"),
                                    
                                )
                            ),
                            
                            card_footer("*Note: Data missing for some years. 
                                   Source: ACS 5 Year Estimate Table DP03")
                        ),
                        nav_panel(
                            title = "Unemployment Rate",
                            value = "plot2",
                            h4(strong("Unemployment Rate in Hampton Roads"), align = "center"),
                            layout_column_wrap(
                                width = 1,
                                heights_equal = "row",
                                fluidRow(
                                    plotOutput("unemployment_plot") 
                                ),
                                sliderInput(
                                    "UnemploymentRateSlider",
                                    "Select Year",
                                    value = 2019,
                                    min = 2010,
                                    max = 2019,
                                    sep = "",
                                    width = "100%",
                                    animate =
                                        animationOptions(interval = 1400)
                                ),
                            ),
                            card_footer("*Note: Red dotted line represents Virgina's unemployment rate. Missing data for the Black population in Mathews and Poquoson
                                   Data Source: ACS 5 Year Estimates Table S2301")
                        ),
                        nav_panel(
                            title = "Unemployment Rate Trends",
                            value = "plot3",
                            layout_column_wrap(
                                width = 1,
                                heights_equal = "row",
                                fluidRow(
                                    width = 12,
                                    height = 550,
                                    img(
                                        src = "updated_unemployment_plot.gif",
                                        height = "800",
                                        width = "1100"
                                    )
                                    
                                )
                            )
                        )
                    )
                )
            ),
            tabPanel(
                title = "Poverty", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("33%"),
                        title = "How does the poverty rate in Hampton Roads compare to all of Virginia?",
                        includeMarkdown("markdown/economics/poverty.Rmd"),
                    ),
                    navset_card_tab(
                        title = "Poverty Rates in Hampton Roads",
                        id = "poverty_tab",
                        nav_panel(
                            title = "Poverty Rates",
                            value = "plot1",
                            
                            h4(strong("Poverty Rates across Hampton Roads and Virginia"), align = "center"),
                            selectInput(
                                "PovertyYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c("2019", "2018", "2017", "2016", "2015", "2014",
                                            "2013", "2012")
                            ),
                            
                            layout_column_wrap(
                                width = 1,
                                heights_equal = "row",
                                fluidRow(
                                    plotOutput("pov_plot"),
                                    
                                )
                            ),
                            
                            card_footer( "Data Source: ACS 5 Year Estimates Table S1701")
                        ),
                        nav_panel(
                            title = "Poverty Rates across Localities",
                            value = "plot2",
                            h4(strong("Poverty Rates across Hampton Roads' Cities and Counties"), align = "center"),
                            layout_column_wrap(
                                width = 1,
                                heights_equal = "row",
                                
                                selectInput(
                                    "PovertyCountYearDrop",
                                    "Select Year:",
                                    width = "100%",
                                    choices = c("2019", "2018", "2017", "2016", "2015", "2014",
                                                "2013", "2012")
                                ),
                                layout_column_wrap(
                                    width = 1,
                                    heights_equal = "row",
                                    fluidRow(
                                        plotlyOutput("counties_pov", width = "100%"),
                                        
                                    )
                                ),
                            ),
                            card_footer("Data Source: ACS 5 Year Estimates Table S1701")
                        ),
                        nav_panel(
                            title = "Poverty Trends",
                            value = "plot3",
                            layout_column_wrap(
                                width = 1,
                                heights_equal = "row",
                                fluidRow(
                                    width = 12,
                                    height = 550,
                                    img(
                                        src = "poverty.gif",
                                        height = "800",
                                        width = "1100"
                                    )
                                    
                                )
                            )
                        )
                    )
                )
            ),
            nav_panel(
                title = "Health", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("33%"),
                        title = "Insurance Status In Hampton Roads",
                        includeMarkdown("markdown/economics/health.Rmd"),
                    ),
                    layout_column_wrap(
                      
                        h4(strong("Health Uninsured Rates' Cities and Counties"), align = "center"),
                        width = 1,
                        heights_equal = "row",
                        fluidRow(
                            plotlyOutput("uninsured_plot"),
                            sliderInput(
                                "UninsuredPctSlider",
                                "Select Year",
                                value = 2019,
                                min = 2012,
                                max = 2019,
                                sep = "",
                                width = "100%",
                                animate = animationOptions(interval = 1400)
                            ),
                        )
                    ),
                      card_footer("Data Source: ACS 5 Year Estimates Table S2701")
                    ,
                )
            ),
            nav_panel(
                title = "Veterans", 
                 layout_sidebar(
                     sidebar = sidebar(
                         width = validateCssUnit("33%"),
                         title = "Who has served in Hampton Roads?",
                         includeMarkdown("markdown/economics/veterans.Rmd"),
                     ),
                     layout_column_wrap(
                       h4(strong("Veteran Status in Hampton Roads"), align = "center"),
                       width = 1,
                       heights_equal = "row",
                       fluidRow(
                         leafletOutput("veteran_map"),
                         sliderInput(
                         "VeteranSlider",
                         "Select Year:",
                         value = 2019,
                         min = 2010,
                         max = 2019,
                         sep = "",
                         width = "100%",
                         animate = animationOptions(interval = 1200)
                       ),
                      )
 
                     ), card_footer("Data Source: ACS 5 Year Estimates Table S2101"),
                 )
            ),
            tabPanel(
                title = "Business", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("33%"),
                        title = "Black Business Trend Tracker",
                        includeMarkdown("markdown/economics/business.Rmd"),
                    ),
                    layout_column_wrap(
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("selectionType", "Choose your selection type:",
                                       choices = c("Select by State" = "state", 
                                                   "Select by Metropolitan Area" = "metro")),
                          uiOutput("dynamicSelectInput"),
                          selectInput("metrics", "Select Metrics:", 
                                      choices = c("Total Average Annual Pay (Total)" = "Total_Avg_Annual_Pay_Total",
                                                  "Total Average Annual Pay (Black Business)" = "Total_Avg_Annual_Pay_Black_Business",
                                                  "Total Average Employees (Total)" = "Total_Avg_Employees_Total",
                                                  "Total Average Employees (Black Business)" = "Total_Avg_Employees_Black_Business",
                                                  "Total Sum of Firms (Total)" = "Total_Sum_of_Firms_Total",
                                                  "Total Sum of Firms (Black Business)" = "Total_Sum_of_Firms_Black_Business",
                                                  "Average Pay Annual Per Employee (Total)" = "Pay_Annual_Per_Employee_Total",
                                                  "Average Pay Annual Per Employee (Black Business)" = "Pay_Annual_Per_Employee_Black_Business",
                                                  "Percent of Total Average Annual Pay (Black Business)" = "Percent_Total_Avg_Annual_Pay_BB",
                                                  "Percent of Total Average Employees (Black Business)" = "Percent_Total_Avg_Employees_BB",
                                                  "Percent of Total Sum of Firms (Black Business)" = "Percent_Total_Sum_of_Firms_BB")),
                        ),
                        mainPanel(
                          tabPanel("Plot and Table", 
                                   plotOutput("plot"),
                                   dataTableOutput("table")
                          )
                        )
                      )
                    )
                )
            ),
            tabPanel(
                title = "Household Well-being", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("33%"),
                        title = "Household in Hampton Roads",
                        includeMarkdown("markdown/economics/household_wellbeing.Rmd"),
                        textOutput("description_text")
                    ),
                    layout_column_wrap(
                        width = 12,
                        heights_equal = "row",
                        fluidRow(
                           h1(strong("Household Characteristics"), align = "center"),
                            selectInput(
                              "select_wellbeing",
                              "Select Indicator:",
                              width = "100%",
                              choices = c(
                                "Percent of Black Households Receiving Foodstamps/SNAP Benefits",
                                "Percent of Black County Migration",
                                "Percent of Black Population that uses car/truck/van to get to work",
                                "Percent of Black Population that uses public transportation to get to work",
                                "Percent of Black Households with a computer with broadband internet",
                                "Percent of Black Households without a computer"
                              )
                            ),
                           card(
                             
                            card_body(leafletOutput("wellbeing_maps")),
                            card_footer("Data Source: ACS 5 Year Estimates Tables: S0901, S2201, S0701, S1002, S1201, S0802, S2802")
                           )
                        ),
                    ),
                    
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