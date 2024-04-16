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
source("economics.r")
source("media.r")

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
                width = validateCssUnit("25%"),
                title = "Demographic Characteristics",
                includeMarkdown("markdown/sodem/sodem_characteristics.md")
            ),
            layout_column_wrap(
                width = 1,
                heights_equal = "row",
                layout_column_wrap(
                    width = 1/3,
                    uiOutput("sodem_vb1"),
                    uiOutput("sodem_vb2"),
                    uiOutput("sodem_vb3")
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
        navset_card_underline(
            id = "edu_nav",
            sidebar = sidebar(
                title = "Location & Race Comparison",
                width = "20%", # sidebar takes up x% of page
                conditionalPanel(
                    condition = "input.edu_nav !== 'Educational Attainment (Graduation Rate)'",
                    selectInput(
                        inputId = "edu_loc",
                        label = "Select location:",
                        selected = "Chesapeake",
                        choices = unique(st_data$division_name)
                    ),
                    checkboxGroupInput(
                        inputId = "edu_races",
                        label = "Select races:",
                        choices = c("Black" = "Black",
                                    "White" = "White",
                                    "Asian" = "Asian",
                                    "Hispanic" = "Hispanic"),
                        selected = "Black"
                    ),
                    p(),
                    p("Use this sidebar to compare races against each other in each location! NOTE: 
                      There are some locations in Hampton Roads where there simply isn't enough data 
                      to be recorded.", style = "text-align: justify")
                ),
                conditionalPanel(
                    condition = "input.edu_nav == 'Educational Attainment (Graduation Rate)'",
                    radioButtons(
                        inputId = "grad_race",
                        label = "Select to view:",
                        choices = c("All Students" = "All Students",
                                    "Black" = "Black",
                                    "White" = "White",
                                    "Asian" = "Asian",
                                    "Hispanic" = "Hispanic"),
                        selected = "All Students"
                    )
                )
            ),
            nav_panel(
                title = "Standardized Testing",
                layout_column_wrap(
                    width = 1,
                    heights_equal = "row",
                    layout_column_wrap(
                        width = 1/3,
                        heights_equal = "row",
                        uiOutput("edu_vb1"),
                        uiOutput("edu_vb2"),
                        uiOutput("edu_vb3")
                    ),
                    navset_card_pill(
                        nav_panel(
                            title = "2022-2023 Testing Results",
                            layout_sidebar(
                                sidebar = sidebar(
                                    width = "20%",
                                    selectInput(
                                        inputId = "st_year",
                                        label = "Select year:",
                                        # there has to be a better way to do this but
                                        # atp my brain has been fried to the point that i'm
                                        # seeing double
                                        choices = c("2022-2023" = "2022-2023_pass_rate",
                                                    "2021-2022" = "2021-2022_pass_rate",
                                                    "2020-2021" = "2020-2021_pass_rate",
                                                    "2018-2019" = "2018-2019_pass_rate",
                                                    "2017-2018" = "2017-2018_pass_rate",
                                                    "2016-2017" = "2016-2017_pass_rate",
                                                    "2015-2016" = "2015-2016_pass_rate",
                                                    "2014-2015" = "2014-2015_pass_rate",
                                                    "2013-2014" = "2013-2014_pass_rate"),
                                        selected = "2022-2023"
                                    )
                                ),
                                plotlyOutput("radar_plot"),
                            ),
                        ),
                        nav_panel(
                            title = "Testing Results Over Time",
                            plotOutput("lollipop_plot")
                        ),
                        card_footer("Source: VDOE Annual Pass Rates (Division Subject Area)")
                    )
                )
                
            ),
            nav_panel(
                title = "Educators vs. Students",
                card(
                    # maybe include teacher-student value boxes here...
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
            nav_panel(
                title = "Educational Attainment (Graduation Rate)",
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
    ),
    nav_panel(
        title = "Economics",
        navset_card_underline(
            nav_panel(
                title = "Income",
                layout_sidebar(
                    sidebar = sidebar(
                        width = "25%", # does the same as validateCssUnit("25%")
                        title = "Household's Economic Status in Hampton Roads",
                        includeMarkdown("markdown/economics/income.Rmd")
                    ),
                    layout_column_wrap(
                        card(
                            card_header("Hampton Roads vs. Virginia Line Graph"),
                            card_body(plotOutput("medianTimeGraph")),
                            card_footer("Source: ???")
                        )
                    )
                )
                # include data table here?
            ),
            nav_panel(
                title = "Homeownership", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = "25%",
                        title = "Homeownership in Hampton Roads",
                        includeMarkdown("markdown/economics/homeownership.Rmd"),
                    ),
                    layout_column_wrap(
                        width = 1,
                        card(
                            # this is fucking BUGGED. idk what happened here but
                            # it is absurdly slow. will try fixing if not it's up to you
                            # fall 2024 team :)
                            card_header("Black vs. Total Homeowners in Hampton Roads"),
                            card_body(leafletOutput("homeownership_map"), class = "p-0"),
                            card_footer("Source: ???")
                        )
                    )
                )     
            ),
            nav_panel(
                title = "Labor Market", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Labor Market Characteristics in Hampton Roads",
                        includeMarkdown("markdown/economics/labor_market.Rmd"),
                    ),
                    # completely redo. graphs aren't normalized and some graphs straight up
                    # don't work
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
                            card_footer("Note: Data missing for some years. 
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
            nav_panel(
                # same as labor market
                title = "Poverty", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
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
                        width = validateCssUnit("25%"),
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
                    )
                )
            ),
            nav_panel(
                title = "Veterans", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Veteran Population in Hampton Roads",
                        includeMarkdown("markdown/economics/veterans.Rmd"),
                    ),
                    layout_column_wrap(
                        width = 1,
                        heights_equal = "row",
                        fluidRow(
                            plotOutput("veteransGraph") 
                        )
                    )
                )
            ),
            nav_panel(
                title = "Business", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Business Environment in Hampton Roads",
                        includeMarkdown("markdown/economics/business.Rmd"),
                    ),
                    layout_column_wrap(
                        width = 1,
                        heights_equal = "row",
                        fluidRow(
                            plotOutput("businessGraph") 
                        )
                    )
                )
            ),
            nav_panel(
                title = "Household Well-being", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Well-being of Households in Hampton Roads",
                        includeMarkdown("markdown/economics/household_wellbeing.Rmd"),
                    ),
                    layout_column_wrap(
                        width = 12,
                        heights_equal = "row",
                        fluidRow(
                            plotOutput("householdWellbeingGraph") 
                        )
                    )
                )
            )
        )
    ),
    ## Media / Entertainment tab-------------------------------------------------------
    nav_panel(
      title = "Media & Entertainment",
      navset_card_underline(
          nav_panel(
              title = "Internet Coverage", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Coverage of Broadband Internet in Hampton Roads",
                      class = "scrollable-sidebar",
                      width = "25%",
                      includeMarkdown("markdown/media/internet_coverage.Rmd")
                  ),
                  layout_column_wrap(
                      card(
                          # remake slider bins rather than continuous
                          # create sidebar with radio buttons - 2015 & 2020
                          card_header(strong("Number of Internet Providers Per Zip Code"), align = "center"),
                          selectInput(
                              "select_coverage",
                              "Select Year:",
                              width = "50%",
                              choices = c("2015", "2020")
                          ),
                          p(),
                          card_body(leafletOutput("internet_coverage_maps"), class = "p-0"),
                          card_footer("Data Source: Sourced from BroadbandNow(2022)")
                      )
                  )
              )
          ),
          nav_panel(
              title = "Internet Quality", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Quality of Broadband Internet in Hampton Roads",
                      class = "scrollable-sidebar",
                      width = validateCssUnit("25%"),
                      includeMarkdown("markdown/media/internet_quality.Rmd"),
                  ),
                  layout_column_wrap(
                      card(
                          card_header(strong("Number of Internet Providers Offering Speeds >= 100mbps"), align = "center"),
                          selectInput(
                              "select_quality",
                              "Select Year:",
                              width = "100%",
                              choices = c("2015", "2020")
                          ),
                          p(),
                          card_body(leafletOutput("internet_quality_maps"), style = "p-0"),
                          card_footer("Data Source: Sourced from BroadbandNow(2022)")
                      )
                  )
              )
          ),
          nav_panel(
              title = "News Anchors", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "News Anchors Across Hampton Roads",
                      class = "scrollable-sidebar",
                      width = validateCssUnit("25%"),
                      includeMarkdown("markdown/media/news_anchor.Rmd"),
                  ),
                  layout_column_wrap(
                      card(
                          card_header(strong("News Anchors"), align = "center"),
                          withSpinner(plotOutput("anch_plots")),
                          card_footer("Data Source: Manually Collected")
                      )
                  )
              )
          ),
          nav_panel(
              title = "Radio Stations", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Radio Stations Across Hampton Roads",
                      class = "scrollable-sidebar",
                      width = validateCssUnit("25%"),
                      includeMarkdown("markdown/media/radio_station.Rmd"),
                  ),
                  layout_column_wrap(
                      fluidPage(
                          h1(strong("Radio Stations"), align = "center"),
                          withSpinner(leafletOutput("radio")),)
                  ),
                  card_footer("Data Source: Collected from new station")
              )
          ),
          nav_panel(
              title = "Headquarter Locations", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Headquarters in Hampton Roads",
                      class = "scrollable-sidebar",
                      width = validateCssUnit("25%"),
                      includeMarkdown("markdown/media/headquarter_location.Rmd"),
                  ),
                  layout_column_wrap(
                      card(
                          card_header(strong("Headquarters"), align = "center"),
                          withSpinner(plotOutput("headquarters_graph")),
                          card_footer("Data Source: Newspaper Headquarter Locations")
                      )
                  )
              )
          ),
          nav_panel(
              title = "Ratio of Sentiment Over Time", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Sentiment Analysis in Hampton Roads",
                      class = "scrollable-sidebar",
                      width = validateCssUnit("25%"),
                      includeMarkdown("markdown/media/radio_sentiment.Rmd"),
                  ),
                  layout_column_wrap(
                      card(
                          card_header(strong("Sentiment Terminology Over Time"), align = "center"),
                          selectInput(
                              "select_sent_year",
                              "Select Year:",
                              width = "100%",
                              choices = c(
                                  "2022", "2021","2020", "2019", "2018","2017", "2016",
                                  "2015", "2014", "2012", "2010"
                              )),
                          mainPanel(plotlyOutput("sentiment_by_year")),
                          card_footer("Derived from Bag of Words Text Analytics Algorithm")
                      )
                  )
              )
          ),
          nav_panel(
              title = "Diversity", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Ratio of Diversity to Positive & Negative Bags",
                      class = "scrollable-sidebar",
                      width = validateCssUnit("25%"),
                      includeMarkdown("markdown/media/diversity.Rmd"),
                  ),
                  layout_column_wrap(
                      card(
                          card_header(strong("Diversity Bag"), align = "center"),
                          withSpinner(plotOutput("div_by_pos_and_neg")),
                          card_footer("Derived from Bag of Words Text Analytics Algorithm")
                      )
                  )
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