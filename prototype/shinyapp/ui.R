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
source("people_values.r")
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
                # create markdown files for any text you want to include
                # and call them using includeMarkdown() in the markdown library
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
        # rearrange tiles such that total pop, median age, black pop
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
                    condition = "input.edu_nav == 'Standardized Testing'",
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
                    )
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
                ),
                conditionalPanel(
                    condition = "input.edu_nav == 'Educators vs. Students'",
                    selectInput(
                        inputId = "edu_ratio_loc",
                        label = "Select location:",
                        selected = "Chesapeake",
                        choices = unique(st_data$division_name)
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
                            title = "Standardized Testing Results",
                            layout_sidebar(
                                sidebar = sidebar(
                                    width = "20%",
                                    selectInput(
                                        inputId = "st_year",
                                        label = "Select year:",
                                        # there has to be a better way to do this but
                                        # atp my brain has been fried to the point i'm
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
                layout_column_wrap(
                    uiOutput("edu_ratio1"), # black t-s ratio
                    uiOutput("edu_ratio2"), # white
                    uiOutput("edu_ratio3") # total
                ),
                card(
                    card_header("Teachers vs. Students per Location"),
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
                            card_footer("Source: ACS 5 Year Estimates Table S1903")
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
                            card_footer("Source: ACS 5 Year Estimates Table S2505")
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
                                            Source: ACS 5 Year Estimates Table DP03")
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
                        width = validateCssUnit("33%"),
                        title = "Who has served in Hampton Roads?",
                        includeMarkdown("markdown/economics/veterans.Rmd"),
                    ),
                    layout_column_wrap(
                        card(
                            card_header("Black Veterans in Hampton Roads"),
                            card_body(leafletOutput("veteran_map"), class = "p-0"),
                            p(),
                            card_body(
                                sliderInput(
                                    "VeteranSlider",
                                    "Select Year:",
                                    value = 2019,
                                    min = 2010,
                                    max = 2019,
                                    sep = "",
                                    width = "100%",
                                    animate = animationOptions(interval = 2400)
                                ),
                            ),
                            card_footer("Data Source: ACS 5 Year Estimates Table S2101")
                        )
                        
                    ), card_footer("Data Source: ACS 5 Year Estimates Table S2101"),
                )
            ),
            nav_panel(
                title = "Business", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Black Business Trend Tracker",
                        includeMarkdown("markdown/economics/business.Rmd"),
                    ),
                    card(
                        card_header("Black Business Trend Tracker"),
                        layout_sidebar(
                            sidebar = sidebar(
                                width = "25%",
                                radioButtons(
                                    "selectionType",
                                    "Choose your selection type:",
                                    choices = c("Select by State" = "state",
                                                "Select by Metropolitan Area" = "metro")),
                                uiOutput("dynamicSelectInput"),
                                selectInput(
                                    "metrics",
                                    "Select Metrics:",
                                    choices = c(
                                        "Total Average Annual Pay (Total)" =
                                            "Total_Avg_Annual_Pay_Total",
                                        "Total Average Annual Pay (Black Business)" =
                                            "Total_Avg_Annual_Pay_Black_Business",
                                        "Total Average Employees (Total)" =
                                            "Total_Avg_Employees_Total",
                                        "Total Average Employees (Black Business)" =
                                            "Total_Avg_Employees_Black_Business",
                                        "Total Sum of Firms (Total)" =
                                            "Total_Sum_of_Firms_Total",
                                        "Total Sum of Firms (Black Business)" =
                                            "Total_Sum_of_Firms_Black_Business",
                                        "Average Pay Annual Per Employee (Total)" =
                                            "Pay_Annual_Per_Employee_Total",
                                        "Average Pay Annual Per Employee (Black Business)" =
                                            "Pay_Annual_Per_Employee_Black_Business",
                                        "Percent of Total Average Annual Pay (Black Business)" =
                                            "Percent_Total_Avg_Annual_Pay_BB",
                                        "Percent of Total Average Employees (Black Business)" =
                                            "Percent_Total_Avg_Employees_BB",
                                        "Percent of Total Sum of Firms (Black Business)" =
                                            "Percent_Total_Sum_of_Firms_BB"
                                    )
                                )
                            ),
                            plotOutput("plot")
                        )
                    )
                )
            ),
            nav_panel(
                title = "Household Well-being", 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Household in Hampton Roads",
                        includeMarkdown("markdown/economics/household_wellbeing.Rmd"),
                        textOutput("description_text")
                    ),
                    layout_column_wrap(
                        width = 1,
                        heights_equal = "row",
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
                            card_header("Household Characteristics"),
                            card_body(leafletOutput("wellbeing_maps"), class = "p-0"),
                            card_footer("Data Source: ACS 5 Year Estimates Tables: S0901, 
                                                S2201, S0701, S1002, S1201, S0802, S2802")
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
                            card_header("Number of Internet Providers Per Zip Code"),
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
                            card_header("Number of Internet Providers Offering Speeds >= 100mbps"),
                            selectInput(
                                "select_quality",
                                "Select Year:",
                                width = "100%",
                                choices = c("2015", "2020")
                            ),
                            p(),
                            card_body(leafletOutput("internet_quality_maps"), class = "p-0"),
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
                            card_header("News Anchors"),
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
                    card(
                        card_header("Radio Stations"),
                        card_body(leafletOutput("radio"), class = "p-0"),
                        card_footer("Data Source: Collected from news stations")
                    )
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
    ## People & Values tab -------------------------------------------------------
    nav_panel(
        title = "People & Values",
        navset_card_underline(
            nav_panel(
                title = "Family Dynamic",
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Family Dynamic",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/people_values/family_dynamic.Rmd"),
                        withSpinner(textOutput("description_famtext"))
                    ),
                    layout_column_wrap(
                        card(
                            card_header(strong("Family Dynamics"), align = "center"),
                            selectInput(
                                "select_family",
                                "Select Indicator:",
                                width = "100%",
                                choices = c(
                                    "Percent of Black Children under 18 in Female Head of Household",
                                    "Percent of Married Black Population 15 years and over",
                                    "Percent of Black Grandparents who are Guardians")
                            ),
                            withSpinner(leafletOutput("family_maps")),
                            card_footer("Data Source: ACS 5 Year Estimates Tables: S0901, S2201, S0701, S1002, S1201, S0802, S2802")
                        )
                    )
                )
            ),
            nav_panel(
                title = "Religion",
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Religion",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/people_values/religion.Rmd"),
                    ),
                    layout_column_wrap(
                        card(
                            card_header(strong("Religion"), align = "center"),
                            selectInput(
                                "select_rel",
                                "Select Religion:",
                                width = "100%",
                                choices = c('Christianity', 'Judaism', 'Budhism','Hindu', 'Islam')
                            ),
                            withSpinner(leafletOutput("religion")), 
                            card_footer("Data Source: 2010 U.S. Religion Census: Religious Congregations & Membership Study")
                        )
                    )
                )
            ),
            nav_panel(
                title = "Financial Literacy",
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Financial Literacy Score",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/people_values/financial_lit.Rmd"),
                    ),
                    layout_column_wrap(
                        card(
                            card_header(strong("Financial Literacy Measured Using VA Survey Data"), align = "center"),
                            tabsetPanel(
                                tabPanel(
                                    "Financial Literacy Score",
                                    h4(strong("Scores by Race"), align = "center"),
                                    withSpinner(plotOutput("financial_literacy")), 
                                ),
                                tabPanel(
                                    "Credit Score",
                                    h4(strong("Credit Scores by Race"),align = "center"),
                                    withSpinner(plotlyOutput("credit_scores"))
                                ),
                                tabPanel(
                                    "Knowledge Gap",
                                    h4(strong( "Answered Do Not Know to Any Question by Race"), align = "center"),
                                    withSpinner(plotlyOutput("dont_know", height = "700px")),
                                )
                            ),
                            card_footer("Data Source: OECD/INFE National Survey")
                        )
                    )
                )
            ),
            nav_panel(
                title = "Financial Banks",
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Food Banks",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/people_values/food_banks.Rmd"),
                    ),
                    layout_column_wrap(
                        card(
                            card_header(strong("Food Banks"), align = "center"),
                            tabsetPanel(
                                tabPanel(
                                    "Food Bank Locations", 
                                    withSpinner(leafletOutput("foodBanksLeaflet")),
                                ),
                                tabPanel(
                                    "Food Banks in Localities",
                                    withSpinner(plotlyOutput("numFoodBanksLocalities")), 
                                )
                            ),card_footer("Data Source: Google Maps API places")
                        )
                    )
                )
            ), 
            nav_panel(
                title = "Food Insecurity",
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Food Insecurity",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/people_values/food_insecurity.Rmd"),
                    ),           
                    layout_column_wrap(
                        card(
                            card_header(strong("Food Insecurity"), align = "center"),
                            tabsetPanel(
                                tabPanel(
                                    "Poverty Rates", 
                                    withSpinner(leafletOutput("povertyRateMap")),
                                ),
                                tabPanel(
                                    "African Population Low Access Markets (Half Mile)",
                                    withSpinner(leafletOutput("lowAccessAF")), 
                                ), 
                                tabPanel(
                                    "African Population Low Access Markets (One Mile)",
                                    withSpinner(leafletOutput("lowAccessAF1")), 
                                )
                            ),card_footer("Data Source: US Department of Agriculture")
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
