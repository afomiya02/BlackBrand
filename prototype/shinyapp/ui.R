library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(tidyverse)

source("code/sodem.r")
source("code/education.r")
source("code/economics.r")
source("code/media.r")
source("code/politics_justice.r")
source("code/people_values.r")
source("code/feedback.r")

ui <- page_navbar(
    title = img(src="logo_WIDE.png"),
    selected = "overview",
    theme = bs_theme(preset = "journal"),
    useShinyjs(),
    nav_menu(
        # Overview Section
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
            # Background Sections
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
            ),
            # Give feedback button
            tags$head(tags$style(
                HTML(
                    "
                    .btn-highlight {
                    background-color: #4CAF50;
                    color: white;
                    }
                    .rating-buttons {
                    margin-left: 50px;
                    padding-bottom: 10px;}"
                )
            )),
            tags$div(style = "text-align: right;",
                     # This aligns the button to the right
                     actionButton("show_feedback", "Give Feedback", class = "btn-primary"))
        ),
        # Data & Methodology Section
        nav_panel(
            title = "Data & Methodology",
            navset_card_underline(
                nav_panel(
                    title = strong("Datasets"),
                    fluidRow(
                        style = "margin: 2px",
                        align = "center",
                        p(h1("Datasets Used Inside the Dashboard"), br(),
                          h4("NOTE: Cards in orange are sources we have updated to 2022 so far"))
                    ),
                    layout_column_wrap(
                        width = 1/2,
                        heights_equal = "all",
                        fill = FALSE,
                        card(
                            card_header(class = "bg-red", "American Community Survey"),
                            layout_sidebar(
                                sidebar = sidebar(img(src = 'acs.png')),
                                includeMarkdown("markdown/data_methodology/data1.md")
                            ),
                        ),
                        card(
                            card_header(class = "bg-red", "Virginia Department of Education"),
                            layout_sidebar(
                                fillable = TRUE,
                                open = FALSE,
                                sidebar = sidebar(img(src = "doe.png")),
                                includeMarkdown("markdown/data_methodology/data5.md")
                            )
                        ),
                        card(
                            card_header(class = "bg-dark", "BroadbandNow"),
                            layout_sidebar(
                                fillable = TRUE,
                                open = FALSE,
                                sidebar = sidebar(img(src = 'broadbandnow.png')),
                                includeMarkdown("markdown/data_methodology/data2.md")
                            )
                        ),
                        card(
                            card_header(class = "bg-dark", "Zillow"),
                            layout_sidebar(
                                fillable = TRUE,
                                open = FALSE,
                                sidebar = sidebar(img(src = "zillow.svg")),
                                includeMarkdown("markdown/data_methodology/data3.md")
                            ),
                        ),
                        card(
                            card_header(class = "bg-dark", "Kids COUNT"),
                            layout_sidebar(
                                fillable = TRUE,
                                open = FALSE,
                                sidebar = sidebar(img(src = "kidscount_datacenter.png")),
                                includeMarkdown("markdown/data_methodology/data4.md")
                            )
                        ),
                        card(
                            card_header(class = "bg-dark", "OECD"),
                            layout_sidebar(
                                fillable = TRUE,
                                open = FALSE,
                                sidebar = sidebar(img(src = "oecd.png")),
                                includeMarkdown("markdown/data_methodology/data6.md")
                            )
                        ),
                        card(
                            card_header(class = "bg-dark", "Google Maps API"),
                            layout_sidebar(
                                fillable = TRUE,
                                open = FALSE,
                                sidebar = sidebar(img(src = "googleMapLogo.png")),
                                includeMarkdown("markdown/data_methodology/data7.md")
                            )
                        ),
                        card(
                            card_header(class = "bg-dark", "Census Reporter"),
                            layout_sidebar(
                                fillable = TRUE,
                                open = FALSE,
                                sidebar = sidebar(img(src = "censusreporter.png")),
                                includeMarkdown("markdown/data_methodology/data8.md")
                            )
                        )
                    )
                ),
                nav_panel(
                    title = strong("Methodology"),
                    fluidRow(
                        style = "margin: 6px;",
                        h1(strong("5 Pillars of Methodology"), align = "center"),
                        p("", style = "padding-top:10px;"),
                        column(
                            width = 4,
                            strong("Education: "),
                            includeMarkdown("markdown/data_methodology/methodology1.md"),
                            img(src = 'education.png', align = "center",
                                height = "300px", width = "400px"),
                            p(tags$small("[1] Anna J. Egalitea, A.J. , Kisida B., & Winters, M.A. 
                                         (2015), Economics of Education Review, 45, 44-52.")),
                            p(tags$small("[2] Noltemeyer, A. L., Ward, R. M., & Mcloughlin, C. 
                                         (2015). Relationship between school suspension and student 
                                         outcomes: A meta-analysis. School Psychology Review, 44(2), 
                                         224-240.")),
                        ),
                        column(
                            width = 4,
                            strong("Policy & Justice: "),
                            includeMarkdown("markdown/data_methodology/methodology2.md"),
                            strong("People & Values: "),
                            includeMarkdown("markdown/data_methodology/methodology3.md"),
                            strong("Media & Entertainment:"),
                            includeMarkdown("markdown/data_methodology/methodology4.md"),
                            uiOutput("tab")
                        ),
                        column(
                            width = 4,
                            strong("Economics: "),
                            includeMarkdown("markdown/data_methodology/methodology5.md"),
                            p(),
                            img(src = 'economic.png', align = "center", 
                                height = "300px", width = "400px"),
                            p(),
                            p(),
                            includeMarkdown("markdown/data_methodology/methodology6.md"),
                            p(tags$small("[1] Butler, S. M., Beach, W. W., & Winfree, P. L. (2008). 
                                         Pathways to economic mobility: Key indicators. 
                                         Economic mobility project."))
                        )
                    )
                )
            )
        ),
        # meet the team
        nav_panel(
            title = "Meet the Team",
        )
    ),
    ## Sociodemographics tab -------------------------------------------------------
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
    ## Education tab -------------------------------------------------------
    nav_panel(
        title = "Education",
        navset_card_underline(
            id = "edu_nav",
            sidebar = sidebar(
                title = "Location & Race Comparison",
                width = "20%", # sidebar takes up x% of page
                # conditional panels that change depending on tab we're on
                conditionalPanel(
                    condition = "input.edu_nav == '<strong>Standardized Testing</strong>'",
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
                    condition = "input.edu_nav == 
                        '<strong>Educational Attainment (Graduation Rate)</strong>'",
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
                    condition = "input.edu_nav == '<strong>Educators vs. Students</strong>'",
                    selectInput(
                        inputId = "edu_ratio_loc",
                        label = "Select location:",
                        selected = "Chesapeake",
                        choices = unique(st_data$division_name)
                    )
                )
            ),
            nav_panel(
                title = strong("Standardized Testing"),
                layout_column_wrap(
                    width = 1,
                    heights_equal = "row",
                    layout_column_wrap(
                        width = 1/3,
                        heights_equal = "all",
                        uiOutput("edu_vb1"),
                        uiOutput("edu_vb2"),
                        uiOutput("edu_vb3")
                    ),
                    navset_card_pill(
                        height = validateCssUnit("1000px"),
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
                title = strong("Educators vs. Students"),
                layout_column_wrap(
                    width = 1,
                    heights = "row",
                    layout_column_wrap(
                        height = "15%",
                        width = 1/3,
                        uiOutput("edu_ratio1"), # black t-s ratio
                        uiOutput("edu_ratio2"), # white
                        uiOutput("edu_ratio3") # total
                    ),
                ),
                card(
                    full_screen = TRUE,
                    height = "85%",
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
                title = strong("Educational Attainment (Graduation Rate)"),
                card(
                    full_screen = TRUE,
                    card_header("Graduation Rate Choropleth Map"),
                    card_body(class = "p-0", leafletOutput("cohort_choropleth_map")),
                    card_footer("Source: VDOE Cohort Graduation Build-a-Table")
                ),
                p(), # spacer
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
        )
    ),
    
    ## Economics tab-------------------------------------------------------
    nav_panel(
        title = "Economics",
        navset_card_underline(
            nav_panel(
                title = strong("Income"),
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
            ),
            nav_panel(
                title = strong("Homeownership"), 
                layout_sidebar(
                    sidebar = sidebar(
                        width = "25%",
                        title = "Homeownership in Hampton Roads",
                        includeMarkdown("markdown/economics/homeownership.Rmd"),
                    ),
                    layout_column_wrap(
                        width = 1,
                        card(
                            card_header("Black vs. Total Homeowners in Hampton Roads"),
                            card_body(leafletOutput("homeownership_map"), class = "p-0"),
                            card_footer("Source: ACS 5 Year Estimates Table S2505")
                        )
                    )
                )     
            ),
            nav_panel(
                title = strong("Labor Market"), 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Labor Market Characteristics in Hampton Roads",
                        includeMarkdown("markdown/economics/labor_market.Rmd"),
                    ),
                    navset_card_pill(
                        title = "Labor Market Analysis",
                        nav_panel(
                            title = "Industry Employment",
                            value = "plot1",
                            layout_column_wrap(
                                width = 1,
                                heights_equal = "row",
                                card(
                                    height = "1000px",
                                    card_header("Top Two Employment Sectors"),
                                    card_body(plotlyOutput("sector_plot")),
                                    card_footer("Note: Data missing for some years. 
                                            Source: ACS 5 Year Estimates Table DP03")
                                ),
                                sliderInput(
                                    "SectorEmploymentYearDrop",
                                    "Select Year:",
                                    value = 2019,
                                    min = 2010,
                                    max = 2019,
                                    sep = "",
                                    width = "100%",
                                    animate =
                                        animationOptions(interval = 2100)
                                ),
                            ),
                            
                        ),
                        nav_panel(
                            title = "Unemployment Rate",
                            value = "plot2",
                            layout_column_wrap(
                                width = 1,
                                heights_equal = "row",
                                card(
                                    height = "1000px",
                                    card_header("Unemployment Rate in Hampton Roads"),
                                    card_body(plotlyOutput("unemployment_plot")),
                                    card_footer("*Note: Red dotted line represents Virgina's 
                                    unemployment rate. Missing data for the Black population in 
                                    Mathews and Poquoson. Data Source: ACS 5 Year Estimates Table S2301")
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
                                        animationOptions(interval = 2100)
                                )
                            )
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
                # same issue as labor market
                title = strong("Poverty"), 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "How does the poverty rate in Hampton Roads compare to all of Virginia?",
                        includeMarkdown("markdown/economics/poverty.Rmd"),
                    ),
                    navset_card_pill(
                        title = "Poverty in Hampton Roads",
                        id = "poverty_tab",
                        nav_panel(
                            title = "Poverty Rates",
                            value = "plot1",
                            card(
                                height = "1000px",
                                card_header("Poverty Rates across Hampton Roads and Virginia"),
                                card_body(plotOutput("pov_plot")),
                                card_footer("Data Source: ACS 5 Year Estimates Table S1701")
                            ),
                            sliderInput(
                                "PovertyYearDrop",
                                "Select Year:",
                                value = 2019,
                                min = 2012,
                                max = 2019,
                                sep = "",
                                width = "100%",
                                animate =
                                    animationOptions(interval = 2100)
                            ),
                        ),
                        nav_panel(
                            title = "Poverty Rates across Hampton Roads",
                            value = "plot2",
                            card(
                                height = "1000px",
                                card_header("Poverty Rates across Hampton Roads"),
                                card_body(plotlyOutput("counties_pov", width = "100%")),
                                card_footer("Data Source: ACS 5 Year Estimates Table S1701")
                            ),
                            sliderInput(
                                "PovertyCountYearDrop",
                                "Select Year:",
                                value = 2019,
                                min = 2012,
                                max = 2019,
                                sep = "",
                                width = "100%",
                                animate =
                                    animationOptions(interval = 2100)
                            )
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
                title = strong("Health"), 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Insurance Status In Hampton Roads",
                        includeMarkdown("markdown/economics/health.Rmd"),
                    ),
                    layout_column_wrap(
                        width = 1,
                        heights_equal = "row",
                        card(
                            height = "1000px",
                            card_header("Health Uninsured Rates' Cities and Counties"),
                            card_body(plotlyOutput("uninsured_plot")),
                            card_footer("Note: Poquoson Black data missing. Data Source: 
                                    ACS 5 Year Estimates Table S2701S")
                        ),
                        sliderInput(
                            "UninsuredPctSlider",
                            "Select Year",
                            value = 2019,
                            min = 2012,
                            max = 2019,
                            sep = "",
                            width = "100%",
                            animate = animationOptions(interval = 2100)
                        ),
                    )
                )
            ),
            nav_panel(
                title = strong("Veterans"), 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Who has served in Hampton Roads?",
                        includeMarkdown("markdown/economics/veterans.Rmd"),
                    ),
                    layout_column_wrap(
                        width = 1,
                        heights_equal = "row",
                        card(
                            height = "1000px",
                            card_header("Black Veterans in Hampton Roads"),
                            card_body(leafletOutput("veteran_map"), class = "p-0"),
                            card_footer("Data Source: ACS 5 Year Estimates Table S2101")
                        ),
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
                )
            ),
            nav_panel(
                title = strong("Business"), 
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
                title = strong("Household Well-being"), 
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
                            height = "1000px",
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
                title = strong("Internet Coverage"), 
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Coverage of Broadband Internet in Hampton Roads",
                        class = "scrollable-sidebar",
                        width = "25%",
                        includeMarkdown("markdown/media/internet_coverage.Rmd")
                    ),
                    selectInput(
                        "select_coverage",
                        "Select Year:",
                        width = "50%",
                        choices = c("2015", "2020")
                    ),
                    card(
                        # remake slider bins rather than continuous
                        # create sidebar with radio buttons - 2015 & 2020
                        card_header("Number of Internet Providers Per Zip Code"),
                        card_body(leafletOutput("internet_coverage_maps"), class = "p-0"),
                        card_footer("Data Source: Sourced from BroadbandNow(2022)")
                    )
                )
            ),
            nav_panel(
                title = strong("Internet Quality"), 
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Quality of Broadband Internet in Hampton Roads",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/media/internet_quality.Rmd"),
                    ),
                    selectInput(
                        "select_quality",
                        "Select Year:",
                        width = "100%",
                        choices = c("2015", "2020")
                    ),
                    card(
                        card_header("Number of Internet Providers Offering Speeds >= 100mbps"),
                        card_body(leafletOutput("internet_quality_maps"), class = "p-0"),
                        card_footer("Data Source: Sourced from BroadbandNow(2022)")
                    )
                )
            ),
            nav_panel(
                title = strong("News Anchors"), 
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
                            plotOutput("anch_plots"),
                            card_footer("Data Source: Manually Collected")
                        )
                    )
                )
            ),
            nav_panel(
                title = strong("Radio Stations"), 
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
                title = strong("Headquarter Locations"), 
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Headquarters in Hampton Roads",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/media/headquarter_location.Rmd"),
                    ),
                    card(
                        card_header("Headquarter Locations Across Major Cities in Virginia"),
                        card_body(plotOutput("headquarters_graph")),
                        card_footer("Data Source: Newspaper Headquarter Locations")
                    )
                )
            ),
            nav_panel(
                title = strong("Sentiment"), 
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Sentiment Analysis in Hampton Roads",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/media/radio_sentiment.Rmd"),
                    ),
                    layout_column_wrap(
                        card(
                            card_header("Sentiment Terminology Over Time"),
                            card_body(plotlyOutput("sentiment_by_year")),
                            card_footer("Derived from Bag of Words Text Analytics Algorithm")
                        )
                    ),
                    # TODO fix this slider -- this shit ain't workin
                    sliderInput(
                        inputId = "select_sent_year",
                        label = "Select Year:",
                        value = 2022,
                        min = 2010,
                        max = 2022,
                        round = TRUE,
                        step = 1,
                        sep = "",
                        width = "100%",
                        animate = animationOptions(interval = 2400)
                    ),
                )
            ),
            nav_panel(
                title = strong("Diversity"), 
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Ratio of Diversity to Positive & Negative Bags",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/media/diversity.Rmd"),
                    ),
                    card(
                        card_header("Diversity Bag of Words"),
                        card_body(plotOutput("div_by_pos_and_neg")),
                        card_footer("Derived from Bag of Words Text Analytics Algorithm")
                    )
                )
            )
        )
    ),
    ### Politics/Justice Tab -------------------------------------------------------------
    nav_panel(
        title = "Politics & Justice",
        navset_card_underline(
            nav_panel(
                title = strong("Traffic Stops"), 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Traffic Stops",
                        includeMarkdown("markdown/politics/traffic_stops.Rmd"),
                    ),
                    
                    navset_card_pill(
                        title = "Traffic Stops",
                        nav_panel(
                            title = "Traffic Stops by Race",
                            value = "plot1",
                            card(
                                card_header("Demographics of Traffic Stops in Hampton Roads"),
                                card_body(plotOutput("trafficRace")),
                            )
                        ),
                        nav_panel(
                            title = "Race and Jurisdiction",
                            value = "plot2",
                            card(
                                card_header("Demographics of Traffic Stops in Hampton Roads"),
                                card_body(plotOutput("jurisdiction"))
                            )
                        ),
                        nav_panel(
                            title = "Toggle Jurisdictions",
                            value = "plot2",
                            selectInput(
                                "select_stop",
                                "Select Hampton Roads County:",
                                width = "100%",
                                choices = c("CHESAPEAKE", "FRANKLIN CITY", "HAMPTON",
                                            "NEWPORT NEWS", "NORFOLK", "POQUOSON",
                                            "PORTSMOUTH", "SUFFOLK", "VIRGINIA BEACH",
                                            "WILLIAMSBURG", "GLOUCESTER CO", "ISLE OF WIGHT CO",
                                            "JAMES CITY CO", "MATHEWS CO", "SOUTHAMPTON CO",
                                            "YORK CO")
                            ),
                            card(
                                card_header("Traffic Stops Across Each Locality"),
                                card_body(plotOutput("jurisdiction2"))
                            )
                        )
                    )
                )
            ),
            nav_panel(
                title = strong("City Council Demographics"), 
                layout_sidebar(
                    sidebar = sidebar(
                        width = "25%",
                        title = "City Council Demographics",
                        includeMarkdown("markdown/politics/city_council_demographics.Rmd"),
                    ),
                    card(
                        card_header("City Council Demographics by Race and Gender, 2021"),
                        layout_column_wrap(
                            width = 1,
                            plotOutput("cityd"),
                            plotOutput("cityd2")
                        )
                    )
                )  
            ),
            nav_panel(
                title = strong("Incarceration Trends"), 
                layout_sidebar(
                    sidebar = sidebar(
                        width = validateCssUnit("25%"),
                        title = "Incarceration Trends",
                        includeMarkdown("markdown/politics/incarceration_trends.Rmd"),
                    ),
                    
                    navset_card_pill(
                        title = "Incarcerations Across Hampton Roads",
                        nav_panel(
                            title = "Jail Rates",
                            value = "plot1",
                            selectInput(
                                "select_jailChoice",
                                "Select:",
                                width = "100%",
                                choices = c(
                                    "Virginia",
                                    "Hampton Roads"
                                )
                            ),
                            card(
                                card_header("Jail Rates Across Virginia and Hampton Roads"),
                                card_body(plotOutput("jail")),
                                card_footer("Source: Vera Institute of Justice")
                            )
                            
                        ),
                        nav_panel(
                            title = "Jail & Pop Demographics",
                            value = "plot2",
                            selectInput(
                                "select_pieYear",
                                "Select Year:",
                                width = "100%",
                                choices = c("2014", "2015", "2016", "2017", "2018")
                            ),
                            card(
                                card_header("Jail and Total Population Hampton Roads Demographics"),
                                layout_column_wrap(
                                    width = 1/2,
                                    heights_equal = "row",
                                    highchartOutput("pie_plots1"),
                                    highchartOutput("pie_plots2"),
                                    h4("Jail Population", align = "center"),
                                    h4("Total Population", align = "center")
                                ),
                                card_footer("Source: Vera Institute of Justice")
                            )
                        ),
                        nav_panel(
                            title = "Prison Rates",
                            value = "plot3",
                            selectInput(
                                "select_prisonYear",
                                "Select Year:",
                                width = "100%",
                                choices = c("2013", "2012","2011", "2010", "2009")
                            ),
                            card(
                                card_header("Admission Rates per 100k People"),
                                card_body(leafletOutput("prison")),
                                card_footer("Source: Vera Institute of Justice")
                            )
                        )
                    )
                )
            ),
            nav_panel(
                title = strong("Gentrification"), 
                layout_sidebar(
                    sidebar = sidebar(
                        width = "25%",
                        title = "Analyzing Gentrification in Hampton Roads",
                        includeMarkdown("markdown/politics/gentrification.Rmd"),
                    ),
                    card(
                        card_header("Median Home Value & Percent Populations"),
                        card_body(leafletOutput("map_homevalues"), class = "p-0"),
                        card_footer("Data Source: Manually collected from Zillow & Census Reporter (2022)")
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
                title = strong("Family Dynamics"),
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Family Dynamic",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/people_values/family_dynamic.Rmd"),
                        withSpinner(textOutput("description_famtext"))
                    ),
                    layout_column_wrap(
                        width = 1,
                        heights_equal = "row",
                        selectInput(
                            "select_family",
                            "Select Indicator:",
                            width = "100%",
                            choices = c(
                                "Percent of Black Children under 18 in Female Head of Household",
                                "Percent of Married Black Population 15 years and over",
                                "Percent of Black Grandparents who are Guardians")
                        ),
                        card(
                            height = "1000px",
                            card_header("Family Dynamics Choropleth Maps"),
                            card_body(leafletOutput("family_maps"), class = "p-0"),
                            card_footer("Data Source: ACS 5 Year Estimates Tables: S0901, S2201, S0701, S1002, S1201, S0802, S2802")
                        )
                    )
                )
            ),
            nav_panel(
                title = strong("Religion"),
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Religion",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/people_values/religion.Rmd"),
                    ),
                    layout_column_wrap(
                        width = 1,
                        heights_equal = "row",
                        selectInput(
                            "select_rel",
                            "Select Religion:",
                            width = "100%",
                            choices = c('Christianity', 'Judaism', 'Budhism','Hindu', 'Islam')
                        ),
                        card(
                            height = "1000px",
                            card_header("Percent of Population Practicing Their Religion in 2010"),
                            card_body(leafletOutput("religion"), class = "p-0"), 
                            card_footer("Data Source: 2010 U.S. Religion Census: 
                                        Religious Congregations & Membership Study")
                        )
                    )
                )
            ),
            nav_panel(
                title = strong("Financial Literacy"),
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
                title = strong("Food Banks"),
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Food Banks",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/people_values/food_banks.Rmd"),
                    ),
                    navset_card_pill(
                        title = "Food Banks",
                        wrapper = card_body(),
                        nav_panel(
                            title = "Food Bank Locations in Hampton Roads",
                            leafletOutput("foodBanksLeaflet")
                        ),
                        nav_panel(
                            title = "Number of Food Banks in Each Location",
                            plotlyOutput("numFoodBanksLocalities")
                        ),
                        card_footer("Data Source: Google Maps API places")
                    )
                )
            ), 
            nav_panel(
                title = strong("Food Insecurity"),
                layout_sidebar(
                    sidebar = sidebar(
                        title = "Food Insecurity",
                        class = "scrollable-sidebar",
                        width = validateCssUnit("25%"),
                        includeMarkdown("markdown/people_values/food_insecurity.Rmd"),
                    ),  
                    navset_card_pill(
                        title = "Food Insecurity",
                        wrapper = card_body(),
                        nav_panel(
                            title = "Poverty Rates",
                            leafletOutput("povertyRateMap")
                        ),
                        nav_panel(
                            title = "African Population Low Access Markets (Half Mile)",
                            leafletOutput("lowAccessAF"), 
                        ),
                        nav_panel(
                            title = "African Population Low Access Markets (One Mile)",
                            leafletOutput("lowAccessAF1"), 
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
shinyApp(ui, server)