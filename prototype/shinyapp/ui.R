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
#source("education.r")
source("economics.r")
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
  # nav_panel(
  #   title = "Education",
  #   layout_sidebar(
  #     sidebar = sidebar(
  #       title = "Standardized Testing",
  #       width = validateCssUnit("20%"), # sidebar takes up x% of page
  #       selectInput(
  #         inputId = "loc",
  #         label = "Select locality:",
  #         selected = "Chesapeake",
  #         choices = unique(st_data$division_name)
  #       ),
  #       checkboxGroupInput(
  #         inputId = "races",
  #         label = "Select races:",
  #         choices = c("Black" = "Black",
  #                     "White" = "White",
  #                     "Asian" = "Asian",
  #                     "Hispanic" = "Hispanic"),
  #         selected = "Black"
  #       )
  #     ),
  #     accordion(
  #       multiple = FALSE,
  #       accordion_panel(
  #         title = "Standardized Testing",
  #         navset_card_tab(
  #           nav_panel(
  #             title = "2022-2023 Testing Results",
  #             layout_column_wrap(
  #               plotlyOutput("radio_plot"),
  #               layout_column_wrap(
  #                 #uiOutput("st_value_boxes")
  #               ))
  #           ),
  #           nav_panel(
  #             title = "Race Comparison",
  #             card_body(plotOutput("lollipop_plot")),
  #           ),
  #           card_footer("Source: VDOE Annual Pass Rates (Division Subject Area)")
  #         ),
  #         
  #       ),
  #       accordion_panel(
  #         title = "Educators?",
  #         "meow"
  #       )
  #     )
  #   )
  # ),
 



  nav_panel(
    title = "Economics",
    fluidRow(
      tabsetPanel(
        id = "economics_tabs",
        type = "tabs",
        tabPanel("Income", 
                 fillPage(
                   layout_sidebar(
                     sidebar = sidebar(
                       width = validateCssUnit("33%"),
                       title = tags$b("Household's Economic Status in Hampton Roads"),
                       includeMarkdown("markdown/economics/income.Rmd"),
                     ),
                     layout_column_wrap(
                       width = 1,
                       heights_equal = "row",
                       fluidRow(
                         plotOutput("medianTimeGraph") 
                       )
                     )
                   )
                 )
        ),
        tabPanel("Homeownership", 
                 fillPage(
                   layout_sidebar(
                     sidebar = sidebar(
                       width = validateCssUnit("33%"),
                       title = tags$b("Homeownership in Hampton Roads"),
                       includeMarkdown("markdown/economics/homeownership.Rmd"),
                     ),
                     layout_column_wrap(
                       width = 1,
                       heights_equal = "row",
                       fluidRow(
                        leafletOutput("homeownership_map")
                       )
                     )
                   )
                 )
        ),
        tabPanel("Labor Market", 
                 fillPage(
                   layout_sidebar(
                     sidebar = sidebar(
                       width = validateCssUnit("33%"),
                       title = tags$b("Labor Market Characteristics in Hampton Roads"),
                       includeMarkdown("markdown/economics/labor_market.Rmd"),
                     ),
                     navbarPage(
                       "Labor Market Characteristics",
                       navset_card_tab(
                         height = validateCssUnit("1000px"), # Adjust the height as needed
                         full_screen = TRUE,
                         wrapper = card_body(),
                         title = "Labor Market Analysis",
                         id = "labor_market_tab",
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
                   )
                 )
        )
        ,
        
       
        tabPanel("Poverty", 
                 fillPage(
                   layout_sidebar(
                     sidebar = sidebar(
                       width = validateCssUnit("33%"),
                       title = tags$b("How does the poverty rate in Hampton Roads compare to all of Virginia?"),
                       includeMarkdown("markdown/economics/poverty.Rmd"),
                     ),
                     navbarPage(
                       "Poverty Rates in Hampton Roads",
                       navset_card_tab(
                         height = validateCssUnit("1000px"), # Adjust the height as needed
                         full_screen = TRUE,
                         wrapper = card_body(),
                         title = "Poverty",
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
                   )
                 )
        ),
        tabPanel("Health", 
                 fillPage(
                   layout_sidebar(
                     sidebar = sidebar(
                       width = validateCssUnit("33%"),
                       title = tags$b("Insurance Status In Hampton Roads"),
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
                 )
        ),
        tabPanel("Veterans", 
                 fillPage(
                   layout_sidebar(
                     sidebar = sidebar(
                       width = validateCssUnit("33%"),
                       title = tags$b("Veteran Population in Hampton Roads"),
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
                 )
        ),
        tabPanel("Business", 
                 fillPage(
                   layout_sidebar(
                     sidebar = sidebar(
                       width = validateCssUnit("33%"),
                       title = tags$b("Business Environment in Hampton Roads"),
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
                 )
        ),
        tabPanel("Household Well-being", 
                 fillPage(
                   layout_sidebar(
                     sidebar = sidebar(
                       width = validateCssUnit("33%"),
                       title = tags$b("Well-being of Households in Hampton Roads"),
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
      )
    )
  )
  





  ,
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