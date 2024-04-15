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
      dplyr::filter(subgroup %in% input$races) %>%
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
        geom_point(aes(x = year, y = !!sym(input$races[i])), colour = pal[input$races[i]], size = 5)
    }
    
    p + ylim(0, 100)
  })
  
  
  ### --- ECONOMICS ---
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
    
    incomeGraph <- ggplot(income_years, aes(x = Year, y = `Median Income (US Dollars)`, color = Demographic, group = Location, linetype = Location)) + 
      geom_line(data = va_total, size = 1.3, aes(linetype = Location)) +
      geom_line(data = va_black, size = 1.3, aes(linetype = Location)) +
      geom_line(data = hr_total, size = 1.3, aes(linetype = Location)) +
      geom_line(data = hr_black, size = 1.3, aes(linetype = Location)) +
      scale_color_manual(name = "Population", values = c("black", "#800404")) +
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
    
    # Function to create line plots for each locality
    pick_n <- function(Locality) {
      # Filter data for the selected locality
      dataFiltered <- filter(all_hm_data, NAME == Locality)
      
      # Create ggplot line plot
      hm_line <- ggplot(dataFiltered,
                        aes(
                          x = Year,
                          y = Percent,
                          color = Demographic,
                          group = Demographic
                        )) +
        geom_line(position = "identity") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_discrete(name = "",
                            labels = c("Black Home Owners", "White Home Owners")) +
        scale_fill_manual(values = c("#A9A9A9", "#8B0000")) +
        theme(legend.position = "bottom") +
        labs(title = Locality)
      
      # Return ggplot object
      return(hm_line)
    }
    
    # Apply pick_n function to each locality in the data
    r <- lapply(1:length(unique(b_hm_19$NAME)), function(i) {
      pick_n(b_hm_19$NAME[i])
    })
    
    # Create color palette for choropleth map
    pal <- colorNumeric(palette = "viridis",
                        domain = b_hm_19$Percent,
                        reverse = TRUE)
    
    # Create Leaflet map object
    b_hmown_leaf_19 <- b_hm_19 %>%
      leaflet(options = leafletOptions(
        minZoom = 5,
        maxZoom = 15,
        drag = FALSE
      )) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(
        data = b_hm_19,
        color = ~ pal(Percent),
        weight = 0.5,
        fillOpacity = 0.7,
        smoothFactor = 0,
        highlightOptions = highlightOptions(
          bringToFront = TRUE,
          opacity = 1.5,
          weight = 3
        ),
        label = ~ paste0(NAME,  " Black Homeowners: ", Percent, "%"),
        group = "Black Home Owners",
        popup = popupGraph(r)
      ) %>%
      addPolygons(
        data = tot_hm_19,
        color = ~ pal(Percent),
        weight = 0.5,
        fillOpacity = 0.7,
        smoothFactor = 0,
        highlightOptions = highlightOptions(
          bringToFront = TRUE,
          opacity = 1.5,
          weight = 3
        ),
        label = ~ paste0(NAME,  " Total Homeowners: ", Percent, "%"),
        group = "Total Home Owners",
        popup = popupGraph(r)
      ) %>%
      addLayersControl(
        baseGroups = c("Total Home Owners"),
        overlayGroups = c("Black Home Owners"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Black Home Owners") %>%
      addLegend(
        "topleft",
        pal = pal,
        values = ~ Percent,
        title = "Home Owners",
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
      va_file <- paste0("data/poverty/va_poverty", year, ".csv")
      hamp_file <- paste0("data/poverty/hamp_poverty", year, ".csv")
      pov_data <- read_process_csv(va_file, hamp_file, c(123, 136))
    } else if (var_poverty() %in% c("2014", "2013", "2012")) {
      year <- var_poverty()
      va_file <- paste0("data/poverty/va_poverty", year, ".csv")
      hamp_file <- paste0("data/poverty/hamp_poverty", year, ".csv")
      pov_data <- read_process_csv(va_file, hamp_file, c(93, 102))
    }
    
    # Generate the plot using ggplot2
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
    input$PovertyCountYearDrop
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
  
}

shinyApp(ui, server)