library(shiny)
library(bslib)
library(bsicons)
library(markdown)
library(leaflet)
library(RColorBrewer)

source("functions.r")
ui <- page_navbar(
  title = "phaceholder",
  selected = "overview",
  theme = bs_theme(preset = "journal"),
  nav_menu(
    title = "Overview",
    nav_panel(
      title = "Project Background",
      value = "overview",
      # probably the only time you'll see fluidRow as it's just better for text
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
            card_body(includeMarkdown("markdown/sodem/sodem_characteristics.md"))
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
              footer = "Source: 2022 ACS 5-Year DP05 Table",
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
    layout_sidebar()
  )
)

server <- function(input, output) {
  ### --- SOCIODEMOGRAPHICS ---
  
  # gets all 2022 ACS DP05 data for median age + black population choropleth
  # check functions.R for more info
  filenames <- list.files("data/ACS_DP05", pattern = "*.csv", full.names = TRUE)
  sodem_data <- lapply(filenames, preprocess_sodem_data)
  sodem_data <- bind_rows(sodem_data)
  
  # gets preprocessed geo data for hampton roads
  # check functions.R for more info
  geo_data <- preprocess_geo_data("data/geo_data.rds")
  
  # create choropleth data for leaflet creation
  heatmap_data <-
    merge(geo_data, sodem_data, by.x = "loc_name", by.y = "loc") %>%
    # postprocess merged data on heatmap
    mutate(loc_name = str_replace_all(loc_name, "_", " ")) %>%
    mutate(loc_name = str_to_title(loc_name)) %>%
    mutate(
      pct_black = round(black_or_african_american / total_population, 3) * 100,
      .before = geometry
    )
  
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