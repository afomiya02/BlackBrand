library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(tidyverse)
library(magick)
source("sodem.r")
source("education.r")

ui <- page_navbar(
    title = img(src="logo_WIDE.png"),
    selected = "overview",
    theme = bs_theme(preset = "journal"),
    useShinyjs(),
    nav_menu(
        title = "Overview",
        nav_panel(
          title = "Home",
          value = "overview",
          fluidRow(
            style = "margin: 2px",
            align = "center",
            img(src = "frontpage.png"),
          ),
          fluidRow(
            style = "margin: 2px",
            align = "center",
            column(
              width = 4,
              img(src = "bb2.png"),
              h1(strong("Professionalism"), align = "center"),
            ),
            column(
              width = 4,
              img(src = "bb3.png"),
              h1(strong("Business"), align = "center"),
            ),
            column(
              width = 4,
              img(src = "bb1.png"),
              h1(strong("Community"), align = "center"),
            )
          ),
          fluidRow(
            style = "margin: 2px",
            align = "center",
            column(
              width = 5,
              img(src = "race_business.png", align = "center", width = "400px"),
              br("The image above shows the average number of businesses per state, by race. As can be seen, the number of businesses in Virginia is 7.8.
                 This is the state where Hampton Roads is located, and the population for the state does not justify the number of businesses. Black BRAND
                 is seeking to change that by promoting entrepreneurship and business within the black population. 
                 ")
            ),
            column(
              width = 2
            ),
            column(
              width = 5,
              br("Below is the Hampton Roads localities, separated by the counties. There are a large number of counties that make up the Hampton Roads 
                 area. However, a bunch of these areas are in despair with a lack of resources that is neglecting the chances of fostering business with 
                 a growth mindset. 
                 "),
              img(src = 'localities.png', align = "center",  width = "300px"),
            ),
          ),
          fluidRow(
            style = "margin: 2px",
            align = "center",
            img(src = "city.png")
          ),
          fluidRow(
            style = "margin: 2px",
            align = "center",
            h1(strong("Executive Team"), align = "center"),
            p("", style = "padding-top:10px;"),
            column(
              width = 4,
              img(src = "blair_durham.png"),
              h1(strong("Blair Durham: "), align = "center"),
              h1(strong("Co-Founder/President"), align = "center")
            ),
            column(
              width = 4,
              img(src = "naomi_lewis.png"),
              h1(strong("Naomi Lewis: "), align = "center"),
              h1(strong("Parliamentarian"), align = "center")
            ),
            column(
              width = 4,
              img(src = "lafayette_judkins.png"),
              h1(strong("Lafayette Judkins: "), align = "center"),
              h1(strong("Chairman"), align = "center")
            ),
          ),
          fluidRow(
            style = "margin: 2px",
            align = "center",
            img(src = "VTDSPG Logo.png")
          )
        ),
        nav_panel(
            title = "Project Background",
            value = "overview",
            # probably the only time you'll see fluidRow since it allows text alignment
            fluidRow(
                style = "margin: 2px",
                align = "center",
               
                img(src = "VTIntro.png"),
            ),
            br(""),
            fluidRow(
              style = "margin: 6px",
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
                    img(src="vtlogo.jpg")
                ),
                column(
                    width = 4,
                    includeMarkdown("markdown/overview/project_goals.md"),
                ),
            )
        ),
        nav_panel(title = "Data & Methodology",
                  fluidRow(
                    style = "margin: 6px;",
                    h1(strong("Data"), align = "center"),
                    p("", style = "padding-top:10px;"),
                    column(
                      width = 4,
                      img(src = 'acs.png', style = "display: inline; float: left;", width = "200px"),
                      includeMarkdown("markdown/data_methodology/data1.md"),
                      br(),
                      img(src = 'broadbandnow.png', style = "display: inline; float: left;", width = "150px"),
                      includeMarkdown("markdown/data_methodology/data2.md"),
                      img(src = 'zillow.png', style = "display: inline; float: left;", width = "150px"),
                      includeMarkdown("markdown/data_methodology/data3.md")
                    ),
                    column(
                      width = 4,
                      img(src = 'localities.png', style = "display: inline; float: center;", width = "475px"),
                      p(),
                      p(),
                      br(),
                      img(src = 'kidscount.png', style = "display: inline; float: left;", width = "150px"),
                      includeMarkdown("markdown/data_methodology/data4.md"),
                      br(),
                      img(src = 'doe.jpg', style = "display: inline; float: left;", width = "150px"),
                      includeMarkdown("markdown/data_methodology/data5.md"),
                    ),
                    column(
                      width = 4,
                      img(src = 'oecd.png', style = "display: inline; float: left;", width = "150px"),
                      includeMarkdown("markdown/data_methodology/data6.md"),
                      br(),
                      img(src = 'googleMapLogo.png', style = "display: inline; float: left;", width = "150px"),
                      includeMarkdown("markdown/data_methodology/data7.md"),
                      br(),
                      img(src = 'censusreporter.png', style = "display: inline; float: left;", width = "150px"),
                      includeMarkdown("markdown/data_methodology/data8.md"),
                    )
                  ),
                  fluidRow(
                    style = "margin: 6px;",
                    h1(strong("5 Pillars of Methodology"), align = "center"),
                    p("", style = "padding-top:10px;"),
                    column(
                      width = 4,
                      strong("Education: "),
                      includeMarkdown("markdown/data_methodology/methodology1.md"),
                      img(
                        src = 'education.png',
                        align = "center",
                        height = "300px",
                        width = "400px"
                      ),
                      p(
                        tags$small(
                          "[1] Anna J. Egalitea, A.J. , Kisida B., & Winters, M.A. (2015), Economics of Education Review, 45, 44-52."
                        )
                      ),
                      p(
                        tags$small(
                          "[2] Noltemeyer, A. L., Ward, R. M., & Mcloughlin, C. (2015). Relationship between school suspension and student outcomes: A meta-analysis. School Psychology Review, 44(2), 224-240."
                        )
                      ),
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
                      img(
                        src = 'economic.png',
                        align = "center",
                        height = "300px",
                        width = "400px"
                      ),
                      p(),
                      p(),
                      includeMarkdown("markdown/data_methodology/methodology6.md"),
                      p(
                        tags$small(
                          "[1] Butler, S. M., Beach, W. W., & Winfree, P. L. (2008). Pathways to economic mobility: Key indicators. Economic mobility project."
                        )
                      )
                    )
                  )
                  )
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
}

shinyApp(ui, server)