library(shiny)
library(shinythemes)
library(markdown)

source("functions.r")

ui <- navbarPage(
  title = "placeholder",
  theme = shinytheme("cosmo"),
  selected = "overview",
  
  # the actual dashboard starts here
  navbarMenu(
    title = "Overview",
    tabPanel(
      "Project Introduction",
      value = "overview",
      fluidRow(
        style = "margin: 2px;",
        align = "center",
        h1(
          strong(
            "Tracking Indicators of the Economic and Social
            Mobility of the Black Community in Hampton Roads"
          ),
          br(""),
          h4(
            "Virginia Tech: Data Science for the Public
            Good Program & CMDA Capstone Team"
          ),
          h4("See 'Meet the Team' tab for detailed team contributions"),
          br()
        )
        # end of tabpanel
      ),
      fluidRow(
        style = "margin: 6px;",
        column(
          width = 4,
          includeMarkdown("markdown/overview/hampton_roads.md")
        ),
        column(
          width = 4,
          h2(strong("Project Background")),
          p(
            "The Black Business Research Analytics Networking and Development (Black BRAND) is a
            non-profit organization operating in the Hampton Roads region. Established in 2016, the
            primary goal of Black BRAND is to improve the values of the Black families and community
            within the region."
          ),
          p(
            "Despite the overall economic growth in the United States, there is a significant
            Black-white wealth gap. In 2019, the median white household was around $188,200 - 7.8",
            em("times"),
            "that of the typical Black household of $24,100. As such, Black BRAND wants
            to investigate the economic well-being of the Black community in Hampton Roads. Moreover,
            given the recent increase in support for black businesses, it is important to determine
            whether this has resulted in any economic improvement for the Black community."
          ),
          p(
            "Guided by our meetings with the Black BRAND stakeholders and Claud Anderson's PowerNomics model,
            there are five main pillars to measure the overall economic and social progress of the
            Black community in Hampton Roads."
          ),
          img(src = "Flowchart.png",
              style = "display:inline;margin-left: auto; margin-right: auto;", width = "80%"),
        ),
        column(
          width = 4,
          h2(strong("Project Goals")),
          p(
            "Our team aims to create a dashboard that shows the state of the Black community in the 
            Hampton Roads region. This will enable stakeholders to understand the myriad of past and 
            current factors that affect the economic and social progress of Black households in Hampton Roads. 
            As such, data-driven recommendations can be made to improve the well-being of our residents."
          ),
          p(
            "We identified, acquired, and used publicly available data to provide our stakeholders, 
            Black BRAND, with a dashboard that shows a combined view of multiple indicators for the 
            two pillars: Education and Economics."
          ),
          p(
            "Under the Education pillar, we collected indicators across three distict issues: "
          ),
          tags$ul(
            tags$li("Educators"),
            tags$li("Educational Attainment"),
            tags$li("Suspension")
          ),
          p(),
          p(
            "Under the Economics pillar, we collected indicators across five distinct issues:"
          ),
          tags$ul(
            tags$li("Income/Wealth"),
            tags$li("Labor Market"),
            tags$li("Homeownership"),
            tags$li("Health"),
            tags$li("Household Wellbeing")
          ),
          p(
            "We conducted a cross-comparison analysis with our indicators across all counties and 
            cities in Hampton Roads.  Our project also compares the Black population against the general 
            population in Hampton Roads to determine whether racial differences exist within each locality. 
            Moreover, we also present information for the general population in Virginia."
          ),
          p(
            "This dashboard compiles our findings and allows our stakeholders, and other interested 
            users to explore the information dynamically."
          )
        )
      ),
      fluidRow(align = "center", p(tags$small(em('Last updated: March 2024'))))
    ),
    tabPanel("Meet the Team"),
    tabPanel("Future Work"),
  ),
  tabPanel(
    title = "Sociodemographics",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          
        )
      )
    )
  ),
  tabPanel(title = "Education",),
  tabPanel(title = "Economics"),
  tabPanel(title = "Policy & Justice"),
  tabPanel(title = "Media & Entertainment"),
  tabPanel(title = "People & Values")
  
)

server <- function(input, output, session) {
  
}
shinyApp(ui, server)