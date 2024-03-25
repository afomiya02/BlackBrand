library(shiny)
library(shinythemes)
source("functions.r")

ui <- navbarPage(
    title = "placeholder",
    theme = shinytheme("cosmo"),
    # the actual dashboard starts here
    navbarMenu(
        title = "Overview",
        tabPanel("Project Introduction"),
        tabPanel("Meet the Team"),
        tabPanel("Future Work")
    ),
    tabPanel(
        title = "Sociodemographics",
    ),
    tabPanel(
        title = "Economics"
    ),
    tabPanel(
        title = "Policy & Justice"
    ),
    tabPanel(
        title = "Media & Entertainment"
    )
)

server <- function(input, output, session) {
  
}
shinyApp(ui, server)
