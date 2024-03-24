library(shiny)

source("functions.r")

ui <- fluidPage(
  titlePanel(
    h1("test123")
  ),
  sidebarLayout()
)

server <- function(input, output, session) {
  
}
shinyApp(ui, server)