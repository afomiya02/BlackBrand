library(shiny)
library(tidyverse)
library(ggExtra)

## FUNCTIONS AND VARIABLES FOR CONSISTENCY PURPOSES
# normally these values, variables and functions are in another file
# as well as ui and server
df <- as_tibble(iris)
df_num <- df |> select(where(is.numeric)) # vars suitable for scatterplot

ui <- fluidPage(
  titlePanel("Hello CMDA 4864!"), # title panel
  sidebarLayout( # creates sidebar
    sidebarPanel("What labels do you want to use?", 
      p(""), # spacer
      selectInput("xvar", # your ID
        label = "x-var:",
        selected = "Sepal.Length", # the option you select to start with
        choices = colnames(df_num) # your choices for the variables
      ),
      selectInput("yvar",
        label = "y-var:",
        selected = "Sepal.Width", 
        choices = colnames(df_num)
      ),
      checkboxInput(
        inputId = "show_species",
        label = "show species type"
      )
    ),
    mainPanel(
      plotOutput("scatterplot")
      # scatterplot is the value we point to in the server
      # to get the output we want to show
    )
  )
)

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({ # the ids given in ui
    req(input$xvar)
    req(input$yvar)
    p <- ggplot(iris, aes_string(paste0("`", input$xvar, "`"), paste0("`", input$yvar, "`"))) +
      list(if (input$show_species) aes(color = Species), geom_point(), theme(legend.position = "bottom"))
    
    margin_type <- if (input$show_species) "density" else "histogram"
    p <- ggMarginal(p, type = margin_type, margins = "both", size = 8, groupColor = input$show_species, groupFill = input$show_species)
    p
  }, res = 100)
}

shinyApp(ui, server)
