library(shiny)
library(tidyverse)
library(ggExtra)
library(DT)

## HEAVILY INFLUENCED BY THE HOME PAGE DASHBOARD
## https://shiny.posit.co/

## FUNCTIONS AND VARIABLES FOR CONSISTENCY PURPOSES
# normally these values, variables and functions are in another file
# as well as ui and server
df <- as_tibble(iris)
df_num <- df |> select(where(is.numeric)) # vars suitable for scatterplot

ui <- fluidPage( # the page your dashboard will be contained in
    titlePanel("Hello CMDA Capstone Crew!"), # title panel
    sidebarLayout( # creates sidebar layout including sidebar and main panels
        sidebarPanel("Select variables to use:",  # everything to include under sidebar
                     p(""), # spacer
                     varSelectInput(
                         inputId = "xvar", # the id given to the input
                         label = "x-var:", # the label given to the front-end
                         selected = "Sepal.Length", # the option you select to start with
                         data = df_num # the dataset used for your inputs and the choices given
                     ),
                     varSelectInput(
                         inputId = "yvar",
                         label = "y-var:", 
                         selected = "Sepal.Width", 
                         data = df_num
                     ),
                     checkboxInput( # creates a checkbox
                         inputId = "show_species",
                         label = "show species type",
                         TRUE # start with the checkbox selected
                     )
        ),
        mainPanel(
            p("Dataset dimension size: 150x5"), # will use placeholders here
            p("Number of species tested: 3"),
            p("Number of flowers per species: 50"),
            p(h2("Iris Comparison Scatterplot")),
            plotOutput("scatterplot"), # outputs plot
            p(h2("Iris data table")),
            dataTableOutput("table") # outputs data table
            # scatterplot and table are the values we point to in the server function
            # to get the output we want to show
        )
    )
)

server <- function(input, output, session) {
    # said value
    output$scatterplot <- renderPlot({
        req(input$xvar) # remember the ids given in ui?
        req(input$yvar) # req() is a function that requires values to be given else there's no output
        
        # assign p to output$scatterplot
        # we use aes_string because we're passing strings as values
        p <- ggplot(iris, aes_string(paste0("`", input$xvar, "`"), paste0("`", input$yvar, "`"))) +
            list(if (input$show_species) aes(color = Species), geom_point(), theme_bw(), theme(legend.position = "bottom"))
        # using list() allows conditionals to be passed onto the plot, allowing the checkbox to work
        
        # depending on whether the checkbox is selected we choose the margin type
        margin_type <- if (input$show_species) "density" else "histogram"
        
        # ggExtra functions that show the marginal plots at the top & right
        p <- ggMarginal(p, type = margin_type, margins = "both", size = 8, groupColor = input$show_species, groupFill = input$show_species)
        
        p # equivalent to return(p)
    }, res = 100) # res: png resolution of the plot
    
    # said value
    output$table <- renderDataTable(iris, options = list(pageLength = 5, lengthChange = FALSE))
}

shinyApp(ui, server)
