library(shiny) # import these
library(tidyverse)
library(ggExtra)

## FUNCTIONS AND VARIABLES FOR CONSISTENCY PURPOSES
# normally these values, variables and functions are in another file
# as well as ui and server
df <- iris
df_num <- df %>% select(where(is.numeric)) # vars suitable for scatterplot

ui <- fluidPage( # the page your dashboard will be contained in
    titlePanel("Hello, world!"), # title panel
    sidebarLayout( # creates sidebar layout including sidebar and main panels
        sidebarPanel("Select variables to use:",  # everything to include under sidebar
            p(""), # spacer
            varSelectInput(
                inputId = "xvar",
                label = "x-var:",
                selected = "Sepal.Length",
                data = df_num
            ),
            varSelectInput(
                inputId = "yvar",
                label = "y-var:",
                selected = "Sepal.Width",
                data = df_num
            ),
            checkboxInput(
                inputId = "show_species",
                label = "show species type",
                TRUE
            )
        ),
        mainPanel(
            p(h2("Iris Comparison Scatterplot")),
            plotOutput("plot")
        )
    )
)

server <- function(input, output, session) {
    # said value
    ## HEAVILY INFLUENCED BY THE HOME PAGE DASHBOARD
    ## https://shiny.posit.co/
    output$plot <- renderPlot({
        req(input$xvar) # remember the ids given in ui?
        req(input$yvar) # req() is a function that requires values to be given else there's no output
        
        # assign p to output$scatterplot
        # we use aes_string because we're passing strings as values
        p <- ggplot(iris, aes(!!input$xvar, !!input$yvar)) +
            list(if (input$show_species) aes(color = Species), 
                 geom_point(), theme_bw(), theme(legend.position = "bottom"))
        # using list() allows conditionals to be passed onto the plot, allowing the checkbox to work
        
        # depending on whether the checkbox is selected we choose the margin type
        margin_type <- if (input$show_species) "density" else "histogram"
        
        # ggExtra functions that show the marginal plots at the top & right
        p <- ggMarginal(p, type = margin_type, margins = "both", size = 8, 
                        groupColor = input$show_species, groupFill = input$show_species)
        
        p # equivalent to return(p)
    }, res = 100) # res: png resolution of the plot
}

shinyApp(ui, server)
