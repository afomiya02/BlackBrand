library(shiny) # shiny
library(bslib) # our savior 🙏
library(bsicons)
library(tidyverse)
library(ggExtra)

## FUNCTIONS AND VARIABLES FOR CONSISTENCY PURPOSES
# normally these values, variables and functions are in another file
# as well as ui and server
df <- iris
df_num <- df |> select(where(is.numeric)) # vars suitable for scatterplot

ui <- page_sidebar(
    title = "Hello, World!",
    theme = bs_theme(preset = "cosmo"), # adds custom bootstrap theming to the dashboard
    ## ALL CUSTOM THEMES CAN BE FOUND HERE: https://bootswatch.com/
    sidebar = sidebar(
        title = "Select variables to use:", # sidebar title
        # notice how we need no spacer here!
        width = validateCssUnit("33%"), # sidebar is one third of entire dashboard
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
        checkboxInput( # creates checkbox
            inputId = "show_species",
            label = "show species type",
            TRUE # keep selected
        )
    ),
    layout_columns(
        value_box(
            title = "Dataset dimensions",
            value = paste(dim(iris)[1], "x", dim(iris)[2]),
            showcase = bs_icon("arrow-up-right-square")
        ),
        value_box(
            title = "Species tested",
            value = length(unique(iris$Species)),
            showcase = bs_icon("flower3")
        ),
        value_box(
            title = "# of flowers per species",
            value = nrow(iris) / length(unique(iris$Species)),
            showcase = bs_icon("grid-3x3")
        )
    ),
    card(
        card_header("Iris Comparison Scatterplot"),
        card_body(plotOutput("plot")),
        card_footer("hello capstone crew! :)")
    )
)

server <- function(input, output, session) {
    ## HEAVILY INFLUENCED BY THE HOME PAGE DASHBOARD
    ## https://shiny.posit.co/
    
    # notice how nothing has changed in the server code
    output$plot <- renderPlot({
        req(input$xvar) # remember the ids given in ui?
        req(input$yvar) # req() is a function that requires values to be given else there's no output

        # assign p to output$scatterplot
        # we use aes_string because we're passing strings as values
        p <- ggplot(iris, aes(!!input$xvar, !!input$yvar)) +
            list(if (input$show_species) aes(color = Species), geom_point(), 
                 theme_bw(), theme(legend.position = "bottom"))
        # using list() allows conditionals to be passed onto the plot, allowing the checkbox to work

        # depending on whether the checkbox is selected we choose the margin type
        margin_type <- if (input$show_species) "density" else "histogram"

        # ggExtra functions that show the marginal plots at the top & right
        p <- ggMarginal(p, type = margin_type, margins = "both", size = 8, 
                        groupFill = input$show_species)

        p # equivalent to return(p)
    }, res = 100) # res: png resolution of the plot
}

shinyApp(ui, server)
