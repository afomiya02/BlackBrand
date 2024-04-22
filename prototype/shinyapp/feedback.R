feedback <- function(input,output,session){
  # feedback button code -----------------------------------------------------------
  # install.packages("shinyjs")
  # store the rating
  rating <- reactiveVal(NULL)
  
  # update button styles based on selection
  updateButtonStyles <- function(selected) {
    # highlight the rating button when click 
    btn_ids <- paste0("rate", 1:10)
    for (btn_id in btn_ids) {
      if (btn_id == selected) {
        addClass(selector = paste0("#", btn_id), class = "btn-highlight")
      } else {
        removeClass(selector = paste0("#", btn_id), class = "btn-highlight")
      }
    }
  }
  
  # when the feedback button is clicked
  observeEvent(input$show_feedback, {
    showModal(modalDialog(
      title = "Help us improve!",
      tags$p("How easy was it for you to navigate the website?"),
      # makes 10 rating buttons from 1-10
      output$ratingButtons <- renderUI({
        tags$div(class = "rating-buttons",
                 fluidRow(
                   lapply(1:10, function(i) {
                     actionButton(inputId = paste0("rate", i), label = as.character(i), class = "btn-light")
                   })
                 )
        )
      }),
      # text box for feedback
      tags$p("Tell us more about your experience."),
      textAreaInput("feedback_text", label = NULL, placeholder = "Your feedback here..."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_feedback", "Submit feedback", class = "btn-primary")
      )
    ))
  })
  
  # update rating button styles when a rating button is clicked
  observeEvents <- lapply(paste0("rate", 1:10), function(btn_id) {
    observeEvent(input[[btn_id]], {
      rating(as.integer(substr(btn_id, 5, nchar(btn_id))))
      updateButtonStyles(btn_id)
    })
  })
  
  # print feedback submission
  observeEvent(input$submit_feedback, {
    feedback <- paste("Rating:", rating(), "Feedback:", input$feedback_text)
    print(feedback)
    removeModal()
    #thank you pop up
    showModal(modalDialog(
      title = "Thank You!",
      "We appreciate your feedback.",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))
  })
  # feedback button code end --------------------------------------------------
}