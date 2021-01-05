downloadWarningsUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("downloadWarningsButtonUIelement"))
}

downloadWarningsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$downloadWarningsButtonUIelement <- renderUI({
        actionBttn(
          inputId = ns("downloadWarningsButton"),
          label = "Warnings",
          color = "warning",
          icon = icon("download"),
          style = "unite",
          size = "sm"
        )
      })
      
      observeEvent(input$downloadWarningsButton, {
        showModal(
          modalDialog(
            h3("Haha"),
            title = "Download logged conditions"
          )
        )
      })
    }
  )
}