library(shinydashboard)
library(shinyWidgets)

mainProgressBarUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("mainProgressBarBox"))
}

mainProgressBarServer <- function(id, scriptOutInfos, overallIters, currentEnds, finishedItersPerScript) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$mainProgressBarBox <- renderUI({
        req(reactiveValuesToList(finishedItersPerScript))
        
        flog.info("mainProgBarUI")
        
        localFinishedItersSum <- isolate(do.call(sum, reactiveValuesToList(finishedItersPerScript)))
        
        fluidRow(
          box(
            width = 12,
            status = "danger",
            progressBar(
              id = ns("mainProgBar"),
              value = localFinishedItersSum,
              total = overallIters,
              display_pct = TRUE,
              status = ifelse(localFinishedItersSum < overallIters, "primary", "success"),
              striped = FALSE,
              title = "Overall progress"
            )
          )
        )
      })
      
      observe({
        finishedItersSum <- do.call(sum, reactiveValuesToList(finishedItersPerScript))
        
        flog.info("mainProgBarSERVER")
        
        updateProgressBar(
          session = session,
          id = "mainProgBar",
          value = finishedItersSum,
          total = overallIters,
          status = ifelse(finishedItersSum < overallIters, "primary", "success")
        )
      })
    }
  )
}
