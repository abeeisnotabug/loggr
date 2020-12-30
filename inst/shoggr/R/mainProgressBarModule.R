library(shinydashboard)
library(shinyWidgets)

mainProgressBarUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("mainProgressBarBox"))
}

mainProgressBarServer <- function(id, scriptOutInfos, overallIters, currentEnds, finishedItersPerScript, processStati) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$mainProgressBarBox <- renderUI({
        req(reactiveValuesToList(finishedItersPerScript))
        
        flog.info("mainProgBarUI")
        
        localFinishedItersSum <- isolate(do.call(sum, reactiveValuesToList(finishedItersPerScript)))
        processStatiLocal <- isolate(
          list(
            scripts = reactiveValuesToList(processStati$scripts),
            workers = lapply(processStati$workers, reactiveValuesToList)
          )
        )
        allProcessStati <- unlist(processStatiLocal)
        overallIcon <- if (all(allProcessStati != "N"))
          icons$R
        else if (any(allProcessStati != "N"))
          icons$failure
        else
          icons$N
        
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
              striped = all(allProcessStati != "N"),
              title = span(overallIcon, "Overall progress")
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
