library(shinyWidgets)
library(shinyjs)

if (is.null(getOption("loggr.log.folder.path"))) library(loggr)
logFolderPath <- getOption("loggr.log.folder.path")

simuPickerUI <- function(id) {
  ns <- NS(id)

  tagList(
    pickerInput(
      inputId = ns("simuPicker"),
      label = "Pick a Simulation",
      choices = dir(logFolderPath),
      options = list(`live-search` = TRUE)
    ),
    # makeRefreshButton(ns("refreshFolders"), "Refresh Simu Dir", "redo"),
    makeRefreshButton(ns("pickButton"), "Pick", "paper-plane"),
    uiOutput(ns("pickedSimuDisplay"))
  )
}

simuPickerServer <- function(id, topLevelSession) {
  moduleServer(
    id,
    function(input, output, session) {
      pickedSimu <- reactiveVal(NULL)
      
      observeEvent(input$refreshFolders, {
        pickedSimuBeforeRefresh <- input$simuPicker
        logFolderContents <- dir(logFolderPath)
        
        updatePickerInput(
          session = session,
          inputId = "simuPicker",
          choices = logFolderContents,
          selected = if (pickedSimuBeforeRefresh %in% logFolderContents) pickedSimuBeforeRefresh else NULL
        )
      })
      
      observeEvent(input$pickButton, {
        lapply(
          c("refreshFolders", "simuPicker", "pickButton"),
          function(actionID) {
            disable(actionID)
            hide(actionID)
          }
        )
        
        output$pickedSimuDisplay <- renderUI({
          div(
            align = "center",
            h5(paste("Monitoring Simu:", input$simuPicker))
          )
        })
        
        updateTabItems(topLevelSession, "tabs", "simuMonitorTab")
        
        pickedSimu(file.path(logFolderPath, input$simuPicker))
      })
      
      return(
        reactive({
          req(pickedSimu())
          
          pickedSimu()
        })
      )
    }
  )
}
