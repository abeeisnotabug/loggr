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
      choices = dir(logFolderPath)[dir(logFolderPath) != "loggr_speed_observer"],
      options = list(`live-search` = TRUE)
    ),
    makeButton(ns("pickButton"), "Pick", "paper-plane"),
    uiOutput(ns("pickedSimuDisplay"))
  )
}

simuPickerServer <- function(id, topLevelSession, topout) {
  moduleServer(
    id,
    function(input, output, session) {
      pickedSimu <- reactiveVal(NULL)
      dropDownColor <- "black"
      
      observeEvent(topout(), {
        pickedSimuBeforeRefresh <- input$simuPicker
        logFolderContents <- dir(logFolderPath)
        simuFolders <- logFolderContents[logFolderContents != "loggr_speed_observer"]
        
        doesSimuRun <- lapply(
          makeSelfNamedVector(simuFolders),
          function(simuFolder) {
            getSimuRunStatus(file.path(logFolderPath, simuFolder), topout)
          }
        )
        
        simuIcons <- ifelse(
          doesSimuRun,
          rep("glyphicon-play", length(doesSimuRun)),
          rep("glyphicon-stop", length(doesSimuRun))
        )
        
        updatePickerInput(
          session = session,
          inputId = "simuPicker",
          choices = simuFolders,
          selected = if (pickedSimuBeforeRefresh %in% simuFolders) pickedSimuBeforeRefresh else NULL,
          choicesOpt = list(
            icon = simuIcons,
            style = rep(sprintf("color: %s", dropDownColor), length(simuFolders))
          )
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
        
        pickedSimuPath <- file.path(logFolderPath, input$simuPicker)
        
        output$pickedSimuDisplay <- renderUI({
          doesSimuRun <- getSimuRunStatus(pickedSimuPath, topout)
          simuRunStatus <- ifelse(doesSimuRun, "R", "N")
            
          div(
            align = "center",
            h5(span(icons[[simuRunStatus]], input$simuPicker))
          )
        })
        
        updateTabItems(topLevelSession, "tabs", "simuMonitorTab")
        
        pickedSimu(pickedSimuPath)
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
