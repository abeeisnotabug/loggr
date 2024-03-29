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
      choices = NULL,
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

        doesSimuRun <- lapply(
          makeSelfNamedVector(logFolderContents),
          function(simuFolder) {
            getSimuRunStatus(file.path(logFolderPath, simuFolder), topout)
          }
        )

        doesSimuRun_vec <- unlist(doesSimuRun)
        simulationOrder <- c(which(doesSimuRun_vec), which(!doesSimuRun_vec))

        simuIcons <- ifelse(
          doesSimuRun,
          rep("glyphicon-play", length(doesSimuRun)),
          rep("glyphicon-stop", length(doesSimuRun))
        )

        updatePickerInput(
          session = session,
          inputId = "simuPicker",
          choices = logFolderContents[simulationOrder],
          selected = if (is.null(pickedSimuBeforeRefresh))
            NULL
          else if (pickedSimuBeforeRefresh %in% logFolderContents)
            pickedSimuBeforeRefresh
          else
            NULL,
          choicesOpt = list(
            icon = simuIcons[simulationOrder],
            style = rep(sprintf("color: %s", dropDownColor), length(logFolderContents))
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
