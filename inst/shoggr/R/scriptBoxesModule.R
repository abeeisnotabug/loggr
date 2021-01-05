scriptBoxesUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("progBoxesByScript"))
}

scriptBoxesServer <- function(id, scriptOutInfos, currentStarts, currentWorkerStati, finishedItersPerScript, processStati, scriptSpeeds, pickedSimu) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$progBoxesByScript <- renderUI({
        req(scriptOutInfos)
        
        lapply(
          names(scriptOutInfos),
          function(scriptTime) {
            fluidRow(
              box(
                width = 12,
                status = "danger",
                scriptBoxHeaderUI(ns(paste0(scriptTime, ".scriptBoxHeader"))),
                # scriptStatsUI(ns(paste0(scriptTime, ".valueBoxes"))),
                # br(),
                column(
                  width = 5,
                  h5(tags$b("Script Stats:")),
                  scriptStatsUI(ns(paste0(scriptTime, ".scriptStats"))),
                  br(),
                  h5(tags$b("Progress Bars:")),
                  progBarsByScriptUI(
                    ns(paste0(scriptTime, ".progressBars")),
                    scriptOutInfos[[scriptTime]],
                    currentStarts[[scriptTime]],
                    isolate(finishedItersPerScript[[scriptTime]]),
                    processStati$script,
                    scriptTime
                  )
                ),
                column(
                  width = 7,
                  h5(tags$b("Workers:")),
                  workerTablesByScriptUI(ns(paste0(scriptTime, ".workerTable")))
                )
              )
            )
          }
        )
      })
      
      lapply(
        names(currentStarts),
        function(scriptTime) {
          scriptBoxHeaderServer(
            paste0(scriptTime, ".scriptBoxHeader"),
            scriptOutInfos[[scriptTime]],
            processStati$script,
            scriptTime,
            pickedSimu
          )
          scriptStatsServer(
            paste0(scriptTime, ".scriptStats"),
            scriptOutInfos[[scriptTime]],
            scriptSpeeds,
            finishedItersPerScript,
            scriptTime
          )
          progBarsByScriptServer(
            paste0(scriptTime, ".progressBars"),
            currentStarts[[scriptTime]],
            scriptOutInfos[[scriptTime]],
            scriptTime,
            finishedItersPerScript[[scriptTime]],
            processStati$script[[scriptTime]]
          )
          workerTablesByScriptServer(
            paste0(scriptTime, ".workerTable"),
            currentWorkerStati[[scriptTime]],
            processStati$workers[[scriptTime]]
          )
        }
      )
    }
  )
}