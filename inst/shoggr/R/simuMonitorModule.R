library(dplyr)
library(futile.logger)

flog.threshold(WARN)

simuMonitorUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    div(
      id = ns("pickSimuInfoText"),
      h4("Pick a simulation to monitor on the left.")
    ),
    uiOutput(ns("monitorPage")) 
  )
}

simuMonitorServer <- function(id, pickedSimu) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$monitorPage <- renderUI({
        hide("pickSimuInfoText")
        
        tagList(
          fluidRow(
            mainProgressBarUI(ns("mainProgBar"))
          ),
          fluidRow(
            progressBoxesByScriptUI(ns("progBoxesByScript"))
          )
        )
      })
      
      flog.info(paste("Monitor", pickedSimu))
      scriptOutInfos <- getInfo(pickedSimu)
      saveRDS(scriptOutInfos, file = "scriptOutInfos.rds")
      
      finishedIters <- reactiveValues()
      currentStarts <- list()
      currentEnds <- list()
      currentWorkerStati <- list()
      finishedItersPerScript <- reactiveValues()
      
      flog.info("Change")
      
      localOverallIters <- getOverallIters(scriptOutInfos)
      
      lapply(
        scriptOutInfos,
        function(script) {
          scriptTime <- script$callTime
          
          currentWorkerStati[[scriptTime]] <<- do.call(reactiveValues, makeInitialWorkerStati(script))
          currentStarts[[scriptTime]] <<- do.call(reactiveValues, makeInitialIterCounters(script))
          currentEnds[[scriptTime]] <<- do.call(reactiveValues, makeInitialIterCounters(script))
          finishedIters[[scriptTime]] <- 0
        }
      )
      
      workerOutMonitors <- makeMonitors(session, scriptOutInfos, pickedSimu, monitorPrefix = "w")
      makeWorkerStatusObservers(workerOutMonitors, currentWorkerStati, finishedIters, scriptOutInfos)
      makeCurrentIterUpdateObservers(currentWorkerStati, currentStarts, currentEnds, scriptOutInfos)
      makeFinishedItersPerScriptObserver(finishedItersPerScript, scriptOutInfos, currentEnds)
      mainProgressBarServer("mainProgBar", scriptOutInfos, localOverallIters, currentEnds, finishedItersPerScript)
      progressBoxesByScriptServer("progBoxesByScript", scriptOutInfos, currentStarts, currentWorkerStati)
    }
  )
}
