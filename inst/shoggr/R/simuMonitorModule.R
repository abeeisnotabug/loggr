library(dplyr)
require(futile.logger)

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

simuMonitorServer <- function(id, pickedSimu, topout) {
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
      
      currentStarts <- list()
      currentEnds <- list()
      currentWorkerStati <- list()
      finishedIters <- reactiveValues()
      finishedItersPerScript <- reactiveValues()
      processStati <- list(
        scripts = reactiveValues(),
        workers = list()
      )
      
      flog.info("Change")
      
      localOverallIters <- getOverallIters(scriptOutInfos)
      
      lapply(
        scriptOutInfos,
        function(script) {
          scriptTime <- script$callTime
          
          currentWorkerStati[[scriptTime]] <<- do.call(reactiveValues, makeInitialWorkerStati(script))
          currentStarts[[scriptTime]] <<- do.call(reactiveValues, makeInitialIterCounters(script))
          currentEnds[[scriptTime]] <<- do.call(reactiveValues, makeInitialIterCounters(script))
          processStati$scripts[[scriptTime]] <- "N"
          processStati$workers[[scriptTime]] <<- do.call(reactiveValues, makeInitialWorkerStati(script, "N"))
          finishedIters[[scriptTime]] <- 0
        }
      )
      
      workerOutMonitors <- makeMonitors(session, scriptOutInfos, pickedSimu, monitorPrefix = "w")
      makeWorkerStatusObservers(workerOutMonitors, currentWorkerStati, finishedIters, scriptOutInfos)
      makeProcessStatusObservers(scriptOutInfos, processStati, topout)
      makeCurrentIterUpdateObservers(currentWorkerStati, currentStarts, currentEnds, scriptOutInfos)
      makeFinishedItersPerScriptObserver(finishedItersPerScript, scriptOutInfos, currentEnds)
      mainProgressBarServer("mainProgBar", scriptOutInfos, localOverallIters, currentEnds, finishedItersPerScript, processStati)
      progressBoxesByScriptServer("progBoxesByScript", scriptOutInfos, currentStarts, currentWorkerStati, finishedItersPerScript, processStati)
    }
  )
}
