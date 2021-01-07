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

simuMonitorServer <- function(id, pickedSimu, topout, settingsInput) {
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
            scriptBoxesUI(ns("progBoxesByScript"))
          )
        )
      })

      flog.info(paste("Monitor", pickedSimu))

      scriptOutInfos <- getInfo(pickedSimu)
      errFiles <- reactiveValues()
      getErrFiles(pickedSimu, errFiles)

      observeEvent(settingsInput$refreshFiles, {
        getErrFiles(pickedSimu, errFiles)
      })

      currentStarts <- list()
      currentEnds <- list()
      currentWorkerStati <- list()

      scriptSpeeds <- reactiveValues()
      finishedItersPerScript <- reactiveValues()
      processStati <- list(
        scripts = reactiveValues(),
        workers = list()
      )

      flog.info("simuMonitorModuleSERVER")

      combinedIterators <- getCombinedIterators(scriptOutInfos)

      lapply(
        scriptOutInfos,
        function(script) {
          scriptTime <- script$callTime

          currentWorkerStati[[scriptTime]] <<- do.call(reactiveValues, makeInitialWorkerStati(script))
          currentStarts[[scriptTime]] <<- do.call(reactiveValues, makeInitialIterCounters(script))
          currentEnds[[scriptTime]] <<- do.call(reactiveValues, makeInitialIterCounters(script))
          processStati$scripts[[scriptTime]] <- "N"
          processStati$workers[[scriptTime]] <<- do.call(reactiveValues, makeInitialWorkerStati(script, "N"))
          scriptSpeeds[[scriptTime]] <- list(overall = NA, current = NA)
        }
      )

      workerOutMonitors <- makeMonitors(session, scriptOutInfos, pickedSimu, isolate(topout()), monitorPrefix = "w")
      makeWorkerStatusObservers(workerOutMonitors, currentWorkerStati, scriptOutInfos)
      makeProcessStatusObservers(scriptOutInfos, processStati, topout)
      makeSpeedObservers(currentWorkerStati, scriptSpeeds, scriptOutInfos)
      makeCurrentIterUpdateObservers(currentWorkerStati, currentStarts, currentEnds, scriptOutInfos)
      makeFinishedItersPerScriptObserver(finishedItersPerScript, scriptOutInfos, currentEnds, currentStarts)
      mainProgressBarServer("mainProgBar", scriptOutInfos, combinedIterators, currentEnds, finishedItersPerScript, processStati)
      scriptBoxesServer("progBoxesByScript", scriptOutInfos, currentStarts, currentWorkerStati, finishedItersPerScript, processStati, scriptSpeeds, pickedSimu, errFiles)
    }
  )
}
