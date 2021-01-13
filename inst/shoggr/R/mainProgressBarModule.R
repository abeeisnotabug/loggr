library(shinydashboard)
library(shinyWidgets)

mainProgressBarUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("mainProgressBarBox"))
}

mainProgressBarServer <- function(id, scriptOutInfos, combinedIterators, currentEnds, finishedItersPerScript, processStati) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      overallIters <- prod(sapply(combinedIterators, base::length))

      output$mainProgressBarBox <- renderUI({
        req(reactiveValuesToList(finishedItersPerScript))

        flog.info("mainProgBarUI")

        nonDuplicatedScripts <- names(scriptOutInfos)[
          !duplicated(
            lapply(
              scriptOutInfos,
              base::`[[`,
              "iterators"
            )
          )
        ]

        localFinishedItersPerScript <- isolate(reactiveValuesToList(finishedItersPerScript))

        finishedItersSum <- isolate(do.call(base::sum, localFinishedItersPerScript[nonDuplicatedScripts]))

        processStatiList <- list(
          scripts = reactiveValuesToList(processStati$scripts),
          workers = lapply(processStati$workers, shiny::reactiveValuesToList)
        )
        allProcessStati <- unlist(processStatiList)
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
              value = finishedItersSum,
              total = overallIters,
              display_pct = TRUE,
              status = ifelse(finishedItersSum < overallIters, "primary", "success"),
              striped = all(allProcessStati != "N"),
              title = span(overallIcon, "Overall progress")
            )
          )
        )
      })

      observe({
        nonDuplicatedScripts <- names(scriptOutInfos)[
          !duplicated(
            lapply(
              scriptOutInfos,
              base::`[[`,
              "iterators"
            )
          )
        ]

        localFinishedItersPerScript <- isolate(reactiveValuesToList(finishedItersPerScript))

        finishedItersSum <- isolate(do.call(base::sum, localFinishedItersPerScript[nonDuplicatedScripts]))

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
