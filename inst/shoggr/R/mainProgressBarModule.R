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

        scriptAndProgressConsistency <- all(
          sapply(
            names(processStatiList$scripts),
            function(script)
              ifelse(
                processStatiList$scripts[[script]] == "N",
                all(processStatiList$workers[[script]] == "N"),
                ifelse(
                  processStatiList$scripts[[script]] %in% c("R", "S"),
                  all(processStatiList$workers[[script]] %in% c("R", "S")),
                  FALSE
                )
              )
          )
        )

        overallIcon <- if (scriptAndProgressConsistency)
          if (any(unlist(processStatiList$scripts) %in% c("R", "S")))
            icons$R
          else
            icons$N
        else
          icons$failure

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
              striped = any(unlist(processStatiList$scripts) %in% c("R", "S")),
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
