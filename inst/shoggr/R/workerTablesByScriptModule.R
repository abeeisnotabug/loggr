library(kableExtra)
library(DT)
library(dplyr)
library(tidyr)

workerTablesByScriptUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("workerBox"))
}

workerTablesByScriptServer <- function(id, thisScriptWorkerStati, thisScriptWorkerProcessStati) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$workerBox <- renderUI({
        scriptPID <- sapply(
          isolate(reactiveValuesToList(thisScriptWorkerStati)),
          function(worker) {
            unique(
              sapply(
                worker,
                function(startEnd)
                  startEnd$loggrValues$parentPID
              )
            )
          }
        )
        
        nWorkers <- length(isolate(reactiveValuesToList(thisScriptWorkerStati)))
        
        # box(
        #   width = 7,
        #   title = paste(nWorkers, "workers to parent PID", unique(scriptPID)),
        #   status = "danger",
          div(DT::dataTableOutput(ns("workerTable")), style = "font-size:85%")
        # )
      })
      
      output$workerTable <- DT::renderDataTable({
        rawTibble <- lapply(
          names(reactiveValuesToList(thisScriptWorkerStati)),
          function(workerName) {
            lapply(
              thisScriptWorkerStati[[workerName]],
              function(startEnd) {
                tibble(S = as.character(icons[[thisScriptWorkerProcessStati[[workerName]]]])) %>% 
                  bind_cols(do.call(dplyr::tibble, startEnd$loggrValues)) %>% 
                  bind_cols(do.call(dplyr::tibble, startEnd$iteratorValues))
              }
            ) %>% bind_rows(.id = "status")
          }
        ) %>% bind_rows()
        
        sortby <- ifelse("start" %in% rawTibble$status, "start", "end")
        
        workerPIDorder <- rawTibble %>% 
          select(status, logTime, workerPID) %>% 
          mutate(logTime = ymd_hms(logTime)) %>% 
          pivot_wider(names_from = status, values_from = logTime) %>% 
          arrange(desc(eval(parse(text = sortby)))) %>% .$workerPID
        
        dom <- ifelse(nrow(rawTibble) <= 10, "t", "lpt")
        pageLen <- ifelse(nrow(rawTibble) <= 12, nrow(rawTibble), 12)
        
        rawTibble %>%
          select(-parentPID) %>% 
          rename(PID = workerPID) %>% 
          mutate(logTime = ymd_hms(logTime)) %>% 
          rename(iter = iterationCounter) %>% 
          relocate(c(PID, S), .before = everything()) %>% 
          arrange(PID, desc(status)) %>%
          arrange(match(PID, workerPIDorder)) %>% 
          group_by(PID) %>% 
          mutate(
            PID = if (n() == 2) c(PID[1], NA) else PID,
            S = if (n() == 2) c(S[1], NA) else S,
            logTime = as.character(logTime)
          ) %>%
          datatable(class = "compact", list(dom = dom, ordering = F, pageLength = pageLen), rownames = FALSE, escape = -2)
      })
    }
  )
}