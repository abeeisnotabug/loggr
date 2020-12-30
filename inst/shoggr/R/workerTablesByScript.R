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
          reactiveValuesToList(thisScriptWorkerStati),
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
        
        box(
          width = 7,
          title = paste("Workers to Parent PID", unique(scriptPID)),
          status = "danger",
          div(DT::dataTableOutput(ns("workerTable")), style = "font-size:85%")
        )
      })
      
      output$workerTable <- DT::renderDataTable({
        rawTibble <- lapply(
          names(reactiveValuesToList(thisScriptWorkerStati)),
          function(workerName) {
            lapply(
              thisScriptWorkerStati[[workerName]],
              function(startEnd) {
                tibble(S = as.character(icons[[thisScriptWorkerProcessStati[[workerName]]]])) %>% 
                  bind_cols(do.call(tibble, startEnd$loggrValues)) %>% 
                  bind_cols(do.call(tibble, startEnd$iteratorValues))
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
        
        rawTibble %>%
          select(-parentPID) %>% 
          mutate(logTime = ymd_hms(logTime)) %>% 
          rename(iter = iterationCounter) %>% 
          relocate(c(workerPID, S), .before = everything()) %>% 
          arrange(workerPID, desc(status)) %>%
          arrange(match(workerPID, workerPIDorder)) %>% 
          group_by(workerPID) %>% 
          mutate(
            workerPID = if (n() == 2) c(workerPID[1], NA) else workerPID,
            S = if (n() == 2) c(S[1], NA) else S,
            logTime = as.character(logTime)
          ) %>%
          datatable(class = "compact", list(dom = "t", ordering = F, pageLength = nrow(.)), rownames = FALSE, escape = -2)
      })
    }
  )
}