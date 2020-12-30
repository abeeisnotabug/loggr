library(kableExtra)
library(DT)
library(sortable)

workerTablesByScriptUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("workerBox"))
}

workerTablesByScriptServer <- function(id, thisScriptWorkerStati) {
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
          # sortable_output(ns("orderSorter")),
          div(DT::dataTableOutput(ns("workerTable")), style = "font-size:85%")
        )
      })
      
      # output$orderSorter <- render_sortable({
      #   rank_list(labels = c("fu", "fa"), input_id = ns("orderSorterList"))
      # })
      
      output$workerTable <- DT::renderDataTable({
        rawTibble <- lapply(
          reactiveValuesToList(thisScriptWorkerStati),
          function(worker) {
            lapply(
              worker,
              function(startEnd) {
                do.call(tibble, startEnd$loggrValues) %>% 
                  bind_cols(do.call(tibble, startEnd$iteratorValues))
              }
            ) %>% bind_rows(.id = "status")
          }
        ) %>% bind_rows()
        
        rawTibble %>%
          select(-parentPID) %>% 
          mutate(logTime = ymd_hms(logTime)) %>% 
          rename(iter = iterationCounter) %>% 
          relocate(workerPID, .before = everything()) %>% 
          arrange(desc(logTime), workerPID, desc(status)) %>%
          group_by(workerPID) %>% 
          mutate(workerPID = if (n() == 2) c(workerPID[1], NA) else workerPID) %>%
          mutate(logTime = as.character(logTime)) %>% 
          datatable(class = "compact", list(dom = "lpt", ordering = F), rownames = FALSE)# %>% 
          # formatDate(3, "toLocaleString")
      })
    }
  )
}