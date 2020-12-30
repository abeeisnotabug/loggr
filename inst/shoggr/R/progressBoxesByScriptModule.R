progressBoxesByScriptUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("progBoxesByScript"))
}

progressBoxesByScriptServer <- function(id, scriptOutInfos, currentStarts, currentWorkerStati) {
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
              progBarsByScriptUI(ns(paste0(scriptTime, ".progBarBox")), scriptOutInfos[[scriptTime]], currentStarts[[scriptTime]], scriptTime),
              workerTablesByScriptUI(ns(paste0(scriptTime, ".workerTableBox")))
            )
          }
        )
      })
      
      lapply(
        names(currentStarts),
        function(scriptTime) {
          progBarsByScriptServer(paste0(scriptTime, ".progBarBox"), currentStarts[[scriptTime]], scriptOutInfos[[scriptTime]], scriptTime)
          workerTablesByScriptServer(paste0(scriptTime, ".workerTableBox"), currentWorkerStati[[scriptTime]])
        }
      )
    }
  )
}