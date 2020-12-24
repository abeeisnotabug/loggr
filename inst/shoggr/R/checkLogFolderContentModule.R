checkLogFolderContentServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      bing <- reactiveVal(1)
      
      observe({
        invalidateLater(10000, session)
        
        old_content <- logFolderContents()
        new_content <- dir(loggr::log_folder)
        
        if (!identical(old_content, new_content)) {
          bing(bing() + 1)
        }
      })
      
      logFolderContents <- reactive({
        bing()
        dir(loggr::log_folder)
      })
      
      return(logFolderContents)
    }
  )
}