scriptBoxHeaderUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("scriptBoxHeader"))
}

scriptBoxHeaderServer <- function(id, thisScriptOutInfos, scriptProcessStati, scriptTime, pickedSimu) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$scriptBoxHeader <- renderUI({
        scriptFileName <- unique(thisScriptOutInfos$files$scriptFileName)
        scriptTimeFmt <- ymd_hms(scriptTime)

        tagList(
          splitLayout(
            cellWidths = c("80%", "20%"),
            h4(
              span(icons[[scriptProcessStati[[scriptTime]]]]),
              tags$b("Script: "),
              span(sprintf("%s (%s) ", scriptFileName, scriptTimeFmt), style = paste("color:", "grey"))
            ),
            div(
              style = "float: left",
              splitLayout(
                cellWidths = c("50%", "50%"),
                killCleanupUI(ns("killCleanupButton")),
                downloadWarningsUI(ns("downloadWarningsButton"))
              )
            )
          )
        )
      })
      
      killCleanupServer("killCleanupButton", thisScriptWorkerStati, pickedSimu, scriptProcessStati, scriptTime)
      downloadWarningsServer("downloadWarningsButton")
    }
  )
}
