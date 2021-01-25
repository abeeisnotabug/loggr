scriptBoxHeaderUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("scriptBoxHeader"))
}

scriptBoxHeaderServer <- function(id, thisScriptOutInfos, thisScriptWorkerStati, scriptProcessStati, scriptTime, pickedSimu, errFiles) {
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
              style = "float: right",
              div(
                style = "display: inline-block",
                downloadWarningsUI(ns("downloadWarningsButton"))
              ),
              div(
                style = "display: inline-block",
                killCleanupUI(ns("killCleanupButton"))
              )
            )
          )
        )
      })
      
      killCleanupServer("killCleanupButton", thisScriptWorkerStati, pickedSimu, scriptProcessStati, scriptTime)
      downloadWarningsServer("downloadWarningsButton", pickedSimu, errFiles, scriptTime, thisScriptOutInfos)
    }
  )
}
