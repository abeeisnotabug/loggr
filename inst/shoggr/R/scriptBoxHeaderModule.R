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
              style = "float: right",
              actionBttn(
                inputId = ns(paste0(scriptTime, ".killBtn")),
                label = ifelse(scriptProcessStati[[scriptTime]] != "N", "Kill", "Clean up"),
                color = ifelse(scriptProcessStati[[scriptTime]] != "N", "danger", "primary"),
                icon = icon(ifelse(scriptProcessStati[[scriptTime]] != "N", "times", "broom")),
                style = "unite",
                size = "sm"
              )
            )
          )
        )
      })
      
      observeEvent(input[[paste0(scriptTime, ".killBtn")]], {
        if (scriptProcessStati[[scriptTime]] != "N") {
          ask_confirmation(
            inputId = ns("killConfirm"),
            type = "question",
            title = "Kill Confirmation",
            text = "Choose method to kill: Send interrupt signal to script process (softly) or send terminate signal to every worker (hard)",
            btn_labels = c("Soft", "Hard"),
            btn_colors = c("blue", "red"),
            showCloseButton = TRUE
          )
        } else {
          ask_confirmation(
            inputId = ns("killConfirm"),
            type = "warning",
            title = "Clean Up Confirmation",
            text = "Are you sure you want to delete all log files belonging to this script?",
            # btn_labels = c("Soft", "Hard"),
            # btn_colors = c("blue", "red"),
            showCloseButton = TRUE
          )
        }
      })
      
      observeEvent(input$killConfirm, {
        if (input$killConfirm) {
          if (scriptProcessStati[[scriptTime]] != "N") {
            workerPIDs <- sapply(
              reactiveValuesToList(thisScriptWorkerStati),
              function(workerName) {
                workerName[[1]]$loggrValues$workerPID
              }
            )
            
            killOut <- lapply(
              workerPIDs,
              function(workerPID) {
                system(sprintf("kill %i", workerPID))
              }
            )
            
            print(killOut)
            session$reload()
          } else {
            simuFiles <- dir(file.path(pickedSimu), full.names = TRUE)
            toDelete <- str_subset(simuFiles, scriptTime)
            
            unlink(toDelete)
            
            session$reload()
          }
        }
      })
    }
  )
}
