library(shinybusy)

downloadWarningsUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("downloadWarningsButtonUIelement"))
}

downloadWarningsServer <- function(id, pickedSimu, errFiles, scriptTime, thisScriptOutInfos) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$downloadWarningsButtonUIelement <- renderUI({
        req(errFiles[[scriptTime]] %>% filter(prefix == "w") %>% .$fileName)
        
        actionBttn(
          inputId = ns("openWarningsModalButton"),
          label = "Warnings",
          color = "warning",
          icon = icon("download"),
          style = "unite",
          size = "sm"
        )
      })
      
      observeEvent(input$openWarningsModalButton, {
        iteratorNames <- names(thisScriptOutInfos$iterators)
        
        show_modal_spinner(
          spin = "cube-grid",
          color = "firebrick",
          text = "Fetching exceptions from all workers..."
        )
        combinedErrFiles <- fetchWarnings(errFiles[[scriptTime]], pickedSimu)
        remove_modal_spinner()
        
        showModal(
          modalDialog(
            title = "Download logged conditions",
            pickerInput(ns("groupingLevelPicker"), "Pick maximum level of grouping", choices = iteratorNames),
            materialSwitch(ns("noGroupingSwitch"), "Make single file for download", status = "danger", right = TRUE),
            dataTableOutput(ns("errTib")),
            footer = tagList(
              downloadButton(ns("downloadWarnings"), "Download split conditions"),
              modalButton("Close")
            )
          )
        )
        
        output$errTib <- renderDataTable({
          chosenIteratorPosition <- iteratorNames[1:which(iteratorNames == input$groupingLevelPicker)]
          
          DT::datatable(
            combinedErrFiles$tib %>%
              group_by_at(vars(all_of(c("condition", chosenIteratorPosition)))) %>%
              summarise("No. Warn." = n(), .groups = "drop") %>% 
              pivot_wider(names_from = condition, values_from = `No. Warn.`, values_fill = 0) %>% 
              arrange_at(vars(all_of(chosenIteratorPosition))),
            class = "compact"
          )
        })
        
        output$downloadWarnings <- downloadHandler(
          filename = function() {
            paste(basename(pickedSimu), "conditions", loggr:::get_time(0), "zip", sep = ".")
          },
          content = function(fname) {
            warnPath <- file.path(pickedSimu, "warns")
            dir.create(warnPath, showWarnings = FALSE)
            
            iteratorGrouping <- if (input$noGroupingSwitch) {
              "workerFile"
            } else {
              c(iteratorNames[1:which(iteratorNames == input$groupingLevelPicker)], "workerFile")
            }
            
            writtenFiles <- combinedErrFiles$tib %>%
              rowwise %>% 
              mutate(seqs = list(seq(starts, ends))) %>% 
              group_by_at(vars(all_of(iteratorGrouping))) %>%
              summarise(lineNumbers = list(do.call(base::c, seqs)), .groups = "keep") %>% 
              summarise(linesToWrite = list(combinedErrFiles$rawErrFiles[[workerFile]][lineNumbers[[1]]]), .groups = "keep") %>% 
              ungroup(workerFile) %>%
              group_map(
                ~ {
                  conditionFileName <- if (input$noGroupingSwitch) {
                    paste(basename(pickedSimu), "conditions", "txt", sep = ".")
                  } else {
                    paste0(paste0(names(.y), "_", .y[1, ], collapse = "."), ".txt")
                  }
                  fileName <- file.path(warnPath, conditionFileName)
                  writeLines(
                    do.call(base::c, .x$linesToWrite),
                    fileName
                  )
                  fileName
                }
              )
            
            zip(zipfile = fname, files = unlist(writtenFiles), extras = "-j")
          },
          contentType = "application/zip"
        )
      })
    }
  )
}