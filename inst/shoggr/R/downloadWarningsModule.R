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
      
      combinedErrTib <- reactiveVal()
      
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
            dataTableOutput(ns("errTib")),
            footer = tagList(
              downloadButton(ns("downloadWarnings"), "Download warnings in seperate files by grouping"),
              modalButton("Close")
            )
          )
        )
        
        output$errTib <- renderDataTable({
          DT::datatable(
            combinedErrFiles$tib %>%
              # select(-workerFile, -iterationCounter) %>%
              group_by_at(vars(c("condition", iteratorNames[1:which(iteratorNames == input$groupingLevelPicker)]))) %>%
              summarise("No. Warn." = n()) %>% 
              ungroup() %>% 
              pivot_wider(names_from = condition, values_from = `No. Warn.`, values_fill = 0),
            class = "compact"
          )
        })
        
        output$downloadWarnings <- downloadHandler(
          filename = function() {
            paste(scriptTime, "warnings", "zip", sep = ".")
          },
          content = function(fname) {
            warnPath <- file.path(pickedSimu, "warns")
            dir.create(warnPath, showWarnings = FALSE)
            
            writtenFiles <- combinedErrFiles$tib %>%
              rowwise %>% 
              mutate(seqs = list(seq(starts, ends))) %>% 
              # group_by_at(vars(c(iteratorNames, workerFile))) %>% 
              group_by_at(vars(c(iteratorNames[1:which(iteratorNames == input$groupingLevelPicker)], workerFile))) %>%
              summarise(lineNumbers = list(do.call(c, seqs)), .groups = "keep") %>% 
              summarise(linesToWrite = list(combinedErrFiles$rawErrFiles[[workerFile]][lineNumbers[[1]]]), .groups = "keep") %>% 
              ungroup(workerFile) %>%
              group_map(
                ~ {
                  fileName <- file.path(warnPath, paste0(paste0(names(.y), "_", .y[1, ], collapse = "."), ".txt"))
                  writeLines(
                    do.call(c, .x$linesToWrite),
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