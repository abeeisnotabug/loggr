simuPickerModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::pickerInput(
      inputId = ns("simuPicker"),
      label = "Pick a Simulation",
      choices = dir(loggr::log_folder),
      options = list(`live-search` = TRUE)
    )
  )
  
}

simuPickerModuleServer <- function(id, logFolderContents) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(logFolderContents(), ignoreInit = TRUE, {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "simuPicker",
          choices = logFolderContents()
        )
      })
    }
  )
}
