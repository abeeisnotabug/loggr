simuPickerModuleUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("simuPickerUI"))
}

simuPickerModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$simuPickerUI <- renderUI({
        ns <- session$ns
        shinyWidgets::pickerInput(
          ns("simuPicker"),
          label = "Pick a Simulation",
          choices = dir(loggr::log_folder),
          options = list(`live-search` = TRUE)
        )
      })
    }
  )
}
