settingsMenuUI <- function(id) {
  ns <- NS(id)
  
  tags$li(
    class = "dropdown",
    style = "padding-right: 25px; padding-top: 8px",
    makeButton(ns("refreshTop"), "Call top", "redo")
  )
}

settingsMenuServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return(input)
    }
  )
}
