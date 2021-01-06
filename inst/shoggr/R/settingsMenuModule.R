settingsMenuUI <- function(id) {
  ns <- NS(id)
  
  tags$li(
    class = "dropdown",
    tags$li(
      class = "dropdown",
      style = "padding-right: 40px; padding-top: 8px",
      makeButton(ns("refreshTop"), "Call top", "redo")
    ),
    tags$li(
      class = "dropdown",
      style = "padding-right: 25px; padding-top: 8px",
      makeButton(ns("refreshFiles"), "Refresh Files", "redo")
    )
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
