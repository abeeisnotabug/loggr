simuMonitorModuleUI <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      shinydashboard::infoBox("Scripts"),
      shinydashboard::infoBox("Workers")
    )
  )
}

simuMonitorModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}
