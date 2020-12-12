simuMonitorModuleUI <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      shinydashboard::box(
        shinyFiles::shinyDirButton(
          id = ns("chooseDir"),
          label = "Pick Simulation",
          title = "Pick a Simulation to Monitor",
          multiple = FALSE
        )
      )
    )
  )
}

simuMonitorModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      shinyFiles::shinyDirChoose(
        input,
        "chooseDir",
        updateFreq = 0,
        roots = c(Scotty = "/data/sim/", simu_logs = loggr::log_folder),
        defaultRoot = "simu_logs",
        allowDirCreate = FALSE
      )
    }
  )
}

##### The same but with a list from shinyWidgets #####
# simuMonitorModuleUI <- function(id) {
#   ns <- shiny::NS(id)
# 
#   tagList(
#     fluidRow(
#       shinydashboard::box(
#         uiOutput(ns("dirPicker"))
#       )
#     )
#   )
# }
# 
# simuMonitorModuleServer <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       output$dirPicker <- renderUI({
#         ns <- session$ns
#         shinyWidgets::pickerInput(
#           ns("simuPicker"),
#           label = "Pick Simulation To Monitor",
#           choices = dir(loggr::log_folder),
#           options = list(`live-search` = TRUE)
#         )
#       })
#     }
#   )
# }
