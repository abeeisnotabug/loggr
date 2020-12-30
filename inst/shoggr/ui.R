library(shinydashboard)
library(shinyjs)

dashboardPage(
  skin = "red",

  dashboardHeader(
    title = "Simulation Monitor",
    titleWidth = 260
  ),

  dashboardSidebar(
    collapsed = FALSE,
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Server Monitor",
        tabName = "serverMonitorTab",
        icon = icon("dashboard"),
        selected = TRUE
      ),
      menuItem(
        text = "Progress",
        tabName = "simuMonitorTab",
        icon = icon("database")
      ),
      hr(),
      simuPickerUI("sidebarSimuPicker"),
      hr(),
      makeRefreshButton("refreshPage", "Clear and reload", "redo")
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(
        ".nav-tabs-custom .nav-tabs li.active { border-top-color: #d73925 }"
      ),
      tags$style(
        type = "text/css",
        "#inline label{ display: table-cell; text-align: center; vertical-align: middle; color: white }
         #inline .form-group { display: table-row }"
      )
    ),
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "simuMonitorTab", simuMonitorUI("simuMonitor")
      ),
      tabItem(
        tabName = "serverMonitorTab", serverMonitorUI("serverMonitor")
      )
    )
  )
)
