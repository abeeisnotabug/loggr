shinydashboard::dashboardPage(
  skin = "red",

  shinydashboard::dashboardHeader(
    title = "Simulation Monitor",
    titleWidth = 260
  ),

  shinydashboard::dashboardSidebar(
    collapsed = FALSE,
    width = 260,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem(
        text = "Server Monitor",
        tabName = "serverMonitor",
        icon = icon("dashboard"),
        selected = TRUE
      ),
      shinydashboard::menuItem(
        text = "Progress",
        tabName = "simuMonitor",
        icon = icon("database")
      ),
      hr(),
      simuPickerModuleUI("sidebarSimuPicker")
    )
  ),

  shinydashboard::dashboardBody(
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
    shinyjs::useShinyjs(),
    shinybusy::use_busy_bar(color = "#FFFFFF"),
    shinydashboard::tabItems(
      # shinydashboard::tabItem(
      #   tabName = "simuMonitor", simuMonitorModuleUI("simuMonitorModule")
      # ),
      shinydashboard::tabItem(
        tabName = "serverMonitor", serverMonitorModuleUI("serverMonitorModule")
      )
    )
  )
)
