library(shinydashboard)
library(DT)

serverMonitorUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      valueBoxOutput(ns("ramBox")),
      valueBoxOutput(ns("swapBox")),
      valueBoxOutput(ns("cpuBox"))
    ),
    fluidRow(
      box(
        width = 12,
        title = "Top Call",
        status = "danger",
        div(DT::dataTableOutput(ns("topTable")), style = "font-size:85%")
      )
    )
  )
}

serverMonitorServer <- function(id, settingsInput) {
  moduleServer(
    id,
    function(input, output, session) {
      topReactive <- reactiveVal(top())

      # observe({
      #   invalidateLater(10000, session)
      #   topReactive(top())
      # })
      observeEvent(settingsInput$refreshTop, {
        topReactive(top())
      })

      output$ramBox <- renderValueBox({
        makeMemoryBox("RAM", total = topReactive()$mem_df$mib[1], used = topReactive()$mem_df$mib[3])
      })

      output$swapBox <- renderValueBox({
        makeMemoryBox("Swap", total = topReactive()$mem_df$mib[5], used = topReactive()$mem_df$mib[7])
      })

      output$cpuBox <- renderValueBox({
        makeCPUBox(32, sum(topReactive()$procs_df$`%CPU` > 90))
      })

      output$topTable <- DT::renderDataTable(
        DT::formatRound(
          DT::datatable(
            topReactive()$procs_df,
            class = "compact",
            options = list(
              columnDefs = list(
                list(
                  targets = 12,
                  render = DT::JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data != null && data.length > 40 ?",
                    "'<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;",
                    "}")
                )
              ),
              pageLength = 50
            )
          ),
          columns = paste(c("VIRT", "RES", "SHR"), "(MiB)"),
          digits = 1
        )
      )

      return(reactive(topReactive()))
    }
  )
}
