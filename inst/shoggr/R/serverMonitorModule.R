serverMonitorModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      shinydashboard::valueBoxOutput(ns("ramBox")),
      shinydashboard::valueBoxOutput(ns("swapBox")),
      shinydashboard::valueBoxOutput(ns("cpuBox"))
    ),
    fluidRow(
      shinydashboard::box(
        title = "Top Call",
        width = 12,
        div(DT::dataTableOutput(ns("topTable")), style = "font-size:85%")
      )
    )
  )
}

serverMonitorModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      top_reactive <- reactiveVal(top())
      
      # observe({
      #   invalidateLater(10000, session)
      #   top_reactive(top())
      # })
      
      output$ramBox <- shinydashboard::renderValueBox({
        makeMemoryBox("RAM", total = top_reactive()$mem_df$mib[1], used = top_reactive()$mem_df$mib[3])
      })
      
      output$swapBox <- shinydashboard::renderValueBox({
        makeMemoryBox("Swap", total = top_reactive()$mem_df$mib[5], used = top_reactive()$mem_df$mib[7])
      })
      
      output$cpuBox <- shinydashboard::renderValueBox({
        makeCPUBox(32, sum(top_reactive()$procs_df$`%CPU` > 90))
      })
      
      output$topTable <- DT::renderDataTable(
        DT::formatRound(
          DT::datatable(
            top_reactive()$procs_df,
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
    }
  )
}
