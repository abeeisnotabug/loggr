library(kableExtra)

scriptStatsUI <- function(id) {
  ns <- NS(id)
  
  div(
    tags$style("td { padding-right: 15px }"),
    tableOutput(ns("scriptStatsTable"))
  )
}

scriptStatsServer <- function(id, thisScriptOutInfos, scriptSpeeds, finishedItersPerScript, scriptTime) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(scriptSpeeds[[scriptTime]], {
        thisScriptSpeed <- scriptSpeeds[[scriptTime]]
        thisScriptFinishedIters <- finishedItersPerScript[[scriptTime]]
        flog.info("scriptStatsSERVER")
        
        output$scriptStatsTable <- function() {
          nWorkers <- thisScriptOutInfos$files %>% filter(prefix == "w") %>% nrow
          
          overallSpeed <- thisScriptSpeed$overall
          currentSpeed <- thisScriptSpeed$current
          
          iterationsLeft <- thisScriptOutInfos$fullIterCount - thisScriptFinishedIters
          
          scriptStatsIcons <- sapply(
            c(
              "database",
              "microchip",
              rep("tachometer-alt", if (!is.nan(currentSpeed)) 4 else 2),
              rep("hourglass-half", if (iterationsLeft) if (!is.nan(currentSpeed)) 2 else 1 else 0)
            ),
            function(iconName) {
              as.character(icon(iconName))
            }
          )
          
          scriptStatsNames <- paste0(
            c(
              "PID",
              "Workers",
              "Overall speed",
              if (!is.nan(currentSpeed)) "Current speed",
              "Overall speed by worker",
              if (!is.nan(currentSpeed)) "Current speed by worker",
              if (iterationsLeft) 
                c(
                  "ETR (based on overall)",
                  if (!is.nan(currentSpeed)) "ETR (based on current)"
                )
            ),
            ":"
          )
          
          scriptStats <- c(
            thisScriptOutInfos$parentPID,
            nWorkers,
            roundSecondsToPeriod(overallSpeed, "/Iteration"),
            if (!is.nan(currentSpeed)) roundSecondsToPeriod(currentSpeed / nWorkers, "/Iteration"),
            roundSecondsToPeriod(overallSpeed * nWorkers, "/Iteration"),
            if (!is.nan(currentSpeed)) roundSecondsToPeriod(currentSpeed, "/Iteration"),
            if (iterationsLeft)
              c(
                roundSecondsToPeriod(iterationsLeft * overallSpeed),
                if (!is.nan(currentSpeed)) roundSecondsToPeriod(iterationsLeft * currentSpeed / nWorkers)
              )
          )
          
          kable(
            cbind(scriptStatsIcons, scriptStatsNames, scriptStats),
            format = "html", col.names = NULL, row.names = FALSE, escape = FALSE, padding = 10
          )
        }
      })
    }
  )
}