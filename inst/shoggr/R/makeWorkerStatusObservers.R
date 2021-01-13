makeWorkerStatusObservers <- function(workerOutMonitors, currentWorkerStati, scriptOutInfos) {
  lapply(
    names(workerOutMonitors),
    function(scriptTime) {
      lapply(
        names(workerOutMonitors[[scriptTime]]),
        function(workerOutName) {
          workerOutMonitor <- workerOutMonitors[[scriptTime]][[workerOutName]]

          observeEvent(workerOutMonitor(), {
            req(workerOutMonitor())

            currentWorkerStatus <- getCurrentWorkerStatus(workerOutMonitor())
            currentWorkerStati[[scriptTime]][[workerOutName]] <- currentWorkerStatus

            flog.info(paste("workerStatusObserver", scriptTime))
          })
        }
      )
    }
  )
}

getCurrentWorkerStatus <- function(workerLines) {
  loggrIndicator <- getOption("loggr.log.prefix")
  processedWorkerLines <- str_extract(workerLines, sprintf("(?<=%s)(.+)", loggrIndicator)) %>%
    str_split(";", 6)

  names(processedWorkerLines) <- sapply(processedWorkerLines, base::`[[`, 1)

  lapply(
    processedWorkerLines,
    function(processedWorkerLine)
      list(
        loggrValues = list(
          logTime = processedWorkerLine[2],
          parentPID = as.integer(processedWorkerLine[3]),
          workerPID = as.integer(processedWorkerLine[4]),
          iterationCounter = as.integer(processedWorkerLine[5])
        ),
        iteratorValues = eval_parse_text(sprintf("list(%s)", processedWorkerLine[6]))
      )
  )
}
