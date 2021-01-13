makeSpeedObservers <- function(currentWorkerStati, scriptSpeeds, scriptOutInfos) {
  lapply(
    names(currentWorkerStati),
    function(scriptTime) {
      observeEvent(reactiveValuesToList(currentWorkerStati[[scriptTime]]), {
        flog.info(paste("makeSpeedObserver", scriptTime))
        
        thisScriptWorkerStati <- reactiveValuesToList(currentWorkerStati[[scriptTime]])
        
        currentTimeDiff <- unlist(
          lapply(
            thisScriptWorkerStati,
            function(worker) {
              if (!(is.null(worker$end$loggrValues$logTime) || is.null(worker$start$loggrValues$logTime))) {
                seconds(interval(ymd_hms(worker$end$loggrValues$logTime), ymd_hms(worker$start$loggrValues$logTime)))
              } else {
                NA
              }
            }
          )
        )
        
        currentIterCount <- do.call(
          base::sum,
          lapply(
            thisScriptWorkerStati,
            function(worker) {
              worker$end$loggrValues$iterationCounter
            }
          )
        )
        
        endTimes <- unlist(
          lapply(
            thisScriptWorkerStati,
            function(worker) {
              worker$end$loggrValues$logTime
            }
          )
        )
        
        startTimes <- unlist(
          lapply(
            thisScriptWorkerStati,
            function(worker) {
              worker$start$loggrValues$logTime
            }
          )
        )
        
        maxTime <- ifelse(is.null(startTimes), max(endTimes), max(startTimes))

        scriptSpeeds[[scriptTime]] <- list(
          overall = seconds(interval(ymd_hms(scriptTime), ymd_hms(maxTime))) / currentIterCount,
          current = mean(currentTimeDiff, na.rm = TRUE)
        )
      })
    }
  )
}