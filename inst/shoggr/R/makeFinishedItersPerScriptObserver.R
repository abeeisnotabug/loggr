makeFinishedItersPerScriptObserver <- function(finishedItersPerScript, scriptOutInfos, currentEnds, currentStarts) {
  lapply(
    names(scriptOutInfos),
    function(scriptTime) {
      observe({
        currentEndVec <- unlist(reactiveValuesToList(currentEnds[[scriptTime]]))
        
        totalsVec <- unlist(scriptOutInfos[[scriptTime]]$iterCounts)
        currentsVec <- if (0 %in% currentEndVec) {
          unlist(isolate(reactiveValuesToList(currentStarts[[scriptTime]])))[names(totalsVec)]
        } else {
          currentEndVec[names(totalsVec)]
        }

        flog.info(paste("finishedItersPerScript", scriptTime))
        
        finishedItersPerScript[[scriptTime]] <- countFinishedIters(currentsVec, totalsVec)
      })
    }
  )
}