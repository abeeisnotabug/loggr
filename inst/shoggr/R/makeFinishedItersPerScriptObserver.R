makeFinishedItersPerScriptObserver <- function(finishedItersPerScript, scriptOutInfos, currentEnds) {
  observe({
    lapply(
      names(scriptOutInfos),
      function(scriptTime) {
        totalsVec <- unlist(scriptOutInfos[[scriptTime]]$iterCounts)
        currentsVec <- unlist(reactiveValuesToList(currentEnds[[scriptTime]]))[names(totalsVec)]
        
        flog.info(paste("mainProgBar", scriptTime))
        flog.debug(str(list(totalsVec, currentsVec)))
        
        finishedItersPerScript[[scriptTime]] <- countFinishedIters(currentsVec, totalsVec)
      }
    )
  })
}