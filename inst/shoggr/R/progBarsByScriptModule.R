progBarsByScriptUI <- function(id, thisScriptOutInfos, thisScriptStarts, scriptTime, thisScriptFinishedIters, thisScriptProcessStatus) {
  ns <- NS(id)
  
  scriptFileName <- unique(thisScriptOutInfos$files$scriptFileName)
  scriptTimeFmt <- ymd_hms(scriptTime)
  
  scriptVars <- names(thisScriptOutInfos$iterators)
  progBarsToMake <- scriptVars[sapply(thisScriptOutInfos$iterators, length) > 1]
  iterCounts <- thisScriptOutInfos$iterCounts
  
  thisIcon <- icons[[thisScriptProcessStatus]]
  
  box(
    width = 5,
    title = span(thisIcon, paste0(scriptFileName, " (", scriptTimeFmt, ")")),
    status = "danger",
    lapply(
      progBarsToMake,
      function(iteratorName) {
        currentIter <- isolate(thisScriptStarts[[iteratorName]])
        iteratorTotal <- iterCounts[[iteratorName]]
        
        progressBar(
          id = ns(paste0(scriptTime, ".", iteratorName)),
          title = iteratorName,
          value = currentIter,
          total = iteratorTotal,
          display_pct = FALSE,
          status = ifelse(thisScriptFinishedIters < thisScriptOutInfos$fullIterCount, "primary", "success"),
          striped = thisScriptProcessStatus != "N",
          size = "xs"
        )
      }
    )
  )
}

progBarsByScriptServer <- function(id, thisScriptStarts, thisScriptOutInfos, scriptTime, thisScriptFinishedIters, processStatus) {
  moduleServer(
    id,
    function(input, output, session) {
      lapply(
        names(thisScriptStarts),
        function(iteratorName) {
          observeEvent(thisScriptStarts[[iteratorName]], {
            currentIter <- thisScriptStarts[[iteratorName]]
            iteratorTotal <- thisScriptOutInfos$iterCounts[[iteratorName]]
            
            flog.info(paste("probBoxByScript", paste0(scriptTime, "-", iteratorName)))
            flog.debug(paste(currentIter, iteratorTotal))
            
            updateProgressBar(
              session = session,
              id = paste0(scriptTime, ".", iteratorName),
              value = currentIter,
              total = iteratorTotal,
              status = ifelse(thisScriptFinishedIters < thisScriptOutInfos$fullIterCount, "primary", "success")
            )
          })
        }
      )
    }
  )
}