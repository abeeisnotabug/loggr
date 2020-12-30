progBarsByScriptUI <- function(id, thisScriptOutInfos, thisScriptStarts, scriptTime) {
  ns <- NS(id)
  
  scriptFileName <- unique(thisScriptOutInfos$files$scriptFileName)
  scriptTimeFmt <- ymd_hms(scriptTime)
  
  scriptVars <- names(thisScriptOutInfos$iterators)
  progBarsToMake <- scriptVars[sapply(thisScriptOutInfos$iterators, length) > 1]
  iterCounts <- thisScriptOutInfos$iterCounts
  
  box(
    width = 5,
    title = paste0(scriptFileName, " (", scriptTimeFmt, ")"),
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
          status = "primary",
          striped = FALSE,
          size = "xs"
        )
      }
    )
  )
}

progBarsByScriptServer <- function(id, thisScriptStarts, thisScriptOutInfos, scriptTime) {
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
              status = "primary"
            )
          })
        }
      )
    }
  )
}