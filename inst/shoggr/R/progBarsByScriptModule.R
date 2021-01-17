progBarsByScriptUI <- function(id, thisScriptOutInfos, thisScriptStarts, thisScriptFinishedIters, scriptProcessStati, scriptTime) {
  ns <- NS(id)

  scriptVars <- names(thisScriptOutInfos$iterators)
  progBarsToMake <- scriptVars[sapply(thisScriptOutInfos$iterators, base::length) > 1]
  iterCounts <- thisScriptOutInfos$iterCounts
  scriptProcessStatusBool <- scriptProcessStati[[scriptTime]] != "N"

  lapply(
    progBarsToMake,
    function(iteratorName) {
      currentIter <- isolate(thisScriptStarts[[iteratorName]])
      iteratorTotal <- iterCounts[[iteratorName]]
      flog.info(paste("progBar", scriptTime, iteratorName, "scriptProcessStatus", scriptProcessStatusBool))

      progressBar(
        id = ns(paste0(scriptTime, ".", iteratorName)),
        title = iteratorName,
        value = currentIter,
        total = iteratorTotal,
        display_pct = FALSE,
        status = ifelse(thisScriptFinishedIters < thisScriptOutInfos$fullIterCount, "primary", "success"),
        striped = scriptProcessStatusBool,
        size = "xs"
      )
    }
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

            flog.info(paste("progBarsByScript", scriptTime, iteratorName))
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
