makeCurrentIterUpdateObservers <- function(currentWorkerStati, currentStarts, currentEnds, scriptOutInfos, session) {
  lapply(
    names(currentWorkerStati),
    function(scriptTime) {
      lapply(
        names(currentWorkerStati[[scriptTime]]),
        function(currentWorkerName) {
          observeEvent(currentWorkerStati[[scriptTime]][[currentWorkerName]], {
            currentWorkerStatus <- currentWorkerStati[[scriptTime]][[currentWorkerName]]
            currentWorkerStatiScriptList <- reactiveValuesToList(currentWorkerStati[[scriptTime]])

            req(currentWorkerStatiScriptList)

            flog.info(paste("currentIterUpdateObserver", scriptTime))

            workerProgress <- lapply(
              currentWorkerStatus,
              function(startEnd) {
                sapply(
                  names(startEnd$iteratorValues),
                  function(iteratorName) {
                    whichStep <- which(
                      scriptOutInfos[[scriptTime]]$iterators[[iteratorName]] == startEnd$iteratorValues[[iteratorName]]
                    )

                    if (length(whichStep) < 1) {
                      shinyWidgets::sendSweetAlert(
                        session = session,
                        title = "Iterator not initialized",
                        text = sprintf("The value '%s' was not initialized for the iterator '%s'. Appending value.
                                       Progress and time calculations will be wrong. Please repair the file %s",
                                       startEnd$iteratorValues[[iteratorName]], iteratorName,
                                       scriptOutInfos[[scriptTime]]$files %>% filter(prefix == "s") %>% .$fileName),
                        type = "warning"
                      )

                      scriptOutInfos[[scriptTime]]$iterators[[iteratorName]] <<- c(
                        scriptOutInfos[[scriptTime]]$iterators[[iteratorName]], startEnd$iteratorValues[[iteratorName]]
                      )

                      length(scriptOutInfos[[scriptTime]]$iterators[[iteratorName]])
                    } else {
                      whichStep
                    }
                  }
                )
              }
            )

            flog.debug(str(workerProgress))

            hasStartIteratorsToUpdate <- if (!is.null(currentWorkerStatus$start)) {
              checkIfIteredMore(
                scriptOutInfos[[scriptTime]]$iterCounts,
                reactiveValuesToList(currentStarts[[scriptTime]]),
                workerProgress$start
              )
            } else {
              FALSE
            }

            hasEndIteratorsToUpdate <- if (!is.null(currentWorkerStatus$end)) {
              checkIfIteredMore(
                scriptOutInfos[[scriptTime]]$iterCounts,
                reactiveValuesToList(currentEnds[[scriptTime]]),
                workerProgress$end
              )
            } else {
              FALSE
            }

            allEnds <- !"start" %in% unlist(lapply(currentWorkerStatiScriptList, base::names))

            if (hasStartIteratorsToUpdate) {
              lapply(
                names(workerProgress$start),
                function(iteratorToUpdate) {
                  currentStarts[[scriptTime]][[iteratorToUpdate]] <- workerProgress$start[[iteratorToUpdate]]
                }
              )
            } else if (allEnds && hasEndIteratorsToUpdate) {
              lapply(
                names(workerProgress$end),
                function(iteratorToUpdate) {
                  currentStarts[[scriptTime]][[iteratorToUpdate]] <- workerProgress$end[[iteratorToUpdate]]
                }
              )
            }

            if (hasEndIteratorsToUpdate) {
              lapply(
                names(workerProgress$end),
                function(iteratorToUpdate) {
                  currentEnds[[scriptTime]][[iteratorToUpdate]] <- workerProgress$end[[iteratorToUpdate]]
                }
              )
            }

            flog.debug(str(reactiveValuesToList(currentStarts[[scriptTime]])))
            flog.debug(str(reactiveValuesToList(currentEnds[[scriptTime]])))
          })
        }
      )
    }
  )
}

checkIfIteredMore <- function(iterCounts, currentIters, newCurrentIters) {
  totalsVec <- unlist(iterCounts)
  currentsVec <- unlist(currentIters)[names(totalsVec)]
  newCurrentsVec <- unlist(newCurrentIters)[names(totalsVec)]

  countFinishedIters(newCurrentsVec, totalsVec) > countFinishedIters(currentsVec, totalsVec)
}
