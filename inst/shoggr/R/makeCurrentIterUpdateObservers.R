makeCurrentIterUpdateObservers <- function(currentWorkerStati, currentStarts, currentEnds, scriptOutInfos) {
  lapply(
    names(currentWorkerStati),
    function(scriptTime) {
      lapply(
        names(currentWorkerStati[[scriptTime]]),
        function(currentWorkerName) {
          observeEvent(currentWorkerStati[[scriptTime]][[currentWorkerName]], {
            currentWorkerStatus <- currentWorkerStati[[scriptTime]][[currentWorkerName]]
            
            req(reactiveValuesToList(currentWorkerStati[[scriptTime]]))
            
            flog.info(paste("currentIterUpdateObserver", scriptTime))
            
            workerProgress <- lapply(
              currentWorkerStatus,
              function(startEnd) {
                sapply(
                  names(startEnd$iteratorValues),
                  function(iteratorName) {
                    which(
                      scriptOutInfos[[scriptTime]]$iterators[[iteratorName]] == startEnd$iteratorValues[[iteratorName]]
                    )
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
            
            allEnds <- !"start" %in% unlist(lapply(reactiveValuesToList(currentWorkerStati[[scriptTime]]), names))
            
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