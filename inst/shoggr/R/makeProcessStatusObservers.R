makeProcessStatusObservers <- function(scriptOutInfos, processStati, topout) {
  observe({
    lapply(
      scriptOutInfos,
      function(script) {
        observe({
          thisS <- topout()$procs_df$S[which(topout()$procs_df$PID == script$parentPID)]
          
          processStati$scripts[[script$callTime]] <- ifelse(length(thisS), thisS, "N")
          
          flog.info(paste("processStatusObserver", script$callTime))
        })
        
        lapply(
          script$files %>% filter(prefix == "w") %>% .$fileName,
          function(worker) {
            observe({
              thisWorkerPID <- script$files %>% filter(fileName == worker) %>% .$workerPID
              
              thisS <- topout()$procs_df$S[which(topout()$procs_df$PID == thisWorkerPID)]
              
              processStati$workers[[script$callTime]][[worker]] <- ifelse(length(thisS), thisS, "N")
              
              flog.info(paste("processStatusObserver", worker))
            })
          }
        )
      }
    )
  })
}