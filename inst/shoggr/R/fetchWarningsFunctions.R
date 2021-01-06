fetchWarnings <- function(files, simuPath) {
  rawErrFiles <- lapply(
    makeSelfNamedVector(files %>% filter(prefix == "w") %>% .$fileName),
    function(fileName) {
      readLines(file.path(simuPath, fileName))
    }
  )
  
  saveRDS(rawErrFiles, "rawErrFiles.rds")
  
  loggrIndicator <- getOption("loggr.log.prefix")

  tib <- lapply(
    rawErrFiles,
    function(errFile) {
      errLineStarts <- str_which(errFile, loggrIndicator)
      errLineEnds <- c(errLineStarts[-1] - 1L, length(errFile))
      
      rawSplitLines <- str_split_fixed(
        str_extract(errFile[errLineStarts], sprintf("(?<=%s)(.+)", loggrIndicator)),
        ";",
        6
      )
      
      rawSplitLines[, 1:5] %>% 
        `colnames<-`(c("condition", "logTime", "parentPID", "workerPID", "iterationCounter")) %>% 
        as_tibble %>% 
        bind_cols(
          lapply(
            rawSplitLines[, 6],
            function(iteratorLine) {
              eval_parse_text(sprintf("list(%s)", iteratorLine)) %>% bind_cols
            }
          ) %>% bind_rows
        ) %>% 
        bind_cols(starts = errLineStarts, ends = errLineEnds)
    }
  ) %>% 
    bind_rows(.id = "workerFile")
  
  print(tib)
  
  list(rawErrFiles = rawErrFiles, tib = tib)
}
