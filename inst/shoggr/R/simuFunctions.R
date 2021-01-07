library(stringr)
library(lubridate)
library(dplyr)
library(loggr)

getSimuFiles <- function(simuPath, extension) {
  simuFiles <- str_subset(dir(simuPath), sprintf("(%s)$", extension))

  if (is.null(simuFiles)) return(NULL)

  fileMatrix <- str_split_fixed(simuFiles, "-", 5)
  scripts <- makeSelfNamedVector(unique(fileMatrix[,2]))

  lapply(
    scripts,
    function(script) {
      filesOfThisScript <- str_detect(fileMatrix[, 2], script)

      fileMatrix %>% 
        `colnames<-`(c("prefix", "callTime", "parentPID", "workerPID", "scriptFileName")) %>%
        as_tibble %>%
        filter(filesOfThisScript) %>%
        mutate(
          callTime = ymd_hms(callTime),
          parentPID = suppressWarnings(as.integer(parentPID)),
          workerPID = suppressWarnings(as.integer(workerPID)),
          scriptFileName = str_extract(scriptFileName, ".+(?=\\.(?!\\.))")
        ) %>%
        bind_cols(fileName = simuFiles[filesOfThisScript])
    }
  )
}

getInfo <- function(simuPath) {
  simuFiles <- getSimuFiles(simuPath, "out")

  info <- lapply(
    simuFiles,
    function(scriptFiles) {
      outLines <- readLines(
        file.path(
          simuPath,
          scriptFiles %>%
            filter(prefix == "s") %>%
            select(fileName)
        )
      )

      loggrIndicator <- getOption("loggr.log.prefix")

      infoLine <- str_subset(outLines, paste0("^", loggrIndicator))

      infos <- str_split_fixed(
        str_extract(infoLine, sprintf("(?<=%s)(.+)", loggrIndicator)),
        ";",
        4
      )
      
      iterators <- eval_parse_text(sprintf("list(%s)", infos[4]))
      iterCounts <- lapply(iterators, length)

      list(
        files = scriptFiles,
        call = eval_parse_text(infos[1]),
        callTime = infos[2],
        parentPID = as.integer(infos[3]),
        iterators = iterators,
        iterCounts = iterCounts,
        fullIterCount = prod(unlist(iterCounts))
      )
    }
  )
  
  info[sort(names(info), decreasing = TRUE)]
}

makeMonitors <- function(session, scriptOutInfos, simuPath, topout, monitorPrefix = "w") {
  lapply(
    scriptOutInfos,
    function(script) {
      workerInfo <- script$files %>%
        filter(prefix == monitorPrefix) %>%
        select(fileName, workerPID)

      lapply(
        makeSelfNamedVector(workerInfo$fileName),
        function(workerFileName) {
          workerFilePath <- file.path(simuPath, workerFileName)
          workerPID <- workerInfo %>% filter(fileName == workerFileName) %>% .$workerPID
          flog.debug(paste(workerFileName, workerPID %in% topout$procs_df$PID))
          if (workerPID %in% topout$procs_df$PID) {
            reactiveFileReader(
              60 * 1000,
              session,
              workerFilePath,
              readLines
            )
          } else {
            function() readLines(workerFilePath)
          }
        }
      )
    }
  )
}

getCombinedIterators <- function(scriptOutInfos) {
  allIterators <- lapply(
    scriptOutInfos,
    `[[`,
    "iterators"
  )
  
  names(allIterators) <- NULL
  cList <- do.call(c, allIterators)
  
  lapply(
    makeSelfNamedVector(unique(names(cList))),
    function(iteratorName) {
      unlist(unique(cList[which(names(cList) == iteratorName)]))
    }
  )
}

makeInitialWorkerStati <- function(script, initalState = list()) {
  workers <- script$files %>% filter(prefix == "w") %>% .$fileName
  initialWorkerStati <- replicate(length(workers), initalState, simplify = FALSE)
  names(initialWorkerStati) <- workers
  initialWorkerStati
}

makeInitialIterCounters <- function(script) {
  initialIters <- replicate(
    length(script$iterators),
    0,
    simplify = FALSE
  )
  names(initialIters) <- names(script$iterators)
  initialIters
}

getSimuRunStatus <- function(path, topout) {
  dir(path) %>% 
    str_subset("^(c-).*(.out)$") %>%
    str_split_fixed("-", 5) %>%
    .[,3] %>% 
    `%in%`(topout()$procs_df$PID) %>% 
    any
}

getErrFiles <- function(simuPath, errFilesRV) {
  errFilesByScript <- getSimuFiles(simuPath, "err")
  
  lapply(
    names(errFilesByScript),
    function(scriptTime) {
      errFilesRV[[scriptTime]] <- errFilesByScript[[scriptTime]]
    }
  )
}
