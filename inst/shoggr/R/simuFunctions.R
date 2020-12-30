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

      fileMatrix[filesOfThisScript, ] %>%
        `colnames<-`(c("prefix", "callTime", "parentPID", "workerPID", "scriptFileName")) %>%
        as_tibble %>%
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

  lapply(
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
}

makeMonitors <- function(session, scriptInfos, simuPath, monitorPrefix = "w") {
  lapply(
    scriptInfos,
    function(script) {
      toMonitor <- makeSelfNamedVector(
        script$files %>%
          filter(prefix == monitorPrefix) %>%
          .$fileName
      )

      lapply(
        toMonitor,
        function(file) {
          reactiveFileReader(
            1000,
            session,
            file.path(simuPath, file),
            readLines
          )
        }
      )
    }
  )
}

getOverallIters <- function(scriptOutInfos) {
  sum(
    sapply(
      scriptOutInfos,
      `[[`,
      "fullIterCount"
    )
  )
}

makeInitialWorkerStati <- function(script) {
  workers <- script$files %>% filter(prefix == "w") %>% .$fileName
  initialWorkerStati <- replicate(length(workers), list(), simplify = FALSE)
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
