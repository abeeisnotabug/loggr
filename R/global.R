initialize_progress <- function(...) {
  command_args <- commandArgs()

  if ("--interactive" %in% command_args) {
    stop("This command must be run in an R script that is either sourced or run via 'Rscript' in the terminal.")
  }

  iterator_variables <- list(...)

  if (any("" %in% names(iterator_variables))) {
    stop("All arguments must be named with format iterator_name = iterator_values.")
  }

  script_pid <- Sys.getpid()

  if ("-e" %in% command_args) {
    Rscript <- FALSE
  } else {
    file_name <- basename(command_args[grepl("--file", command_args)])
    simu_name <- substring(command_args[grepl("--simu_name", command_args)], 13)

    Rscript <- TRUE
  }

  loggr:::cat_id(script_pid)

  loggr:::cat_cmd(command_args)

  loggr:::cat_vars(script_pid, iterator_variables)

  list(
    script_pid = script_pid,
    outfile = ifelse(
      Rscript,
      file.path(
        loggr::log_folder,
        simu_name,
        paste0(
          script_pid,
          "-", format(Sys.time(), "%y.%m.%d-%H.%M.%OS6"),
          "-", file_name,
          "-cluster.log"
        )
      ),
      paste0(script_pid, "-cluster.log")
    ),
    simu_name = ifelse(
      Rscript,
      simu_name,
      FALSE
    )
  )
}

log_progress <- function(..., loggr_object, expr) {
  call_args <- as.list(match.call(expand.dots = FALSE))
  iterators <- list(...)

  iterator_values <- ifelse(
    sapply(iterators, is.numeric) | sapply(iterators, is.logical),
    sprintf("%s", iterators),
    ifelse(
      sapply(iterators, is.character),
      sprintf("'%s'", iterators),
      sprintf("'%s'", sapply(iterators, capture.output))
    )
  )

  to_write <- quote(
    paste0(
      "#!iter;",
      timepoint,
      ";",
      format(Sys.time(), "%y.%m.%d-%H.%M.%OS6"),
      ";",
      loggr_object$script_pid, "-", Sys.getpid(),
      ";",
      paste(
        sprintf(
          "%s=%s",
          if (is.null(names(call_args$...))) {
            call_args$...
          } else {
            append(
              as.list(names(call_args$...)[names(call_args$...) != ""]),
              call_args$...[names(call_args$...) == ""]
            )
          },
          iterator_values
        ),
        collapse = ","
      )
    )
  )

  file_name <- paste0(loggr_object$script_pid, "-", Sys.getpid(), ".log")

  out_file <- ifelse(
    isFALSE(loggr_object$simu_name),
    file_name,
    file.path(
      loggr::log_folder,
      loggr_object$simu_name,
      file_name
    )
  )

  write(
    with(list(timepoint = "start"), eval(to_write)),
    file = out_file,
    append = TRUE
  )

  result <- eval(substitute(expr, env = globalenv()))

  write(
    with(list(timepoint = "end"), eval(to_write)),
    file = out_file
  )

  result
}

run_and_log <- function(simu_name, ..., append = FALSE) {
  ##### Exception handling #####
  if (missing(simu_name)) {
    stop("A simulation name must be supplied.")
  }

  call_args <- as.list(match.call(expand.dots = FALSE))
  scripts <- call_args$...

  if (any(!sapply(scripts, is.character))) {
    stop(
      sprintf(
"%s not a character string.
The ... must be paths to .R files relative to the current working directory.",
        paste(scripts[!sapply(scripts, is.character)], collapse = ", ")
      )
    )
  }

  if (any(!sapply(scripts, file.exists))) {
    cat(dir(), sep = "\n")
    stop(
      sprintf(
        "%s not found. Above are the contents of your current working directory.",
        paste(scripts[!sapply(scripts, file.exists)], collapse = ", ")
      )
    )
  }

  if (any(!sapply(scripts, endsWith, ".R"))) {
    for (script in scripts[!sapply(scripts, endsWith, ".R")]) {
      if (!askYesNo(sprintf("%s should be a .R file, continue anyway?", script))) {
        return(NULL)
      }
    }
  }

  if (!file.exists(loggr::log_folder)) {
    dir.create(loggr::log_folder)
  }

  if (!file.exists(file.path(loggr::log_folder, simu_name))) {
    dir.create(file.path(loggr::log_folder, simu_name))
  } else if (!append) {
    overwrite <- askYesNo(
      sprintf(
"The directory %s already exists. Overwrite the current directory (Chosing 'No' will append to the existing files)?",
        simu_name
      )
    )

    if (is.na(overwrite)) {
      return(NULL)
    } else if (overwrite) {
      unlink(file.path(loggr::log_folder, simu_name), recursive = TRUE)
      dir.create(file.path(loggr::log_folder, simu_name))
    }
  }

  ##### Execute Scripts and log in log_folder #####
  sapply(
    scripts,
    loggr:::make_Rscript_call,
    simu_name
  )
}
