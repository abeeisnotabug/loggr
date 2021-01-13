#' @export
run_and_log <- function(simu_name, ..., append = FALSE, count_explicitly = FALSE) {
  ##### Exception handling #####
  if (missing(simu_name)) {
    stop("A simulation name must be supplied.")
  }

  call_args <- as.list(match.call(expand.dots = FALSE))
  scripts <- call_args$...

  if (any(!sapply(scripts, base::is.character))) {
    stop(
      sprintf(
"%s not a character string.
The ... must be paths to .R files relative to the current working directory.",
        paste(scripts[!sapply(scripts, base::is.character)], collapse = ", ")
      )
    )
  }

  if (any(!sapply(scripts, base::file.exists))) {
    cat(dir(), sep = "\n")
    stop(
      sprintf(
        "%s not found. Above are the contents of your current working directory.",
        paste(scripts[!sapply(scripts, base::file.exists)], collapse = ", ")
      )
    )
  }

  if (any(!sapply(scripts, base::endsWith, ".R"))) {
    for (script in scripts[!sapply(scripts, base::endsWith, ".R")]) {
      if (!askYesNo(sprintf("%s should be a .R file, continue anyway?", script))) {
        return(NULL)
      }
    }
  }

  if (is.null(getOption("loggr.log.folder.path"))) {
    stop("Option loggr.log.folder.path not set (did you load the package?)")
  } else {
    log_folder <- getOption("loggr.log.folder.path")
  }

  if (!file.exists(log_folder)) {
    dir.create(log_folder)
  }

  simu_log_folder_path <- file.path(log_folder, simu_name)

  if (!file.exists(simu_log_folder_path)) {
    dir.create(simu_log_folder_path)
  } else if (!append) {
    overwrite <- askYesNo(
      sprintf(
"The directory %s already exists.
Overwrite the current directory?
(Chosing 'No' will append to the existing files)",
        simu_name
      )
    )

    if (is.na(overwrite)) {
      return(NULL)
    } else if (overwrite) {
      unlink(simu_log_folder_path, recursive = TRUE)
      dir.create(simu_log_folder_path)
    }
  }

  ##### Execute Scripts and log in log_folder #####
  pids <- sapply(
    scripts,
    loggr:::make_Rscript_call,
    simu_log_folder_path,
    count_explicitly
  )

  names(pids) <- scripts

  pids
}
