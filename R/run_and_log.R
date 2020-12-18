#' @export
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
