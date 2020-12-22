#' @export
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
    file_name <- basename(substring(command_args[grepl("--file", command_args)], 8))
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
    ),
    iterator = iterators::icount()
  )
}
