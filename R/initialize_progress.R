#' @export
initialize_progress <- function(...) {
  command_args <- commandArgs()
  iterator_variables <- list(...)

  if ("--interactive" %in% command_args) {
    stop("This command must be run in an R script that is either sourced or run via 'Rscript' in the terminal.")
  }

  if (any("" %in% names(iterator_variables))) {
    stop("All arguments must be named with format iterator_name = iterator_values.")
  }

  parent_id <- Sys.getpid()

  if ("-e" %in% command_args) {
    rscript <- FALSE
    call_time <- format(Sys.time(), "%y.%m.%d-%H.%M.%OS6")
  } else {
    rscript_file_name <- basename(substring(command_args[grepl("--file", command_args)], 8))
    simu_log_folder_path <- substring(command_args[grepl("--simu_log_folder_path", command_args)], 24)
    call_time <- substring(command_args[grepl("--call_time", command_args)], 13)

    rscript <- TRUE
  }

  log_folder_path <- ifelse(rscript, simu_log_folder_path, ".")

  cluster_log_file <- paste0(
    "c-",
    call_time,
    "-", parent_id,
    ifelse(rscript, paste0("-", rscript_file_name), ""),
    ".out"
  )

  cat(
    paste0(
      make_cat_prefix("script"), ";",
      paste(list(command_args)),";",
      paste_vars(iterator_variables, parent_id, worker_id = NULL, log_time = NULL)
    ),
    sep = "\n"
  )

  list(
    parent_id = parent_id,
    outfile = file.path(log_folder_path, cluster_log_file),
    call_time = call_time,
    log_folder_path = ifelse(rscript, log_folder_path, FALSE),
    rscript_file_name = rscript_file_name,
    iterator = iterators::icount()
  )
}
