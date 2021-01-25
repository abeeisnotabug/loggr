#' @export
initialize_progress <- function(...) {
  command_args <- commandArgs()
  call_args <- as.list(match.call(expand.dots = FALSE))
  raw_list <- call_args$...
  raw_values <- list(...)

  if ("--interactive" %in% command_args) {
    stop("This command must be run in an R script that is either sourced or run via 'Rscript' in the terminal.")
  }

  if (!all(sapply(raw_values, function(val) is.numeric(val) | is.complex(val) | is.character(val) | is.logical(val)))) {
    stop("Sorry, iterators must be either: numeric, complex, character, or logical.")
  }

  parent_id <- Sys.getpid()

  if ("-e" %in% command_args) {
    rscript_file_name <- NA
    log_folder_path <- "."
    call_time <- format(Sys.time(), "%y.%m.%d-%H.%M.%OS6")
    count_explicitly <- FALSE
  } else {
    command_args_list <- command_args_as_list(command_args)

    rscript_file_name <- basename(command_args_list$file)
    log_folder_path <- command_args_list$simu_log_folder_path
    call_time <- command_args_list$call_time
    count_explicitly <- command_args_list$count_explicitly
  }

  cluster_log_file <- paste0(
    "c-",
    call_time,
    "-", parent_id,
    "-NA-",
    rscript_file_name,
    ".out"
  )

  cat_initialize_progress(command_args, call_time, parent_id, raw_list, raw_values)

  list(
    parent_id = parent_id,
    outfile = file.path(log_folder_path, cluster_log_file),
    call_time = call_time,
    log_folder_path = log_folder_path,
    rscript_file_name = rscript_file_name,
    iterator = iterator_with_state(),
    count_explicitly = count_explicitly
  )
}
