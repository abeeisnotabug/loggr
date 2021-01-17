#' @export
observe_speed <- function(interval = 60) {
  if (!is.numeric(interval)) {
    stop("interval must be a (real) number.")
  }

  topout <- loggr:::top()
  is_speed_observer <- stringr::str_detect(topout$procs_df$COMMAND, "loggr_simu_speed_observer.R")

  if (any(is_speed_observer)) {
    warning("The speed observer is already running with PID:")

    return(topout$procs_df$PID[is_speed_observer])
  }

  log_folder_path <- getOption("loggr.log.folder.path")
  speed_observer_script_path <- system.file("scripts", "loggr_simu_speed_observer.R", package = "loggr")
  speed_observer_folder_path <- file.path(log_folder_path, "loggr_speed_observer")

  if (!file.exists(speed_observer_folder_path)) {
    dir.create(speed_observer_folder_path)
  }

  out_and_err_file <- file.path(speed_observer_folder_path, paste0("loggr_speed_observer", c(".out", ".err")))

  message(sprintf("Started running loggr_simu_speed_observer.R observing every %i minutes with PID:", interval))

  system_call_script(
    speed_observer_script_path,
    out_and_err_file,
    list(
      log_folder_path = log_folder_path,
      call_time = loggr:::get_time(),
      interval = interval
    )
  )
}
