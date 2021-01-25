command_args_list <- loggr:::command_args_as_list(commandArgs(trailingOnly = TRUE))

while (TRUE) {
  simus <- dir(command_args_list$log_folder_path)

  Sys.sleep(60 * command_args_list$interval)
}
