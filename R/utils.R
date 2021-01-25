get_time <- function(mus = 6) {
  format(Sys.time(), paste0("%y.%m.%d.%H.%M.%OS", mus))
}

make_Rscript_call <- function(script, simu_log_folder_path, count_explicitly) {
  call_time <- get_time()

  out_and_err_file <- file.path(
    simu_log_folder_path,
    paste0(
      "s-",
      call_time, "-",
      "NA-NA-",
      basename(script), ".",
      c("out", "err")
    )
  )

  system_call_script(
    script,
    out_and_err_file,
    list(
      simu_log_folder_path = simu_log_folder_path,
      call_time = call_time,
      count_explicitly = count_explicitly
    )
  )
}

system_call_script <- function(script, out_and_err_file, args) {
  parsed_args <- paste0("--", names(args), "=", args, collapse = " ")

  call_command <- paste(
    "nohup", "Rscript", "--vanilla",  script,
    parsed_args,
    ">", out_and_err_file[1],
    "2>", out_and_err_file[2],
    "&", "echo", "$!"
  )

  expected_pid <- system(call_command, intern = TRUE)

  if (is.na(strtoi(expected_pid))) {
    stop(sprintf("Call to run script %s did not return PID: %s", script, expected_pid))
  } else {
    strtoi(expected_pid)
  }
}

command_args_as_list <- function(command_args) {
  split_command_args <- strsplit(command_args, "=")
  names(split_command_args) <- lapply(lapply(split_command_args, `[`, 1), substring, 3)
  lapply(split_command_args, `[`, 2)
}

make_logg_file_names <- function(loggr_object) {
  files <- file.path(
    loggr_object$log_folder_path,
    paste0(
      "w-", loggr_object$call_time, "-",
      loggr_object$parent_id, "-",
      Sys.getpid(), "-",
      loggr_object$rscript_file_name, ".",
      c("out", "err")
    )
  )

  `names<-`(files, c("out", "err"))
}

prefix <- function(to_paste = NULL) paste0(getOption("loggr.log.prefix"), to_paste)

prepare_iterator_variable_names <- function(iterator_list) {
  if (is.null(names(iterator_list))) {
    iterator_list
  } else {
    ifelse(
      names(iterator_list) == "",
      as.character(iterator_list),
      names(iterator_list)
    )
  }
}

prepare_iterator_values <- function(iterator_values) {
  ifelse(
    sapply(iterator_values, base::is.character),
    ifelse(
      sapply(iterator_values, base::length) > 1,
      sprintf("%s", iterator_values),
      sprintf(
        "\"%s\"",
        sapply(
          iterator_values,
          function(value) {
            if (is.character(value)) gsub("\"", "'", value) else value
          }
        )
      )
    ),
    sprintf("%s", iterator_values)
  )
}

paste_vars <- function(raw_list, raw_values) {
  iterators <- prepare_iterator_values(raw_values)
  names(iterators) <- prepare_iterator_variable_names(raw_list)

  paste0(
    sprintf(
      "%s=%s",
      names(iterators),
      paste(iterators)
    ),
    collapse = ","
  )
}

logg_condition <- function(c, start_time, parent_id, worker_id, it_count, raw_list, raw_values, log_file_name) {
  write(
    paste0(
      make_logg_line(class(c)[2], start_time, parent_id, worker_id, it_count, raw_list, raw_values), "\n",
      "Call: ", paste(capture.output(conditionCall(c)), collapse = "\n"), "\n",
      "Msg: ", conditionMessage(c), "\n"
    ),
    file = log_file_name,
    append = TRUE
  )
}

cat_initialize_progress <- function(command_args, call_time, parent_id, raw_list, raw_values) {
  cat(
    prefix(
      paste(
        paste(list(command_args)),        # call
        call_time,                        # callTime
        parent_id,                        # parentPID
        paste_vars(raw_list, raw_values), # vars
        sep = ";"
      )
    ),
    sep = "\n"
  )
}

logg_iteration <- function(status, start_time, parent_id, worker_id, it_count, raw_list, raw_values, file) {
  write(
    make_logg_line(status, start_time, parent_id, worker_id, it_count, raw_list, raw_values),
    file = file,
    append = (status == "start")
  )
}

make_logg_line <- function(status, start_time, parent_id, worker_id, it_count, raw_list, raw_values) {
  prefix(
    paste(
      status, start_time,
      parent_id, worker_id,
      it_count,
      paste_vars(raw_list, raw_values),
      sep = ";"
    )
  )
}
