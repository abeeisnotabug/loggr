get_time <- function() {
  format(Sys.time(), "%y.%m.%d.%H.%M.%OS6")
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

  system(
    paste0(
      "nohup Rscript --vanilla ",  script, " ",
      "--simu_log_folder_path=", simu_log_folder_path, " ",
      "--call_time=", call_time, " ",
      "--count_explicitly=", count_explicitly, " ",
      "> ", out_and_err_file[1], " ",
      "2> ", out_and_err_file[2], " ",
      "& echo $!"
    ),
    intern = TRUE
  )
}

make_logg_file_names <- function(loggr_object) {
  lapply(
    c(out = "out", err = "err"),
    function(file_ext) {
      file_name <- paste0("w-", loggr_object$call_time, "-", loggr_object$parent_id, "-", Sys.getpid(), "-", loggr_object$rscript_file_name, ".", file_ext)

      ifelse(
        isFALSE(loggr_object$log_folder_path),
        file_name,
        file.path(
          loggr_object$log_folder_path,
          file_name
        )
      )
    }
  )
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
    sapply(iterator_values, is.character),
    ifelse(
      sapply(iterator_values, length) > 1,
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
