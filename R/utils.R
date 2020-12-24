make_Rscript_call <- function(script, simu_log_folder_path) {
  call_time <- format(Sys.time(), "%y.%m.%d-%H.%M.%OS6")

  out_and_err_file <- file.path(
    simu_log_folder_path,
    paste0(
      "s-",
      call_time, "-",
      basename(script), ".",
      c("out", "err")
    )
  )

  system(
    paste0(
      "nohup Rscript --vanilla ",  script, " ",
      "--simu_log_folder_path=", simu_log_folder_path, " ",
      "--call_time=", call_time, " ",
      "> ", out_and_err_file[1], " ",
      "2> ", out_and_err_file[2], " ",
      "& echo $!"
    ),
    intern = TRUE
  )
}

paste_vars <- function(iterator_variables) {
  paste0(
    "#!vars;",
    paste(
      sprintf(
        "%s=%s",
        names(iterator_variables),
        paste(iterator_variables)
      ),
      collapse = ","
    )
  )
}

logg_condition <- function(c, parent_id, worker_id, variables, log_file_name) {
  write(
    paste0(
      loggr:::make_cat_prefix(class(c)[2], parent_id, worker_id), ";",
      variables, "\n",
      "Call: ", paste(capture.output(c$call), collapse = "\n"), "\n",
      "Msg: ", c$message, "\n"
    ),
    file = log_file_name,
    append = TRUE
  )
}

make_logg_file_names <- function(loggr_object) {
  lapply(
    c(out = "out", err = "err"),
    function(file_ext) {
      file_name <- paste0("w-", loggr_object$call_time, "-", loggr_object$parent_id, "-", Sys.getpid(), "-", loggr_object$rscript_file_name, ".", file_ext)

      file.path(
        ifelse(isFALSE(loggr_object$log_folder_path), "", loggr_object$log_folder_path),
        file_name
      )
    }
  )
}

make_cat_prefix <- function(name, parent_id, worker_id = NULL, call_time = format(Sys.time(), "%y.%m.%d-%H.%M.%OS6")) {
  paste0(
    "#!", name, ";", call_time, ";",
    "parent_id=", parent_id, ifelse(is.null(worker_id), "", paste0(";", "worker_id=", worker_id))
  )
}