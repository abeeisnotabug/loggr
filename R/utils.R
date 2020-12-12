make_Rscript_call <- function(script, simu_name) {
  call_time <- format(Sys.time(), "%y.%m.%d-%H.%M.%OS6")
  out_and_err_file <- file.path(
    loggr::log_folder,
    simu_name,
    sprintf(
      "%s-%s.%s",
      call_time,
      basename(script),
      c("out", "err")
    )
  )

  system(
    sprintf(
      "nohup Rscript --vanilla %s --simu_name=%s > %s 2> %s & echo $!",
      script,
      simu_name,
      out_and_err_file[1],
      out_and_err_file[2]
    ),
    intern = TRUE
  )
}

cat_id <- function(script_pid) {
  cat(
    sprintf(
      "#!id;%s;%i",
      format(Sys.time(), "%y.%m.%d-%H.%M.%OS6"),
      script_pid
    ),
    sep = "\n"
  )
}

cat_cmd <- function(command_args) {
  cat(sprintf("#!cmd;%s", paste(list(command_args))),  sep = "\n")
}

cat_vars <- function(script_pid, iterator_variables) {
  cat(
    sprintf(
      "#!vars;%s;%i;%s",
      format(Sys.time(), "%y.%m.%d-%H.%M.%OS6"),
      script_pid,
      paste(
        sprintf(
          "%s=%s",
          names(iterator_variables),
          paste(iterator_variables)
        ),
        collapse = ","
      )
    ),
    sep = "\n"
  )
}
