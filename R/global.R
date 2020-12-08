initialize_progress <- function(...) {
  cl_pid <- Sys.getpid()
  cat(sprintf("#!id:%i", cl_pid), sep = "\n")

  iterator_variables <- list(...)

  cat(
    sprintf(
      "#!vars;%s",
      paste(
        sprintf(
          "%s=%s",
          names(iterator_variables),
          paste(iterator_variables)),
        collapse = ","
      )
    ),
    sep = "\n"
  )
}

log_progress <- function(..., cluster_pid = "cl_pid") {
  cl_pid <- ifelse(is.character(cluster_pid), get(cluster_pid), cluster_pid)

  call_args <- as.list(match.call(expand.dots = FALSE))

  cat(
    sprintf(
      "#!iter;%i-%i,%s",
      cl_pid, Sys.getpid(),
      paste(sprintf("%s=%s", names(call_args$...), call_args$...), collapse = ",")),
    sep = "\n"
  )
}
