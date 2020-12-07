initialize_progress <- function(...) {
  cl_pid <- Sys.getpid()
  cat(sprintf("#!id:%i", cl_pid), sep = "\n")
  
  iterator_variables <- list(...)
  
  cat(
    sprintf(
      "#!vars;%s",
      paste(sprintf("%s=%s", names(iterator_variables), paste(iterator_variables)), collapse = ",")
    ),
    sep = "\n"
  )
}

log_progress <- function(..., cluster_pid = "cl_pid") {
  cl_pid <- ifelse(is.character(cluster_pid), get(cluster_pid), cluster_pid)
  
  iterator_names <- as.character(sys.call()[-1])
  iterator_values <- c(...)
  
  cat(
    sprintf(
      "#!iter;%i-%i,%s",
      cl_pid, Sys.getpid(),
      paste(sprintf("%s=%s", iterator_names, iterator_values), collapse = ",")),
    sep = "\n"
  )
}
