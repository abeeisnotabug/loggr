initialize_progress <- function(...) {
  command_args <- commandArgs()
  if ("--interactive" %in% command_args) {
    stop("This command must be run in an R script that is either sourced or run via 'Rscript' in the terminal.")
  }

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

run_and_log <- function(script) {
  if (!is.character(script)) {
    stop("'script' must be a character string with the path to a .R file relative to
         the current working directory.")
  }

  if (!file.exists(script)) {
    cat(dir(), sep = "\n")
    stop(sprintf("%s not found. Above are the contents of your current working directory.", script))
  }

  if (!grepl(".R", script)) {
    if(!askYesNo("'script' should be a .R file, continue anyway?")) {
      return(NULL)
    }
  }
}
