#' @export
log_progress <- function(..., loggr_object, expr) {
  call_args <- as.list(match.call(expand.dots = FALSE))
  iterators <- list(...)

  iterator_values <- ifelse(
    sapply(iterators, is.numeric) | sapply(iterators, is.logical),
    sprintf("%s", iterators),
    ifelse(
      sapply(iterators, is.character),
      sprintf("'%s'", iterators),
      sprintf("'%s'", sapply(iterators, capture.output))
    )
  )

  to_write <- quote(
    paste0(
      "#!iter;",
      timepoint,
      ";",
      format(Sys.time(), "%y.%m.%d-%H.%M.%OS6"),
      ";",
      loggr_object$iterator,
      ";",
      loggr_object$script_pid, "-", Sys.getpid(),
      ";",
      paste(
        sprintf(
          "%s=%s",
          if (is.null(names(call_args$...))) {
            call_args$...
          } else {
            append(
              as.list(names(call_args$...)[names(call_args$...) != ""]),
              call_args$...[names(call_args$...) == ""]
            )
          },
          iterator_values
        ),
        collapse = ","
      )
    )
  )

  file_name <- paste0(loggr_object$script_pid, "-", Sys.getpid(), ".log")

  out_file <- ifelse(
    isFALSE(loggr_object$simu_name),
    file_name,
    file.path(
      loggr::log_folder,
      loggr_object$simu_name,
      file_name
    )
  )

  write(
    with(list(timepoint = "start"), eval(to_write)),
    file = out_file,
    append = TRUE
  )

  result <- eval(substitute(expr, env = globalenv()))

  write(
    with(list(timepoint = "end"), eval(to_write)),
    file = out_file
  )

  result
}
