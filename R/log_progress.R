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

  variables <- paste0(
    "iteration_counter=", loggr_object$iterator$nextElem(), ";",
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

  worker_id <- Sys.getpid()

  to_write <- quote(
    paste0(
      loggr:::make_cat_prefix(timepoint, loggr_object$parent_id, worker_id, loggr_object$call_time), ";",
      variables
    )
  )

  log_file_names <- make_logg_file_names(loggr_object)

  write(
    with(list(timepoint = "start"), eval(to_write)),
    file = log_file_names$out,
    append = TRUE
  )

  result <- try(withCallingHandlers(
    eval(substitute(expr, env = globalenv())),
    error = function(e) {
      loggr:::logg_condition(e, loggr_object$parent_id, worker_id, variables, log_file_names$err)
    },
    warning = function(w) {
      loggr:::logg_condition(w, loggr_object$parent_id, worker_id, variables, log_file_names$err)
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      loggr:::logg_condition(m, loggr_object$parent_id, worker_id, variables, log_file_names$err)
    }
  ), silent = TRUE)

  write(
    with(list(timepoint = "end"), eval(to_write)),
    file = log_file_names$out
  )

  if (class(result) == "try-error") {
    stop(attr(result, "condition"))
  } else {
    result
  }
}
