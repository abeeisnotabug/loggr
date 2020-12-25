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

  iterator_variables <- c(loggr_object$iterator$nextElem(), iterator_values)
  names(iterator_variables) <- c("iterationCounter", make_iterator_variable_names(call_args$...))

  worker_id <- Sys.getpid()

  to_write <- quote(
    paste0(
      make_cat_prefix(timepoint),
      paste_vars(iterator_variables, loggr_object$parent_id, worker_id)
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
      logg_condition(e, loggr_object$parent_id, worker_id, iterator_variables, log_file_names$err)
    },
    warning = function(w) {
      logg_condition(w, loggr_object$parent_id, worker_id, iterator_variables, log_file_names$err)
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      logg_condition(m, loggr_object$parent_id, worker_id, iterator_variables, log_file_names$err)
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
