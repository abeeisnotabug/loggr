#' @export
log_progress <- function(..., loggr_object, expr) {
  call_args <- as.list(match.call(expand.dots = FALSE))
  raw_list <- call_args$...
  raw_values <- list(...)

  worker_id <- Sys.getpid()
  it_count <- loggr_object$iterator$currentElem()

  start_time <- get_time()

  log_file_names <- make_logg_file_names(loggr_object)

  count <- !loggr_object$count_explicitly

  logg_this_condition <- function(c) {
    logg_condition(c = c, get_time(), loggr_object$parent_id, worker_id, it_count, raw_list, raw_values, log_file_names$err)
  }

  logg_iteration("start", start_time, loggr_object$parent_id, worker_id, it_count, raw_list, raw_values, log_file_names$out)

  result <- try(withCallingHandlers(
    eval(substitute(expr, env = globalenv())),
    error = function(e) {
      logg_this_condition(e)
    },
    warning = function(w) {
      logg_this_condition(w)
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      if (!grepl(prefix("count"), conditionMessage(m))) {
        logg_this_condition(m)
      } else {
        count <<- conditionMessage(m) == prefix("count")
      }
    }
  ), silent = TRUE)

  if (count) {
    it_count <- loggr_object$iterator$nextElem()
  }

  logg_iteration("end", start_time, loggr_object$parent_id, worker_id, it_count, raw_list, raw_values, log_file_names$out)

  if (class(result) == "try-error") {
    stop(attr(result, "condition"))
  } else {
    result
  }
}

#'@export
dontcount <- function() {
  message(prefix("countNOT\n"), appendLF = FALSE)
}
