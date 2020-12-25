.onLoad <- function(libname, pkgname) {
  op <- options()
  op.loggr <- list(
    loggr.log.folder.path = "/data/sim/_shared/simu_logs"
  )

  toset <- !(names(op.loggr) %in% names(op))
  if (any(toset)) options(op.loggr[toset])

  invisible()
}
