.onLoad <- function(libname, pkgname) {
  op <- options()
  op.loggr <- list(
    loggr.log.folder.path = "/data/sim/_shared/simu_logs",
    loggr.log.prefix = "#!"
  )

  toset <- !(names(op.loggr) %in% names(op))
  if (any(toset)) options(op.loggr[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the loggr package! Start the shoggr app by calling the shoggr() function.")
}
