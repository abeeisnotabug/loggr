#' @export
shoggr <- function(launch.browser = getOption("shiny.launch.browser", interactive())) {
  appDir <- system.file("shoggr", package = "loggr")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `loggr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = launch.browser)
}
