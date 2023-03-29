#' runExample
#'
#' App launcher
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "gcamreport_app", package = "gcamreport")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `gcamreport_app`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
