#' Run Shiny App
#'
#' Launches the interactive Shiny application for the robustMDAS package.
#'
#' @export
run_shiny_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Please install it: install.packages('shiny')")
  }
  
  app_dir <- system.file("shiny", package = "robustMDAS")
  if (app_dir == "") {
    stop("Shiny app not found. Please reinstall the package.")
  }
  
  shiny::runApp(app_dir, launch.browser = TRUE)
}
