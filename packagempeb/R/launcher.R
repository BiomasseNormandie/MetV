#' Lance l'interface de l'outil MPEB
#'
#' @return l'interface shiny lancée dans un navigateur web
#' @export
#' @import shiny
launch_mpeb <- function(){
  appDir <- system.file("shiny", package = "packagempeb")
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
