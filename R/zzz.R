#' load package dependencies
#'
#' @param libname libname
#' @param pkgname pkgname
#' @importFrom shiny addResourcePath
#' @return load package dependencies
#'

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "dashboardr-assets",
    system.file("assets", package = "dashboardr")
  )
}
