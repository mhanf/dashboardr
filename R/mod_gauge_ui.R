
#' gauge ui part
#'
#' @param id module id
#' @param df A graph dashboarder dataframe
#' @param r r internal list (advanced use)
#' @param pattern default pattern to identify r code in dashboardr dataframe
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
#' @return a gauge ui

mod_gauge_ui <- function(id,
                         df,
                         r = NULL,
                         pattern = "^%r%") {
  # ns
  ns <- shiny::NS(id)
  # parameter value
  val <- def_gauge_val_ui(
    df = df,
    r = r,
    pattern = pattern
  )

  plotly::plotlyOutput(
    outputId = ns("gauge"),
    width = "100%",
    height = val$el_height,
    inline = FALSE,
    reportTheme = TRUE
  )
}
