#' dashboardr ui part
#'
#' @param id module id
#' @param df A dashboarder dataframe
#' @importFrom plotly plotlyOutput
#' @import shiny
#' @return a dashboard
#' @export

mod_dashboardr_ui <- function(id, df) {
  ns <- NS(id)
  # test df
  df <- test_df(df = df)
  # normalize df
  df <- norm_df(df = df)
  # graph definition
  graph <- plotlyOutput(
    outputId = ns("test"),
    width = "100%",
    height = "400px",
    inline = FALSE,
    reportTheme = TRUE
  )

  graph <- card_ui(
    title = "Titre",
    body = graph
    )

  graph
}



