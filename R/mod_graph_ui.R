#' Create a ui shiny tag for plotly graph
#'
#' @param id plot id
#' @param df_graph A dashboarder graph dataframe
#' @param r r internal list (advanced use)
#' @importFrom plotly plotlyOutput
#' @import shiny
#' @return a ui shiny tag for plotly graph

mod_graph_ui <- function(id, df_graph, r = NULL) {
  # ns
  ns <- NS(id)

  if (df_graph$type[1] == "plot") {
    # graph
    tag <- plotlyOutput(
      outputId = ns("plot"),
      width = "100%",
      height = "400px",
      inline = FALSE,
      reportTheme = TRUE
    )
  } else if (df_graph$type[1] == "table") {
    # table
    tag <- DT::dataTableOutput(ns("table"))
  }
  # vertical center of the element in the section
  tag <- div(class = "d-flex align-items-center h-100 w-100", tag)
  # return
  return(tag)
}
