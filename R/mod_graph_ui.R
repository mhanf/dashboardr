#' Create a ui shiny tag for plotly graph
#'
#' @param id plot id
#' @param df_graph A dashboarder graph dataframe
#' @param r r internal list (advanced use)
#' @importFrom plotly plotlyOutput
#' @importFrom DT dataTableOutput
#' @importFrom htmltools htmlDependency
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
      # height = "400px",
      inline = FALSE,
      reportTheme = TRUE
    )
  } else if (df_graph$type[1] == "table") {
    # table
    tag <- DT::dataTableOutput(ns("table"))
    # table css dependency
    table_dep <- htmltools::htmlDependency(
      name = "table",
      version = "0.0.1",
      package = "dashboardr",
      src = "assets",
      stylesheet = c(file = "table.css")
    )
    # add dependency
    tag <- tagList(table_dep, tag)
  } else if (df_graph$type[1] == "module") {
    # module
    if (is.na(df_graph$mod_ui[1])) {
      stop("mod_ui must be specified")
    } else {
      tag <- div(
        class = "w-100",
        eval(parse(text = as.character(df_graph$mod_ui[1])))
      )
    }
  }
  # return
  return(tag)
}
