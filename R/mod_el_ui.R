#' Create a ui shiny tag for plotly graph
#'
#' @param id plot id
#' @param df_graph A dashboarder graph dataframe
#' @param default_pattern default_pattern = default_pattern
#' @param r r internal list (advanced use)
#'
#' @importFrom plotly plotlyOutput
#' @importFrom DT dataTableOutput
#' @importFrom htmltools htmlDependency
#' @importFrom shiny NS tagList
#' @return a ui shiny tag for plotly graph

mod_el_ui <- function(id, df_graph, r = NULL, default_pattern = "^%r%") {
  # ns
  ns <- shiny::NS(id)

  # element parameters
  el_val <- def_el_val(
    df_graph = df_graph,
    r = r,
    default_pattern = default_pattern
  )

  if (df_graph$type[1] == "plot") {
    # graph
    tag <- plotly::plotlyOutput(
      outputId = ns("plot"),
      width = "100%",
      height = el_val$el_height,
      inline = FALSE,
      reportTheme = TRUE
    )
  } else if (df_graph$type[1] == "table") {
    # table
    tag <- DT::dataTableOutput(ns("table"),
      width = "100%",
      height = el_val$el_height
    )
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
  # infobox experimental
  else if (df_graph$type[1] == "infobox") {
    # module
    tag <- mod_infobox_ui(
      id = ns("infobox"),
      df_graph = df_graph,
      r = r,
      default_pattern = default_pattern
    )
  }
  # indicator
  else if (df_graph$type[1] == "indicator") {
    tag <- plotly::plotlyOutput(
      outputId = ns("indicator"),
      width = "100%",
      height = el_val$el_height,
      inline = FALSE,
      reportTheme = TRUE
    )
  }
  # return
  return(tag)
}
