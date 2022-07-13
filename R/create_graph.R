#' create a default plotly graph
#'
#' @param theme_var the list of bslib variables
#' @importFrom plotly plot_ly layout config
#'
#' @return a new default plotly graph

create_graph <- function(theme_var) {
  # graph creation
  graph <- plotly::plot_ly()
  # default graph layout
  graph <- plotly::layout(
    graph,
    # background
    plot_bgcolor = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    modebar = list(
      bgcolor = "transparent",
      color = theme_var$`body-color`,
      activecolor = theme_var$primary
    ),
    # xaxis
    xaxis = list(
      tickfont = list(color = theme_var$`body-color`),
      title = list(color = theme_var$`body-color`),
      zerolinecolor = theme_var$`body-color`,
      zerolinewidth = 2,
      gridcolor = theme_var$light
    ),
    # yaxis
    yaxis = list(
      tickfont = list(color = theme_var$`body-color`),
      title = list(color = theme_var$`body-color`),
      gridcolor = theme_var$light
    ),
    # personalized font
    font = list(family = theme_var$`font-family-base`)
  )
  # suppress plotly logo
  graph <- plotly::config(graph, displaylogo = FALSE)
  # return
  return(graph)
}
