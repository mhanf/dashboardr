#' create a default indicator plotly graph
#'
#' @param theme_var the list of bslib variables
#' @importFrom plotly plot_ly layout config
#'
#' @return a new default indicator plotly graph

gauge_create <- function(theme_var) {
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
    # margin
    margin = list(l = 25, r = 35, t = 25, b = 5),
    # personalized font
    font = list(family = theme_var$`font-family-base`)
  )
  # suppress plotly logo
  graph <- plotly::config(graph,
    displaylogo = FALSE,
    locale = "fr"
  )
  # return
  return(graph)
}
