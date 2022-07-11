


#' apply a default plotly theme for graph
#'
#' @param graph a graph to which apply the default theme
#' @param theme_var the list of bslib variables
#'
#' @return a graph with the default plotly theme applied

graph_default_theme <- function(graph, theme_var){
  # default graph layout
  graph <- plotly::layout(
    graph,
    # background
    plot_bgcolor  = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    modebar=list(
      bgcolor = 'transparent',
      color = theme_var$`body-color`,
      activecolor = theme_var$primary
    ),
    # xaxis
    xaxis = list(
      tickfont = list(color = theme_var$`body-color`),
      title=list(color = theme_var$`body-color`),
      zerolinecolor = theme_var$`body-color`,
      zerolinewidth = 2,
      gridcolor = theme_var$light
    ),
    # yaxis
    yaxis = list(
      tickfont = list(color = theme_var$`body-color`),
      title=list(color = theme_var$`body-color`),
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
