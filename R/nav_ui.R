#' shiny tag encapsulation into a nav
#'
#' @param graph a graph to encapsulate
#' @param df_sect A section dashboarder dataframe
#' @param df_graph A graph dashboarder dataframe
#' @param r r internal list (advanced use)
#'
#' @importFrom htmltools tagQuery
#' @return shiny tag encapsulated into a nav

nav_ui <- function(graph, df_sect, df_graph, r) {
  if (length(unique(df_sect$id)) > 1) {
    graph <- htmltools::tagQuery(
      nav(
        title = df_graph$id[1],
        icon = shiny::icon("user"),
        graph
      )
    )$
      addClass("pt-1 align-items-center w-100 h-100")$
      allTags()
  }
  # return
  return(graph)
}
