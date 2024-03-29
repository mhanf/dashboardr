#' shiny tag encapsulation into a nav
#'
#' @param graph a graph to encapsulate
#' @param df_sect A section dashboardr dataframe
#' @param df_graph A graph dashboardr dataframe
#' @param r r internal list (advanced use)
#' @param default_pattern default pattern to evaluate
#'
#' @importFrom htmltools tagQuery
#' @importFrom bslib nav
#' @return shiny tag encapsulated into a nav

nav_ui <- function(graph, df_sect, df_graph, r, default_pattern) {
  if (length(unique(df_sect$id)) > 1) {
    # nav parameters
    nav_val <- def_nav_val(
      df_graph = df_graph,
      r = r,
      default_pattern = default_pattern
    )
    # nav encapsulation
    graph <- htmltools::tagQuery(
      bslib::nav(
        title = nav_val$nav_title,
        icon = icon(
          name = nav_val$nav_icon_name,
          lib = nav_val$nav_icon_lib
        ),
        graph,
        div(nav_val$nav_title,class = "text-danger")
      )
    )$
      addClass("pt-1 align-items-center w-100 h-100")$
      allTags()
  }
  # return
  return(graph)
}
