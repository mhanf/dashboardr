#' dashboardr ui part
#'
#' @param id module id
#' @param r r internal list (advanced use)
#' @param df A dashboarder dataframe
#' @param default_pattern default pattern to identify r code in dashboardr dataframe
#'
#' @importFrom plotly plotlyOutput
#' @importFrom bslib navs_tab nav
#' @importFrom htmltools tagQuery
#' @import shiny
#' @return a dashboard
#' @export

mod_dashboardr_ui <- function(id, df, r = NULL, default_pattern = "^%r%") {
  # ns
  ns <- NS(id)
  # test df
  test_df(
    df = df,
    check_var = c("id", "type", "data")
  )
  # normalize df
  df <- norm_df(df = df, r = r)
  # dashboard ui
  dashboard <- lapply(unique(df$row), function(i) {
    # df row selection
    df_row <- df[df$row == i, ]
    # row compilation
    row <- lapply(unique(df_row$section), function(j) {
      # df sect selection
      df_sect <- df_row[df_row$section == j, ]
      # df associated sect value
      sect_val <- def_sect_val(
        df_sect = df_sect,
        r = r,
        default_pattern = default_pattern
      )
      # lapply df_sect
      section <- lapply(unique(df_sect$id), function(k) {
        # df graph selection
        df_graph <- df_sect[df_sect$id == k, ]
        # graph ui creation
        graph <- mod_graph_ui(
          id = ns(sprintf("r_%s_%s_%s", i, j, k)),
          df_graph = df_graph,
          r = r
        )
        # nav encapsulation
        graph <- nav_ui(
          graph = graph,
          df_sect = df_sect,
          df_graph = df_graph,
          r = r
        )
        # return
        return(graph)
      })
      # navs_tab encapsulation
      if (length(unique(df_sect$id)) > 1) {
        section <- navs_tab(!!!section)
      }
      # card encapsulation
      section <- card_ui(
        body = section,
        sect_val = sect_val,
        r = r
      )
      # col encapsulation
      section <- col_ui(
        section = section,
        sect_val = sect_val,
        r = r
      )
      # return
      return(section)
    })
    # row encapsulation
    row <- div(class = "row m-0 p-0", row)
    # return
    return(row)
  })
  # table css dependency
  table_dep <- htmltools::htmlDependency(
    name = "table",
    version = "0.0.1",
    package = "dashboardr",
    src = "assets",
    stylesheet = c(file = "table.css")
  )
  # add dependency
  dashboard <- tagList(table_dep, dashboard)
  # return
  return(dashboard)
}
