#' dashboardr ui part
#'
#' @param id module id
#' @param r r internal list (advanced use)
#' @param df A dashboarder dataframe
#'
#' @importFrom plotly plotlyOutput
#' @import shiny
#' @return a dashboard
#' @export

mod_dashboardr_ui <- function(id, df, r = NULL) {
  # ns
  ns <- NS(id)
  # test df
  df <- test_df(df = df, r = r)
  # normalize df
  df <- norm_df(df = df, r = r)
  # dashboard ui
  dashboard <- lapply(
    unique(df$row),
    function(i){
      # df row selection
      df_row <- df[df$row == i,]
      # row compilation
      row <- lapply(
        unique(df_row$id),
        function(j){
          # df graph selection
          df_graph <- df_row[df_row$id == j,]
          # graph ui creation
          graph <- mod_graph_ui(
            id = ns(sprintf("r_%s_%s",i,j)),
            df_graph = df_graph,
            r = r
          )
          # card creation
          graph <- card_ui(
            title = j,
            body = graph,
            df_graph = df_graph,
            r = r
          )
        })
      # row formatting
      row <- div(class = "row m-0 p-0", row)
    })


}



