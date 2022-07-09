#' dashboardr ui part
#'
#' @param id module id
#' @param r r internal list (advanced use)
#' @param df A dashboarder dataframe
#'
#' @importFrom plotly plotlyOutput
#' @importFrom bslib navs_tab nav
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
  dashboard <- lapply(unique(df$row), function(i){
    # df row selection
    df_row <- df[df$row == i,]
    # row compilation
    row <- lapply(unique(df_row$card), function(j){
      # df card selection
      df_card <- df_row[df_row$card == j,]
      # lapply df_card
      card <- lapply(unique(df_card$id), function(k){
        # df graph selection
        df_graph <- df_card[df_card$id == k,]
        # graph ui creation
        graph <- mod_graph_ui(
          id = ns(sprintf("r_%s_%s_%s",i,j,k)),
          df_graph = df_graph,
          r = r
        )
        # nav encapsulation
        if (length(unique(df_card$id)) > 1){ graph <-nav(title = k, graph) }
        return(graph)
      })
      # navs_tab encapsulation
      if (length(unique(df_card$id)) > 1){ card <- navs_tab(!!!card) }
      # card encapsulation
      card <- card_ui(
        title = paste("row_",i,"_card_",j),
        body = card,
        df_card = df_card,
        r = r
      )
    })
    # row encapsulation
    row <- div(class = "row m-0 p-0", row)
  })


}



