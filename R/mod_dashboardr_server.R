#' dashboardr Server part
#'
#' @param id A module id
#' @param r  r internal list (advanced use)
#' @param df A dashboarder dataframe
#'
#' @importFrom plotly renderPlotly plot_ly
#' @import shiny
#' @return a dashboard
#' @export

mod_dashboardr_server <- function(id, df, r = NULL) {

  moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns
    # test df
    test_df(df = df,
            check_var = c("id", "type", "data")
    )
    # normalize df
    df <- norm_df(df = df, r = r)
    # extract bslib theme
    theme_var <- extract_bslib_theme(session)
    # dashboard server
    lapply(unique(df$row), function(i){
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
          # graph server creation
          graph <- mod_graph_server(
            id = sprintf("r_%s_%s_%s",i,j,k),
            df_graph = df_graph,
            r = r,
            theme_var = theme_var
          )
        })
      })
    })
  })
}
