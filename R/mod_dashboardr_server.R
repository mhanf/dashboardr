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
    df <- test_df(df = df, r = r)
    # normalize df
    df <- norm_df(df = df, r = r)
    # dashboard compilation
    lapply(
      unique(df$row),
      function(i){
        # df row selection
        df_row <- df[df$row == i,]
        # row compilation
        lapply(
          unique(df_row$id),
          function(j){
            # df graph selection
            df_graph <- df_row[df_row$id == j,]
            # graph ui creation
            mod_graph_server(
              id = sprintf("r_%s_%s",i,j),
              df_graph = df_graph,
              r = r
            )
          })
      })
  })
}
