#' dashboardr Server part
#'
#' @param id A module id
#' @param df A dashboarder dataframe
#'
#' @return a dashboard
#' @export

mod_dashboardr_server <- function(id, df) {

  moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns
    # test df
    df <- test_df(df = df)
    # normalize df
    df <- norm_df(df = df)
    # render plot
    output$test <- renderPlotly({
      plot_ly(economics, x = ~pop)
  })
})
}
