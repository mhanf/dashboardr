#' dashboardr Server part
#'
#' @param id A module id
#' @param r  r internal list (advanced use)
#' @param df_graph A graph dashboarder dataframe
#'
#' @importFrom plotly renderPlotly plot_ly
#' @import shiny
#' @return a dashboard
#' @export

mod_graph_server <- function(id, df_graph, r = NULL) {

  moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns
    # data extraction
    data <- eval(parse(text = df_graph$data[1]))
    # graph compilation
    output$graph <- renderPlotly({
      plot_ly(
        data = data,
        type = "scatter",
        x = data[,df_graph$x],
        y = data[,df_graph$y]
      )
    })
  })}
