#' dashboardr Server part
#'
#' @param id A module id
#' @param r  r internal list (advanced use)
#' @param df_graph A graph dashboarder dataframe
#' @param theme_var Shiny app theme list of interesting variables
#' @importFrom plotly renderPlotly plot_ly layout
#' @import shiny
#' @return a dashboard

mod_graph_server <- function(id, df_graph, r = NULL, theme_var) {
  moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns
    # graph compilation
    if (df_graph$type[1] == "plot") {
      output$plot <- renderPlotly({
        # data evaluation
        data <- eval(parse(text = df_graph$data[1]))
        # graph construction
        graph <- plot_ly(
          data = data,
          type = "scatter",
          x = data[, df_graph$x],
          y = data[, df_graph$y]
        )
        # graph default theme
        graph <- graph_default_theme(graph = graph, theme_var = theme_var)
        # graph return
        graph
      })
    } else if (df_graph$type[1] == "table") {
      output$table <- DT::renderDataTable(
        {
          data <- eval(parse(text = df_graph$data[1]))
        },
        selection = "single",
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          buttons = c("excel"),
          language = list(
            paginate = list(previous = "Pr\u00e9c\u00e9dent", `next` = "Suivant"),
            search = "Recherche : "
          ),
          dom = "Btp",
          columnDefs = list(
            list(
              className = "dt-center",
              targets = "_all"
            )
          ),
          pageLength = 5
        )
      )
    }
  })
}
