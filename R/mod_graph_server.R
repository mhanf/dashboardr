#' dashboardr Server part
#'
#' @param id A module id
#' @param r  r internal list (advanced use)
#' @param df_graph A graph dashboarder dataframe
#'
#' @importFrom plotly renderPlotly plot_ly layout
#' @importFrom bslib bs_current_theme
#' @import shiny
#' @return a dashboard
#' @export

mod_graph_server <- function(id, df_graph, r = NULL) {

  moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns

    # graph compilation
    if (df_graph$type[1] == "plot"){
    output$plot <- renderPlotly({
      # theme identification
      theme = bslib::bs_current_theme(session = session)
      # data evaluation
      data <- eval(parse(text = df_graph$data[1]))
      #graph construction
      graph <- plot_ly(
        data = data,
        type = "scatter",
        x = data[,df_graph$x],
        y = data[,df_graph$y]
      )
      # graph layout
      graph <- plotly::layout(
        graph,
        font = list(family = bslib::bs_get_variables(theme,"font-family-base"))
        )
      # graph return
      graph
      })}
      else if (df_graph$type[1] == "table"){
        output$table <- DT::renderDataTable({
          data <- eval(parse(text = df_graph$data[1]))
        },
        selection = 'single',
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          buttons = c('excel'),
          language = list(
            paginate = list(previous = 'Pr\u00e9c\u00e9dent', `next` = 'Suivant'),
            search = 'Recherche : '
          ),
          dom = 'Btp',
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = "_all")
          ),
          pageLength = 5)
        )
      }

  })}
