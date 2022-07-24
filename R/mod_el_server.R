#' dashboardr Server part
#'
#' @param id A module id
#' @param r  r internal list (advanced use)
#' @param df_graph A graph dashboarder dataframe
#' @param theme_var Shiny app theme list of interesting variables
#' @param default_pattern default pattern to identify r code in dashboardr dataframe
#'
#' @importFrom shiny moduleServer
#' @importFrom plotly renderPlotly add_markers
#' @return a dashboard

mod_el_server <- function(id, df_graph, r = NULL, theme_var, default_pattern = "^%r%") {
  shiny::moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns
    # graph compilation
    if (df_graph$type[1] == "plot") {
      output$plot <- plotly::renderPlotly({
        # data evaluation
        data <- eval(parse(text = df_graph$data[1]))
        # create default graph
        graph <- create_graph(theme_var = theme_var)
        # add traces to graph
        graph <- plotly::add_lines(
          graph,
          data = data,
          type = "scatter",
          x = data[, df_graph$x],
          y = data[, df_graph$y],
          line = list(color = theme_var$primary, width = 2)
        )
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
            paginate = list(previous = "<", `next` = ">"),
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
    } else if (df_graph$type[1] == "module") {
      if (!is.na(df_graph$mod_server)) {
        eval(parse(text = df_graph$mod_server))
      }
    }
    # info box experimental
    else if (df_graph$type[1] == "infobox") {
      mod_infobox_server(
        id = "infobox",
        df_graph = df_graph,
        r = r,
        default_pattern = default_pattern
      )
    } else if (df_graph$type[1] == "indicator") {
      output$indicator <- plotly::renderPlotly({
        # df associated indicator values
        indic_val <- def_indic_val(
          df_graph = df_graph,
          r = r,
          default_pattern = default_pattern
        )
        # create default indicator
        fig <- create_indicator(theme_var = theme_var)
        # create indicator
        fig <- compute_indicator(
          fig = fig,
          indic_val = indic_val,
          theme_var = theme_var
        )
        # figure restitution
        fig
      })
    }
  })
}
