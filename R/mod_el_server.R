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
        # Indicator compilation
        fig <- plotly::add_trace(
          fig,
          type = "indicator",
          mode = "number+delta+gauge",
          value = indic_val$indic_value,
          number = list(
            font = list(color = theme_var[[indic_val$indic_value_color]]),
            prefix = "",
            suffix = "$"
          ),
          # title = list(text = "Speed", font = list(size = 24)),
          delta = list(
            reference = 60,
            position = "bottom",
            relative = TRUE,
            valueformat = ".0%", # ".1f",
            increasing = list(color = theme_var$danger),
            decreasing = list(color = theme_var$success)
          ),
          gauge = list(
            axis = list(
              range = list(0, 130),
              tickfont = list(color = theme_var$`body-color`),
              tickprefix = "",
              ticksuffix = "$",
              tickcolor = theme_var$`body-color`
            ),
            bar = list(color = theme_var$primary),
            bgcolor = theme_var$light,
            # borderwidth = 2,
            bordercolor = theme_var$`body-color`,
            steps = list(
              list(range = c(0, 30), color = theme_var$success),
              list(range = c(30, 50), color = theme_var$warning),
              list(range = c(50, 70), color = theme_var$danger),
              list(range = c(70, 100), color = theme_var$dark),
              list(range = c(100, 130), color = theme_var$light)
            ),
            threshold = list(
              line = list(color = "red", width = 4),
              thickness = 0.75,
              value = 80
            )
          )
        )
        # fig <- fig |>
        #   layout(
        #     margin = list(l = 20, r = 35, t = 10, b = 5),
        #     paper_bgcolor = "rgba(0, 0, 0, 0)",
        #     plot_bgcolor = "rgba(0, 0, 0, 0)",
        #     modebar = list(
        #       bgcolor = "transparent",
        #       color = theme_var$`body-color`,
        #       activecolor = theme_var$primary
        #     ),
        #     font = list(color = theme_var$primary, family = theme_var$`font-family-base`))

        fig
      })
    }
  })
}
