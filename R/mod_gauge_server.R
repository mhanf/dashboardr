#' gauge server part
#'
#' @param id module id
#' @param df A graph dashboarder dataframe
#' @param r r internal list (advanced use)
#' @param theme_var bslib theme variables
#' @param pattern default pattern to identify r code in dashboardr dataframe
#' @importFrom shiny moduleServer renderUI tagList div tags icon p h3
#' @importFrom plotly renderPlotly
#' @return an gauge server

mod_gauge_server <- function(id,
                             df,
                             r = NULL,
                             theme_var,
                             pattern = "^%r%") {
  shiny::moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns

    output$gauge <- plotly::renderPlotly({
      # parameter values
      val <- def_gauge_val_server(
        df = df,
        r = r,
        pattern = pattern
      )
      # theme
      theme_var <- extract_bslib_theme(session = session)
      # # create default indicator
      fig <- gauge_create(theme_var = theme_var)
      # # create indicator
      fig <- gauge_compute(
        fig = fig,
        indic_val = val,
        theme_var = theme_var
      )
      # figure restitution
      fig
    })
  })
}
