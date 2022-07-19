#' dashboardr Server part
#'
#' @param id A module id
#' @param r  r internal list (advanced use)
#' @param df A dashboarder dataframe
#' @param default_pattern default pattern to identify r code in dashboardr dataframe
#'
#' @importFrom plotly renderPlotly plot_ly
#' @importFrom shiny moduleServer
#' @return a dashboard
#' @export

mod_dashboardr_server <- function(id, df, r = NULL, default_pattern = "^%r%") {
  shiny::moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns
    # test df
    test_df(
      df = df,
      check_var = c("id", "type", "data")
    )
    # normalize df
    df <- norm_df(df = df, r = r)
    # extract bslib theme
    theme_var <- extract_bslib_theme(session)
    # dashboard server
    lapply(unique(df$row), function(i) {
      # df row selection
      df_row <- df[df$row == i, ]
      # row compilation
      row <- lapply(unique(df_row$section), function(j) {
        # df section selection
        df_sect <- df_row[df_row$section == j, ]
        # lapply df_sect
        section <- lapply(unique(df_sect$id), function(k) {
          # df graph selection
          df_graph <- df_sect[df_sect$id == k, ]
          # graph server creation
          graph <- mod_el_server(
            id = sprintf("r_%s_%s_%s", i, j, k),
            df_graph = df_graph,
            r = r,
            theme_var = theme_var,
            default_pattern = default_pattern
          )
        })
      })
    })
  })
}
